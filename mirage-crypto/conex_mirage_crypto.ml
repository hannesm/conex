open Conex_utils

module V = struct
  let good_rsa p = Mirage_crypto_pk.Rsa.pub_bits p >= 2048

  let encode_key pub =
    String.trim (Cstruct.to_string (X509.Public_key.encode_pem (`RSA pub)))

  let decode_key data =
    match X509.Public_key.decode_pem (Cstruct.of_string data) with
    | Error _ -> None
    | Ok (`RSA pub) -> Some pub
    | Ok _ -> None

  module Pss_sha256 = Mirage_crypto_pk.Rsa.PSS (Mirage_crypto.Hash.SHA256)

  let verify_rsa_pss ~key ~data ~signature id =
    let ( let* ) = Result.bind in
    let* signature =
      Result.map_error (fun _ -> `InvalidBase64Encoding id)
        (Base64.decode signature)
    in
    let signature = Cstruct.of_string signature in
    let cs_data = Cstruct.of_string data in
    let* key =
      Option.to_result ~none:(`InvalidPublicKey id) (decode_key key)
    in
    let* () = guard (good_rsa key) (`InvalidPublicKey id) in
    guard (Pss_sha256.verify ~key ~signature (`Message cs_data))
      (`InvalidSignature id)

  let to_h i =
    if i < 10 then
      char_of_int (0x30 + i)
    else
      char_of_int (0x57 + i)

  let to_hex cs =
    let l = Cstruct.length cs in
    let out = Bytes.create (2 * l) in
    for i = 0 to pred l do
      let b = Cstruct.get_uint8 cs i in
      let up = b lsr 4
      and low = b land 0x0F
      in
      Bytes.set out (i * 2) (to_h up);
      Bytes.set out (i * 2 + 1) (to_h low);
    done;
    Bytes.to_string out

  let sha256 data =
    let cs = Cstruct.of_string data in
    let check = Mirage_crypto.Hash.digest `SHA256 cs in
    to_hex check
end

module NC_V = Conex_verify.Make (V)

module C = struct

  type t =
    Conex_resource.identifier * Conex_resource.timestamp * Mirage_crypto_pk.Rsa.priv

  let created (_, ts, _) = ts

  let id (id, _, _) = id

  let decode_priv id ts data =
    match X509.Private_key.decode_pem (Cstruct.of_string data) with
    | Error (`Msg e) -> Error e
    | Ok (`RSA priv) -> Ok (id, ts, priv)
    | Ok _ -> Error "only RSA keys supported"

  let encode_priv priv =
    let pem = X509.Private_key.encode_pem (`RSA priv) in
    Cstruct.to_string pem

  let pub_of_priv_rsa_raw key =
    let pub = Mirage_crypto_pk.Rsa.pub_of_priv key in
    V.encode_key pub

  let generate_rsa ?(bits = 4096) () =
    let key = Mirage_crypto_pk.Rsa.generate ~bits () in
    encode_priv key, pub_of_priv_rsa_raw key

  let bits (_, _, k) = Mirage_crypto_pk.Rsa.priv_bits k

  let pub_of_priv_rsa (_, _, k) = pub_of_priv_rsa_raw k

  let sign_pss (_, _, key) data =
    let cs = Cstruct.of_string data in
    let signature = V.Pss_sha256.sign ~key (`Message cs) in
    Ok (Base64.encode_string (Cstruct.to_string signature))

  let sha256 = V.sha256
end
