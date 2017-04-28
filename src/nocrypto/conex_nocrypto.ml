open Conex_utils

module V = struct
  let good_rsa p = Nocrypto.Rsa.pub_bits p >= 2048

  let encode_key pub =
    String.trim (Cstruct.to_string (X509.Encoding.Pem.Public_key.to_pem_cstruct1 (`RSA pub)))

  let decode_key data =
    match X509.Encoding.Pem.Public_key.of_pem_cstruct (Cstruct.of_string data) with
    | [ `RSA pub ] -> Some pub
    | _ -> None

  module Pss_sha256 = Nocrypto.Rsa.PSS (Nocrypto.Hash.SHA256)

  let verify_rsa_pss ~key ~data ~signature =
    match Nocrypto.Base64.decode (Cstruct.of_string signature) with
    | None -> Error `InvalidBase64Encoding
    | Some signature ->
      let cs_data = Cstruct.of_string data in
      match decode_key key with
      | Some key when good_rsa key ->
        if Pss_sha256.verify ~key ~signature cs_data then
          Ok ()
        else
          Error `InvalidSignature
      | _ -> Error `InvalidPublicKey

  let to_h i =
    if i < 10 then
      char_of_int (0x30 + i)
    else
      char_of_int (0x57 + i)

  let to_hex cs =
    let l = Cstruct.len cs in
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
    let check = Nocrypto.Hash.digest `SHA256 cs in
    to_hex check
end

module NC_V = Conex_verify.Make (V)

module C (F : Conex_private.FS) = struct

  type t = Nocrypto.Rsa.priv * Uint.t

  let created (_, ts) = ts

  let ids = F.ids

  let decode_priv data =
    match X509.Encoding.Pem.Private_key.of_pem_cstruct (Cstruct.of_string data) with
    | [ `RSA priv ] -> Some priv
    | _ -> None

  let read id =
    F.read id >>= fun (k, ts) ->
    match decode_priv k with
    | Some priv -> Ok (priv, ts)
    | None -> Error "could not decode private key"

  let encode_priv priv =
    let pem = X509.Encoding.Pem.Private_key.to_pem_cstruct1 (`RSA priv) in
    Cstruct.to_string pem

  let generate_rsa ?(bits = 4096) id ts () =
    let key = Nocrypto.Rsa.generate bits in
    let data = encode_priv key in
    F.write id data >>= fun () ->
    Ok (key, ts)

  let bits (k, _) = Nocrypto.Rsa.priv_bits k

  let pub_of_priv_rsa (k, _) =
    let pub = Nocrypto.Rsa.pub_of_priv k in
    Ok (V.encode_key pub)

  let sign_pss (key, _) data =
    let cs = Cstruct.of_string data in
    let signature = V.Pss_sha256.sign ~key cs in
    let b64 = Nocrypto.Base64.encode signature in
    Ok (Cstruct.to_string b64)
end
