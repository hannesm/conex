module V = struct
  type nc_pub =
    | RSA_pub of Nocrypto.Rsa.pub

  let good_rsa p = Nocrypto.Rsa.pub_bits p >= 2048

  let encode_key = function
    | RSA_pub pub -> String.trim (Cstruct.to_string (X509.Encoding.Pem.Public_key.to_pem_cstruct1 (`RSA pub)))

  let decode_key data =
    match X509.Encoding.Pem.Public_key.of_pem_cstruct (Cstruct.of_string data) with
    | [ `RSA pub ] -> Some (RSA_pub pub)
    | _ -> None

  module Pss_sha256 = Nocrypto.Rsa.PSS (Nocrypto.Hash.SHA256)

  let verify_rsa_pss ~key ~data ~signature =
    match Nocrypto.Base64.decode (Cstruct.of_string signature) with
    | None -> Error `InvalidBase64Encoding
    | Some signature ->
      let cs_data = Cstruct.of_string data in
      match decode_key key with
      | Some (RSA_pub key) when good_rsa key ->
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

module C = struct
  type nc_priv =
    | RSA_priv of Nocrypto.Rsa.priv

  let decode_priv data =
    match X509.Encoding.Pem.Private_key.of_pem_cstruct (Cstruct.of_string data) with
    | [ `RSA priv ] -> Some (RSA_priv priv)
    | _ -> None

  let encode_priv = function
    | RSA_priv priv ->
      let pem = X509.Encoding.Pem.Private_key.to_pem_cstruct1 (`RSA priv) in
      Cstruct.to_string pem

  let generate_rsa ?(bits = 4096) () =
    let key = RSA_priv (Nocrypto.Rsa.generate bits) in
    encode_priv key

  let bits_rsa k =
    match V.decode_key k with
    | None -> Error "couldn't decode key"
    | Some (V.RSA_pub pub) -> Ok (Nocrypto.Rsa.pub_bits pub)

  let pub_of_priv_rsa k =
    match decode_priv k with
    | Some (RSA_priv k) ->
      let pub = Nocrypto.Rsa.pub_of_priv k in
      let pem = V.encode_key (V.RSA_pub pub) in
      Ok pem
    | None -> Error "couldn't decode private key"

  let primitive_sign priv data =
    let cs = Cstruct.of_string data in
    let signature =
      match priv with
      | RSA_priv key -> V.Pss_sha256.sign ~key cs
    in
    let b64 = Nocrypto.Base64.encode signature in
    Cstruct.to_string b64

  let sign_rsa_pss ~key data =
    match decode_priv key with
    | Some key -> Ok (primitive_sign key data)
    | None -> Error "couldn't decode private key"
end

module NC_S = Conex_crypto.Make_sign (C)
module NC_V = Conex_crypto.Make_verify (V)
