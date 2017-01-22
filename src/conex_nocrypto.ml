open Conex_result

type nc_pub =
  | RSA_pub of Nocrypto.Rsa.pub

let good_rsa p = Nocrypto.Rsa.pub_bits p >= 2048

let encode_key = function
  | RSA_pub pub -> Cstruct.to_string (X509.Encoding.Pem.Public_key.to_pem_cstruct1 (`RSA pub))

let decode_key data =
  match X509.Encoding.Pem.Public_key.of_pem_cstruct (Cstruct.of_string data) with
  | [ `RSA pub ] ->
    let enc = encode_key (RSA_pub pub) in
    if enc = data then
      Some (RSA_pub pub)
    else
      None
  | _ -> None

module Pss_sha256 = Nocrypto.Rsa.PSS (Nocrypto.Hash.SHA256)

let verify pub data sigval =
  match Nocrypto.Base64.decode (Cstruct.of_string sigval) with
  | None -> Error `InvalidBase64Encoding
  | Some signature ->
    let cs_data = Cstruct.of_string data in
    let x = match pub with `RSA_pub x -> x in
    match decode_key x with
    | Some (RSA_pub key) when good_rsa key ->
      if Pss_sha256.verify ~key ~signature cs_data then
        Ok ()
      else
        Error `InvalidSignature
    | _ -> Error `InvalidPublicKey


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

let generate ?(bits = 2048) () =
  `RSA_priv (encode_priv (RSA_priv (Nocrypto.Rsa.generate bits)))

let pub_of_priv = function
  | `RSA_priv k ->
    match decode_priv k with
    | Some (RSA_priv k) ->
      let pub = Nocrypto.Rsa.pub_of_priv k in
      let pem = encode_key (RSA_pub pub) in
      Ok (`RSA_pub pem)
    | None -> Error "couldn't decode private key"

let primitive_sign priv data =
  let cs = Cstruct.of_string data in
  let signature =
    match priv with
    | RSA_priv key -> Pss_sha256.sign ~key cs
  in
  let b64 = Nocrypto.Base64.encode signature in
  Cstruct.to_string b64

let sign priv data = match priv with
  | `RSA_priv priv -> match decode_priv priv with
    | Some priv ->
      let sigval = primitive_sign priv data in
      Ok sigval
    | None -> Error "couldn't decode private key"

let digest data =
  let cs = Cstruct.of_string data in
  let check = Nocrypto.Hash.digest `SHA256 cs in
  let b64 = Nocrypto.Base64.encode check in
  Cstruct.to_string b64

let id = function
  | `RSA_pub rsa -> "RSA:" ^ digest rsa
