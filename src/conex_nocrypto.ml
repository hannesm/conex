open Conex_result
open Conex_resource
open Conex_utils

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

let verify (alg, pub, _) data sigval =
  match Nocrypto.Base64.decode (Cstruct.of_string sigval) with
  | None -> Error `InvalidBase64Encoding
  | Some signature ->
    let cs_data = Cstruct.of_string data in
    let x = match alg with `RSA -> pub in
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

let generate ?(bits = 4096) () =
  let key = RSA_priv (Nocrypto.Rsa.generate bits) in
  match Uint.of_float (Unix.time ()) with
  | None -> invalid_arg "couldn't convert time to uint"
  | Some c -> `Priv (`RSA, encode_priv key, c)

let pub_of_priv = function
  | `Priv (`RSA, k, created) ->
    match decode_priv k with
    | Some (RSA_priv k) ->
      let pub = Nocrypto.Rsa.pub_of_priv k in
      let pem = encode_key (RSA_pub pub) in
      Ok (`RSA, pem, created)
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
  | `Priv (`RSA, priv, _) -> match decode_priv priv with
    | Some priv ->
      let sigval = primitive_sign priv data in
      Ok sigval
    | None -> Error "couldn't decode private key"

let digest data =
  let cs = Cstruct.of_string data in
  let check = Nocrypto.Hash.digest `SHA256 cs in
  let b64 = Nocrypto.Base64.encode check in
  (`SHA256, Cstruct.to_string b64)

let id (alg, data, _) =
  (Key.alg_to_string alg) ^ snd (digest data)
