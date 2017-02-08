open Conex_utils
open Conex_resource

type verification_error = [
  | `InvalidBase64Encoding
  | `InvalidSignature
  | `InvalidPublicKey
  | `NoSignature
]

(*BISECT-IGNORE-BEGIN*)
let pp_verification_error ppf = function
  | `InvalidBase64Encoding -> Format.fprintf ppf "signature: no valid base64 encoding"
  | `InvalidSignature -> Format.fprintf ppf "signature: invalid"
  | `InvalidPublicKey -> Format.fprintf ppf "invalid public key"
  | `NoSignature -> Format.fprintf ppf "no signature found"
(*BISECT-IGNORE-END*)

module type VERIFY = sig
  val verify_rsa_pss : key:string -> data:string -> signature:string -> (unit, [> verification_error ]) result

  val b64sha256 : string -> string
end

module type SIGN_BACK = sig
  val pub_of_priv_rsa : string -> (string, string) result

  val generate_rsa : ?bits:int -> unit -> string

  val sign_rsa_pss : key:string -> string -> (string, string) result
end

module type SIGN = sig
  val generate : ?bits:int -> Uint.t -> unit -> Key.priv

  val pub_of_priv : Key.priv -> (Key.t, string) result

  val sign : Uint.t -> Author.t -> Key.priv -> (Author.t, string) result
end

module Make (C : SIGN_BACK) = struct
  let generate ?bits time () =
    let key = C.generate_rsa ?bits () in
    `Priv (`RSA, key, time)

  let pub_of_priv key = match key with
    | `Priv (`RSA, key, created) ->
      C.pub_of_priv_rsa key >>= fun pub ->
      Ok (`RSA, pub, created)

  let sign now idx priv =
    let idx, _overflow = Author.prep_sig idx in
    let data = Wire.to_string (Author.wire_raw idx)
    and id = idx.Author.name
    in
    let hdr = `RSA_PSS_SHA256, now in
    let data = Wire.to_string (Signature.wire id hdr data) in
    (match priv with
     | `Priv (`RSA, key, _) -> C.sign_rsa_pss ~key data) >>= fun signature ->
    Ok (Author.add_sig idx (hdr, signature))
end
