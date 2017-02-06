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

module type SIGN = sig
  include VERIFY

  val pub_of_priv_rsa : string -> (string, string) result

  val generate_rsa : ?bits:int -> unit -> string

  val sign_rsa_pss : key:string -> string -> (string, string) result
end
