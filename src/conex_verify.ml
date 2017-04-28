open Conex_utils
open Conex_resource

type error = [
  | `InvalidBase64Encoding
  | `InvalidSignature
  | `InvalidPublicKey
]

(*BISECT-IGNORE-BEGIN*)
let pp_error ppf = function
  | `InvalidBase64Encoding -> Format.fprintf ppf "signature: no valid base64 encoding"
  | `InvalidSignature -> Format.fprintf ppf "signature: invalid"
  | `InvalidPublicKey -> Format.fprintf ppf "invalid public key"
(*BISECT-IGNORE-END*)

module type S_RSA_BACK = sig
  val verify_rsa_pss : key:string -> data:string -> signature:string -> (unit, [> error ]) result

  val sha256 : string -> string
end

module type S = sig
  val raw_digest : string -> Digest.t

  val digest : Wire.t -> Digest.t

  val keyid : identifier -> Key.t -> Digest.t

  val verify : Author.t -> (unit, [> error ]) result
end

(** Instantiation. *)
module Make (C : S_RSA_BACK) = struct

  let raw_digest data = `SHA256, C.sha256 data

  let digest data = raw_digest (Wire.to_string data)

  let keyid id k = digest (Key.wire id k)

  let verify_signature data name key (hdr, sigval) =
    let data = Signature.wire name hdr data in
    let key = match key with `RSA, k, _ -> k in
    C.verify_rsa_pss ~key ~data:(Wire.to_string data) ~signature:sigval

  let verify author =
    let tbv = Wire.to_string (Author.wire_raw author) in
    foldM
      (fun () (k, s) -> verify_signature tbv author.Author.name k s)
      () author.Author.keys
end
