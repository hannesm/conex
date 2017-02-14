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

module type VERIFY_BACK = sig
  val verify_rsa_pss : key:string -> data:string -> signature:string -> (unit, [> verification_error ]) result

  val b64sha256 : string -> string
end

module type VERIFY = sig
  val raw_digest : string -> Digest.t

  val digest : Wire.t -> Digest.t

  val keyid : identifier -> Key.t -> Digest.t

  val verify : Author.t -> (unit, [> verification_error ]) result
end

(** Instantiation. *)
module Make_verify (C : VERIFY_BACK) = struct

  let raw_digest data = `SHA256, C.b64sha256 data

  let digest data =
    raw_digest (Wire.to_string data)

  let keyid id k = digest (Key.wire id k)

  let verify_signature data name key (hdr, sigval) =
    let data = Signature.wire name hdr data in
    let key = match key with `RSA, k, _ -> k in
    C.verify_rsa_pss ~key ~data:(Wire.to_string data) ~signature:sigval

  let verify author =
    let tbv = Wire.to_string (Author.wire_raw author) in
    match author.Author.keys with
    | [] -> Error `NoSignature
    | _ ->
      foldM
        (fun () (k, s) -> verify_signature tbv author.Author.name k s)
        () author.Author.keys
end

module type SIGN_BACK = sig
  val pub_of_priv_rsa : string -> (string, string) result

  val bits_rsa : string -> (int, string) result

  val generate_rsa : ?bits:int -> unit -> string

  val sign_rsa_pss : key:string -> string -> (string, string) result
end

module type SIGN = sig
  val generate : ?bits:int -> Uint.t -> unit -> Key.priv

  val pub_of_priv : Key.priv -> (Key.t, string) result

  val bits : Key.t -> (int, string) result

  val sign : Uint.t -> Author.t -> Key.priv -> (Author.t, string) result
end

module Make_sign (C : SIGN_BACK) = struct
  let generate ?bits time () =
    let key = C.generate_rsa ?bits () in
    `Priv (`RSA, key, time)

  let pub_of_priv key = match key with
    | `Priv (`RSA, key, created) ->
      C.pub_of_priv_rsa key >>= fun pub ->
      Ok (`RSA, pub, created)

  let bits = function
    | `RSA, key, _ -> C.bits_rsa key

  let sign now idx priv =
    let idx, _overflow = Author.prep_sig idx in
    let data = Wire.to_string (Author.wire_raw idx)
    and id = idx.Author.name
    in
    let hdr = `RSA_PSS_SHA256, now in
    let data = Wire.to_string (Signature.wire id hdr data) in
    (match priv with
     | `Priv (`RSA, key, _) -> C.sign_rsa_pss ~key data) >>= fun signature ->
    pub_of_priv priv >>= fun key ->
    Ok (Author.replace_sig idx (key, (hdr, signature)))
end
