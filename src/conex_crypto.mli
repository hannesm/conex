(** Crypto primitives

    Implementations are provided in {!Conex_nocrypto} and {!Conex_openssl}. *)

(** {1 Verification} *)

open Conex_utils
open Conex_resource

(** Potential error case when verifying a signature *)
type verification_error = [
  | `InvalidBase64Encoding
  | `InvalidSignature
  | `InvalidPublicKey
]

(** [pp_verification_error] is a pretty printer for [verification_error]. *)
val pp_verification_error : verification_error fmt

(** The verification module type *)
module type VERIFY = sig

  (** [raw_digest str] is the digest of the given [str]. *)
  val raw_digest : string -> Digest.t

  (** [digest wire] is the digest of the {{!Conex_resource.Wire.to_string}string
      encoding} of [wire]. *)
  val digest : Wire.t -> Digest.t

  (** [keyid id key] is the unique fingerprint of [key]. *)
  val keyid : identifier -> Key.t -> Digest.t

  (** [verify author] verifies all signatures of [author]: they have to sign
      the current resource list. *)
  val verify : Author.t -> (unit, [> verification_error ]) result
end

(** The verification backend, to be implemented by a crypto provider *)
module type VERIFY_BACK = sig

  (** [verify_rsa_pss ~key ~data ~signature] returns [Ok ()] on success,
      otherwise a [verification_error].  Currently, SHA256 is used as hash
      algorithm. *)
  val verify_rsa_pss : key:string -> data:string -> signature:string ->
    (unit, [> verification_error ]) result

  (** [sha356 str] computes the SHA256 digest of [str] and converts it to
      hex. *)
  val sha256 : string -> string
end

(** Instantiation. *)
module Make_verify (C : VERIFY_BACK) : VERIFY
