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
  | `NoSignature
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

  (** [keyid key] is the unique fingerprint of [key]. *)
  val keyid : Key.t -> Digest.t

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

  (** [b64sha356 str] computes the SHA256 digest of [str] and converts it to
      base64. *)
  val b64sha256 : string -> string
end

(** Instantiation. *)
module Make_verify (C : VERIFY_BACK) : VERIFY

(** {1 Signing} *)

(** The signing module type *)
module type SIGN = sig

  (** [generate ?bits created ()] generates a [bits] sized private key. *)
  val generate : ?bits:int -> Uint.t -> unit -> Key.priv

  (** [pub_of_priv priv] extracts the public key from the private. *)
  val pub_of_priv : Key.priv -> (Key.t, string) result

  (** [bits pub] returns the bit size of the public key. *)
  val bits : Key.t -> (int, string) result

  (** [sign created author priv] uses {!Conex_resource.Author.prep_sig} to
      prepare [author] (incrementing counter, moving queued resource to signed).
      {!Conex_resource.Signature.wire} is used to produce the to-be-signed
      data.  This includes the author name, and the signature algorithm (at the
      moment RSA-PSS-SHA256). *)
  val sign : Uint.t -> Author.t -> Key.priv -> (Author.t, string) result
end

(** The signing backend, to be implemented by a crypto provider. *)
module type SIGN_BACK = sig

  (** [pub_of_priv_rsa priv] is either [Ok pub] or [Error str].  The encoding is
      left to the provider, usual PKCS8 PEM encoding works fine. *)
  val pub_of_priv_rsa : string -> (string, string) result

  (** [bits_rsa pub] is the number of bits in [pub]. *)
  val bits_rsa : string -> (int, string) result

  (** [generate_rsa ~bits ()] generates an RSA private key with [bits] size. *)
  val generate_rsa : ?bits:int -> unit -> string

  (** [sign_rsa_pss ~key data] is either [Ok signature] or [Error str]. *)
  val sign_rsa_pss : key:string -> string -> (string, string) result
end

(** Instantiation. *)
module Make_sign (C : SIGN_BACK) : SIGN
