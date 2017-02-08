(** Interface to crypto backend

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

module type VERIFY = sig
  (** [verify_rsa_pss ~key ~data ~signature] returns [Ok ()] on success,
      otherwise a [verification_error].  The digest used is SHA256 at the moment. *)
  val verify_rsa_pss : key:string -> data:string -> signature:string -> (unit, [> verification_error ]) result

  (** [b64sha356 str] computes the SHA256 digest of [str] and converts its
      binary output to base64. *)
  val b64sha256 : string -> string
end

(** {1 Signing} *)

module type SIGN_BACK = sig

  (** [pub_of_priv_rsa priv] is either [Ok pub] or [Error str].  The encoding is
      left to the provider, usual PKCS8 PEM encoding works fine. *)
  val pub_of_priv_rsa : string -> (string, string) result

  (** [generate_rsa ~bits ()] generates an RSA private key with [bits] size. *)
  val generate_rsa : ?bits:int -> unit -> string

  (** [sign_rsa_pss ~key data] is either [Ok signature] or [Error str]. *)
  val sign_rsa_pss : key:string -> string -> (string, string) result
end

module type SIGN = sig

  (** [generate ?bits created ()] generates a [bits] sized private key. *)
  val generate : ?bits:int -> Uint.t -> unit -> Key.priv

  (** [pub_of_priv priv] extracts the public key from the private. *)
  val pub_of_priv : Key.priv -> (Key.t, string) result

  (** [sign created author priv] uses {!Conex_resource.Author.prep_sig} to
      prepare [author] (incrementing counter, moving queued resource to signed).
      {!Conex_resource.Signature.wire} is used to produce the to-be-signed
      data.  This includes the author name, and the signature algorithm (at the
      moment RSA-PSS-SHA256). *)
  val sign : Uint.t -> Author.t -> Key.priv -> (Author.t, string) result
end

(** Instantiation. *)
module Make (C : SIGN_BACK) : SIGN
