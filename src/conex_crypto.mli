open Conex_result

(** Crypto: signature for the external crypto providers

    Implementations are provided in {Conex_nocrypto} and {Conex_openssl}. *)

(** {1 Verification errors} *)

(** Potential error case when verifying a signature *)
type verification_error = [
  | `InvalidBase64Encoding
  | `InvalidSignature
  | `InvalidPublicKey
  | `NoSignature
]

(** [pp_verification_error pp] is a pretty printer for [verification_error]. *)
val pp_verification_error : Format.formatter -> verification_error -> unit

(** {1 Verification support} *)

module type VERIFY = sig
  (** [verify_rsa_pss ~key ~data ~signature] returns [Ok ()] on success,
      otherwise a [verification_error].  The digest used is SHA256 at the moment. *)
  val verify_rsa_pss : key:string -> data:string -> signature:string -> (unit, [> verification_error ]) result

  (** [b64sha356 str] computes the SHA256 digest of [str] and converts its
      binary output to base64. *)
  val b64sha256 : string -> string
end

(** {1 Signing support} *)

module type SIGN = sig

  (** [pub_of_priv_rsa priv] is either [Ok pub] or [Error str].  The encoding is
      left to the provider, usual PKCS8 PEM encoding works fine. *)
  val pub_of_priv_rsa : string -> (string, string) result

  (** [generate_rsa ~bits ()] generates an RSA private key with [bits] size. *)
  val generate_rsa : ?bits:int -> unit -> string

  (** [sign_rsa_pss ~key data] is either [Ok signature] or [Error str]. *)
  val sign_rsa_pss : key:string -> string -> (string, string) result
end
