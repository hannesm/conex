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
