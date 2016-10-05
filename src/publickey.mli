open Core

type pub
val decode_key : string -> pub option
val encode_key : pub -> string
val pp_key : Format.formatter -> pub -> unit

type t = private {
  counter : int64 ;
  version : int64 ;
  keyid : identifier ;
  key : pub option ;
}

val pp_publickey : Format.formatter -> t -> unit

val publickey : ?counter:int64 -> ?version:int64 -> identifier -> pub option -> (t, string) result

val equal : t -> t -> bool

val verify : t -> string -> Signature.t -> (identifier, [> verification_error ]) result
