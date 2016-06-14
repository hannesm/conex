open Core

type pub
val decode_key : string -> pub
val encode_key : pub -> string
val pp_key : Format.formatter -> pub -> unit

type t = private {
  counter : int64 ;
  version : int64 ;
  keyid : identifier ;
  key : pub option ;
  role : role ;
  signatures : Signature.t list
}

val pp_publickey : Format.formatter -> t -> unit

val publickey : ?counter:int64 -> ?version:int64 -> ?role:role -> ?signatures:(Signature.t list) -> identifier -> pub option -> (t, string) result

val add_sig : t -> Signature.t -> t

val equal : t -> t -> bool

val verify : t -> role -> kind -> string -> Signature.t -> (identifier, error) result
