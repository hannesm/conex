open Core

type t = private {
  counter : int64 ;
  version : int64 ;
  name : name ;
  authorised : S.t ;
}

val authorisation : ?counter:int64 -> ?version:int64 -> ?authorised:S.t -> name -> t

val add : t -> identifier -> t
val remove : t -> identifier -> t

val pp_authorisation : Format.formatter -> t -> unit
val pp_authorised : Format.formatter -> S.t -> unit
