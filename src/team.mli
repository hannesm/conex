open Core

type t = private {
  counter : int64 ;
  version : int64 ;
  name : identifier ;
  members : S.t
}

val team : ?counter:int64 -> ?version:int64 -> ?members:S.t -> identifier -> t

val add : t -> identifier -> t
val remove : t -> identifier -> t

val pp_team : Format.formatter -> t -> unit
