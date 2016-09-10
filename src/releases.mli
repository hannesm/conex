open Core

type t = private {
  counter : int64 ;
  version : int64 ;
  name : name ;
  releases : S.t ;
}

val releases : ?counter:int64 -> ?version:int64 -> ?releases:S.t -> name -> t

val pp_releases : Format.formatter -> t -> unit
