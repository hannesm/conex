open Core

type t = private {
  counter : int64 ;
  version : int64 ;
  name : name ;
  releases : name list ; (* XXX: Set? *)
}

val releases : ?counter:int64 -> ?version:int64 -> ?releases:(name list) -> name -> t

val pp_releases : Format.formatter -> t -> unit
