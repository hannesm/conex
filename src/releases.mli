open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  releases : name list ;
}

val pp_releases : Format.formatter -> t -> unit
