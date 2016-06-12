open Core

type t = {
  name : name ;
  counter : int64 ;
  releases : name list ;
}

val pp_releases : Format.formatter -> t -> unit
