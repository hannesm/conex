open Core

type t = private {
  counter : int64 ;
  version : int64 ;
  name : name ;
  releases : name list ;
  signatures : Signature.t list ;
}

val releases : ?counter:int64 -> ?version:int64 -> ?releases:(name list) -> ?signatures:(Signature.t list) -> name -> t

val pp_releases : Format.formatter -> t -> unit
