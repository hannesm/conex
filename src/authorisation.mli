open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  authorised : identifier list ;
  signatures : Signature.t list ;
}

val pp_authorisation : Format.formatter -> t -> unit
val pp_authorised : Format.formatter -> Core.identifier list -> unit
