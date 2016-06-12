open Core

type t = {
  name : name ;
  counter : int64 ;
  authorised : identifier list ;
  signatures : Signature.t list ;
}

val pp_authorisation : Format.formatter -> t -> unit
val pp_authorised : Format.formatter -> Core.identifier list -> unit
