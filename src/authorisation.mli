open Core

type t = {
  name : string ;
  counter : int64 ;
  authorised : identifier list ;
  signatures : Signature.t list ;
}

val pp_authorisation : Format.formatter -> t -> unit
val pp_owners : Format.formatter -> Core.identifier list -> unit
