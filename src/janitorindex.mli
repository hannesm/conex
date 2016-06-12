open Core

type t = {
  counter : int64 ;
  version : int64 ;
  identifier : identifier ;
  resources : (name * kind * digest) list ;
  signatures : Signature.t list ;
}

val pp_resource : Format.formatter -> (name * kind * digest) -> unit

val pp_janitorindex : Format.formatter -> t -> unit
