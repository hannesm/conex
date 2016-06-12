open Core

type t = {
  identifier : identifier ;
  counter : int64 ;
  resources : (name * kind * digest) list ;
  signatures : Signature.t list ;
}

val pp_resource : Format.formatter -> (name * kind * digest) -> unit

val pp_janitorindex : Format.formatter -> t -> unit
