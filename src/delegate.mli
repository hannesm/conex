open Core

type t = {
  name : string ;
  counter : int64 ;
  validids : identifier list ;
  signatures : Signature.t list ;
}

val pp_delegate : Format.formatter -> t -> unit
val pp_owners : Format.formatter -> Core.identifier list -> unit
