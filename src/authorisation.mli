open Core

type t = private {
  counter : int64 ;
  version : int64 ;
  name : name ;
  authorised : identifier list ;
  signatures : Signature.t list ;
}

val authorisation : ?counter:int64 -> ?version:int64 -> ?authorised:(identifier list) -> ?signatures:(Signature.t list) -> name -> t

val add_sig : t -> Signature.t -> t

val pp_authorisation : Format.formatter -> t -> unit
val pp_authorised : Format.formatter -> Core.identifier list -> unit
