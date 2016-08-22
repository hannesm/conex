open Core

type t = private {
  counter : int64 ;
  version : int64 ;
  identifier : identifier ;
  resources : (name * resource * digest) list ;
  signatures : Signature.t list ;
}

val janitorindex : ?counter:int64 -> ?version:int64 -> ?resources:((name * resource * digest) list) -> ?signatures:(Signature.t list) -> identifier -> t

val pp_resource : Format.formatter -> (name * resource * digest) -> unit

val pp_janitorindex : Format.formatter -> t -> unit

val add_sig : t -> Signature.t -> t
