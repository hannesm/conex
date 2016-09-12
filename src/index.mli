open Core

type t = private {
  counter : int64 ;
  version : int64 ;
  identifier : identifier ;
  resources : (name * resource * digest) list ;
  signature : Signature.t option ;
}

val index : ?counter:int64 -> ?version:int64 -> ?resources:((name * resource * digest) list) -> ?signature:Signature.t -> identifier -> t

val add_resource : t -> name * resource * digest -> t

val add_resources : t -> (name * resource * digest) list -> t

val pp_resource : Format.formatter -> (name * resource * digest) -> unit

val pp_index : Format.formatter -> t -> unit

val add_sig : t -> Signature.t -> t
