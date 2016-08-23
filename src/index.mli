open Core

type t = private {
  counter : int64 ;
  version : int64 ;
  identifier : identifier ;
  resources : (name * resource * digest) list ;
  signature : Signature.t option ;
}

val index : ?counter:int64 -> ?version:int64 -> ?resources:((name * resource * digest) list) -> ?signature:Signature.t -> identifier -> t

val pp_resource : Format.formatter -> (name * resource * digest) -> unit

val pp_index : Format.formatter -> t -> unit

val add_sig : t -> Signature.t -> t
