open Core

type c = {
  filename : name ;
  bytesize : int64 ;
  checksum : digest ;
}

val pp_checksum : Format.formatter -> c -> unit
val checksum : string -> string -> c
val checksum_equal : c -> c -> bool

type checksum_map

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  files : checksum_map ;
  signatures : Signature.t list ;
}
val pp_checksums : Format.formatter -> t -> unit

val checksums : ?counter:int64 -> ?version:int64 -> ?signatures:(Signature.t list) -> string -> c list -> t

val checksums_equal : t -> t -> bool

val fold : (c -> 'b -> 'b) -> checksum_map -> 'b -> 'b
val find : checksum_map -> string -> c
