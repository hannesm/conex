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

type t = private {
  counter : int64 ;
  version : int64 ;
  name : name ;
  files : checksum_map ;
}

val pp_checksums : Format.formatter -> t -> unit

val checksums : ?counter:int64 -> ?version:int64 -> string -> c list -> t

val set_counter : t -> int64 -> t

val compare_checksums : t -> t ->
  (unit,
   [ `InvalidName of name * name
   | `ChecksumsDiff of name * name list * name list * (c * c) list ])
    result

val fold : (c -> 'b -> 'b) -> checksum_map -> 'b -> 'b
val find : checksum_map -> string -> c
