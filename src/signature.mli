open Core

(* a signature is a tuple *)
type t = identifier * int64 * string

val extend_data : string -> identifier -> int64 -> string

val pp_signature : Format.formatter -> t -> unit
