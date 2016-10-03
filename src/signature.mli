open Core

(* a signature is a tuple *)
type t = identifier * string

val extend_data : string -> identifier -> string

val pp_signature : Format.formatter -> t -> unit
