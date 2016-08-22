open Core

(* a signature is a tuple *)
type t = identifier * string

val extend_data : string -> identifier -> resource -> string

val pp_signature : Format.formatter -> t -> unit
val pp_signatures : Format.formatter -> t list -> unit
