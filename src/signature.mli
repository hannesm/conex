open Core

(* a signature is a triple *)
type t = identifier * algorithm * string

val extend_data : string -> algorithm -> identifier -> string

val pp_signature : Format.formatter -> t -> unit
val pp_signatures : Format.formatter -> t list -> unit
