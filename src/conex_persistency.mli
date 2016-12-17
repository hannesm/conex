open Conex_core

val exists : string -> bool
val mkdir : ?mode:int -> string -> unit
val remove : string -> unit
val rename : string -> string -> unit
val file_type : string -> file_type option
val read_file : string -> string
val write_file : ?mode:int -> string -> string -> unit
val write_replace : ?mode:int -> string -> string -> unit
val collect_dir : string -> string list
