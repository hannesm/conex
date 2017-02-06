open Conex_result
open Conex_provider

val exists : string -> bool
val mkdir : ?mode:int -> string -> (unit, string) result
val remove : string -> (unit, string) result
val rename : string -> string -> (unit, string) result
val file_type : string -> (file_type, string) result
val read_file : string -> (string, string) result
val write_file : ?mode:int -> string -> string -> (unit, string) result
val write_replace : ?mode:int -> string -> string -> (unit, string) result
val collect_dir : string -> (string list, string) result
