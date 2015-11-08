
val exists : string -> bool
val remove : string -> unit
val rename : string -> string -> unit
type file_type = [ `File | `Directory ]
val file_type : string -> file_type
val read_file : string -> string
val write_file : ?mode:int -> string -> string -> unit
val write_replace : ?mode:int -> string -> string -> unit
val collect_dir : string -> string list
