open Core

type item = [
  | `File of string
  | `Dir of string
]

type t = {
  name : string ;
  description : string ;
  file_type : path -> Persistency.file_type ;
  read : path -> string option ;
  write : path -> string -> unit ;
  read_dir : path -> item list ;
  exists : path -> bool ;
}

val pp_provider : Format.formatter -> t -> unit

val fs_provider : string -> t
val fs_ro_provider : string -> t
