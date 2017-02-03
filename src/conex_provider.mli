open Conex_result
open Conex_core

type item = [
  | `File of string
  | `Dir of string
]

type t = {
  name : string ;
  description : string ;
  file_type : path -> (file_type, string) result ;
  read : path -> (string, string) result ;
  write : path -> string -> (unit, string) result ;
  read_dir : path -> (item list, string) result ;
  exists : path -> bool ;
}

val pp_provider : Format.formatter -> t -> unit

val fs_provider : string -> (t, string) result
val fs_ro_provider : string -> (t, string) result
