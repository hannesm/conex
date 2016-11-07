open Conex_result
open Conex_core

type item = [
  | `File of string
  | `Dir of string
]

type err = [ `NotFound | `UnknownFileType of string ]

type t = {
  name : string ;
  description : string ;
  file_type : path -> (file_type, err) result ;
  read : path -> (string, err) result ;
  write : path -> string -> unit ;
  read_dir : path -> (item list, err) result ;
  exists : path -> bool ;
}

val pp_provider : Format.formatter -> t -> unit

val fs_provider : string -> t
val fs_ro_provider : string -> t
