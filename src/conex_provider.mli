open Conex_result
open Conex_utils

(** {1 File system types} *)

(** The sum type of possible file types we expect *)
type file_type = File | Directory

(** A [path] is a list of strings *)
type path = string list

(** [path_to_string path] is [String.concat "/" path]. *)
val path_to_string : path -> string

(** [string_to_path str] is [String.cut "/" str]. *)
val string_to_path : string -> path

(** An [item] is a type and its payload *)
type item = file_type * string

(** A provider contains its base directory, a description, and read/write/exist
    functionality.  TODO: define a module type. *)
type t = {
  basedir : string ;
  description : string ;
  file_type : path -> (file_type, string) result ;
  read : path -> (string, string) result ;
  write : path -> string -> (unit, string) result ;
  read_dir : path -> (item list, string) result ;
  exists : path -> bool ;
}

(** [pp t] is a pretty printer for [t]. *)
val pp : t fmt
