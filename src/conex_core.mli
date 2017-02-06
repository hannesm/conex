open Conex_result
(** Conex core definitions *)


(** {1 File system types} *)

(** The sum type of possible file types we expect *)
type file_type = File | Directory

(** A [path] is a list of strings *)
type path = string list

(** [path_to_string path] is [String.concat "/" path]. *)
val path_to_string : path -> string

(** [string_to_path str] is [String.cut "/" str]. *)
val string_to_path : string -> path
