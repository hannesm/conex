open Conex_result
open Conex_utils

type file_type = File | Directory

type path = string list

let path_to_string path =
  let skip x = List.mem x [ "." ; "" ; "/" ] in
  List.fold_left (fun d f ->
                  match d, f with
                  | "..", _ -> invalid_arg "there's no escape!"
                  | _, ".." -> invalid_arg "no escape for files!"
                  | d, f when skip d -> f
                  | d, f when skip f -> d
                  | d, f -> Filename.concat d f)
                 "" path

let string_to_path str = String.cuts '/' str

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

(*BISECT-IGNORE-BEGIN*)
let pp_provider ppf t =
  Format.fprintf ppf "repository %s: %s" t.name t.description
(*BISECT-IGNORE-END*)
