open Conex_utils

let valid_id id = String.is_ascii id

let valid_name package =
  let p = function '-' | '_' -> true | _ -> false in
  String.is_ascii ~p package

let authorisation_of_package x =
  match String.cut '.' x with
  | Some (pre, _) -> Some pre
  | None -> None

let id_dir = "id"
let id_path = [ id_dir ]
let id_file id = [ id_dir ; id ]

let data_dir = "packages"
let data_path = [ data_dir ]

let authorisation_filename = "authorisation"

let authorisation_path id = [ data_dir ; id ; authorisation_filename ]

let package_filename = "package"
let package_path id = [ data_dir ; id ; package_filename ]

let release_filename = "release"

let release_dir p =
  match authorisation_of_package p with
  | Some d -> [ data_dir ; d ; p ]
  | None -> [ data_dir ; p ; p ]

let release_path p =
  release_dir p @ [release_filename]

let categorise = function
  | idx ::id :: [] when idx = id_dir -> `Id id
  | dd :: id :: dfn :: [] when dd = data_dir && dfn = authorisation_filename -> `Authorisation id
  | dd :: id :: dfn :: [] when dd = data_dir && dfn = package_filename -> `Package id
  | dd :: id :: dfn :: _ when dd = data_dir ->
    (* current: packages/foo/foo.0.0.1 *)
    (match authorisation_of_package dfn with
     | Some x when String.compare_insensitive x id -> `Release (id, dfn)
     | _ ->
       (* earlier: packages/foo.0.0.1 *)
       match authorisation_of_package id with
       | Some x -> `Release (x, id)
       | _ -> `Unknown)
  | cc :: v :: vm :: _ when cc = "compilers" -> `Compiler (v, vm)
  | _ -> `Unknown
