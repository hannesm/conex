open Conex_utils
open Conex_resource
open Conex_opam_repository_layout
open Conex_opam_encoding

type t = {
  basedir : string ;
  description : string ;
  file_type : path -> (file_type, string) result ;
  read : path -> (string, string) result ;
  write : path -> string -> (unit, string) result ;
  read_dir : path -> (item list, string) result ;
  exists : path -> bool ;
}

(*BISECT-IGNORE-BEGIN*)
let pp ppf t =
  Format.fprintf ppf "repository %s: %s" t.basedir t.description
(*BISECT-IGNORE-END*)

type cc_err = [ `FileNotFound of name | `NotADirectory of name ]

(*BISECT-IGNORE-BEGIN*)
let pp_cc_err ppf = function
  | `FileNotFound n -> Format.fprintf ppf "couldn't find file %a" pp_name n
  | `NotADirectory n -> Format.fprintf ppf "expected %a to be a directory, but it is a file" pp_name n
 (*BISECT-IGNORE-END*)

let checksum_files t pv =
  (match authorisation_of_item pv with
   | Some de -> Ok (data_path@[ de ; pv ])
   | None -> Error (`FileNotFound pv )) >>= fun st ->
  let rec collect1 acc d = function
    | File, f when d = [] && f = release_filename -> acc
    | File, f -> (d@[f]) :: acc
    | Directory, dir ->
      let sub = d @ [ dir ] in
      match t.read_dir (st@sub) with
      | Error _ -> []
      | Ok data -> List.fold_left (fun acc x -> collect1 acc sub x) acc data
  in
  match t.read_dir st with
  | Error _ -> Error (`FileNotFound pv)
  | Ok data ->
    Ok (List.fold_left (fun acc x -> collect1 [] [] x @ acc) [] data)

let compute_release digest t now name =
  let checksum filename data =
    let size = Uint.of_int_exn (String.length data)
    and digest = digest data
    in
    { Release.filename ; size ; digest }
  in
  match t.file_type (release_dir name) with
  | Error _ -> Error (`FileNotFound name)
  | Ok File -> Error (`NotADirectory name)
  | Ok Directory ->
    checksum_files t name >>= fun fs ->
    let d = release_dir name in
    foldM (fun acc f ->
        match t.read (d@f) with
        | Error _ -> Error (`FileNotFound (path_to_string (d@f)))
        | Ok data -> Ok (data :: acc)) [] fs >>= fun ds ->
    let r = List.(map2 checksum (map path_to_string fs) (rev ds)) in
    Ok (Release.t now name r)

let read_dir f t path =
  t.read_dir path >>= fun data ->
  Ok (S.of_list (filter_map ~f data))

let ids t = read_dir (function File, f -> Some f | _ -> None) t id_path

let dirs = (function Directory, d -> Some d | _ -> None)

let items t = read_dir dirs t data_path
let subitems t name = read_dir dirs t (data_path@[name])

let compute_package t now name =
  subitems t name >>= fun releases ->
  Ok (Package.t ~releases now name)


type r_err = [ `NotFound of typ * name | `ParseError of typ * name * string | `NameMismatch of typ * name * name ]

(*BISECT-IGNORE-BEGIN*)
let pp_r_err ppf = function
  | `NotFound (res, nam) -> Format.fprintf ppf "%a %a was not found in repository" pp_typ res pp_name nam
  | `ParseError (res, n, e) -> Format.fprintf ppf "parse error while parsing %a %a: %s" pp_typ res pp_name n e
  | `NameMismatch (res, should, is) -> Format.fprintf ppf "%a %a is named %a" pp_typ res pp_name should pp_name is
(*BISECT-IGNORE-END*)

let read_team t name =
  match t.read (id_file name) with
  | Error _ -> Error (`NotFound (`Team, name))
  | Ok data ->
    match decode data >>= Team.of_wire with
    | Error p -> Error (`ParseError (`Team, name, p))
    | Ok team ->
      if id_equal team.Team.name name then
        Ok team
      else
        Error (`NameMismatch (`Team, name, team.Team.name))

let write_team t team =
  let id = team.Team.name in
  t.write (id_file id) (encode (Team.wire team))

let read_author t name =
  match t.read (id_file name) with
  | Error _ -> Error (`NotFound (`Author, name))
  | Ok data ->
    match decode data >>= Author.of_wire with
    | Error p -> Error (`ParseError (`Author, name, p))
    | Ok i ->
      if id_equal i.Author.name name then
        Ok i
      else
        Error (`NameMismatch (`Author, name, i.Author.name))

let write_author t i =
  let name = id_file i.Author.name in
  t.write name (encode (Author.wire i))

let read_id t id =
  match read_team t id with
  | Ok team -> Ok (`Team team)
  | Error _ -> match read_author t id with
    | Ok idx -> Ok (`Author idx)
    | Error e -> Error e

let read_authorisation t name =
  match t.read (authorisation_path name) with
  | Error _ -> Error (`NotFound (`Authorisation, name))
  | Ok data ->
    match decode data >>= Authorisation.of_wire with
    | Error p -> Error (`ParseError (`Authorisation, name, p))
    | Ok auth ->
      if name_equal auth.Authorisation.name name then
        Ok auth
      else
        Error (`NameMismatch (`Authorisation, name, auth.Authorisation.name))

let write_authorisation t a =
  t.write (authorisation_path a.Authorisation.name)
    (encode (Authorisation.wire a))

let read_package t name =
  match t.read (package_path name) with
  | Error _ -> Error (`NotFound (`Package, name))
  | Ok data ->
    match decode data >>= Package.of_wire with
    | Error p -> Error (`ParseError (`Package, name, p))
    | Ok r ->
      if name_equal r.Package.name name then
        Ok r
      else
        Error (`NameMismatch (`Package, name, r.Package.name))

let write_package t r =
  let name = package_path r.Package.name in
  t.write name (encode (Package.wire r))

let read_release t name =
  match t.read (release_path name) with
  | Error _ -> Error (`NotFound (`Release, name))
  | Ok data ->
    match decode data >>= Release.of_wire with
    | Error p -> Error (`ParseError (`Release, name, p))
    | Ok csum ->
      if name_equal csum.Release.name name then
        Ok csum
      else
        Error (`NameMismatch (`Release, name, csum.Release.name))

let write_release t csum =
  let name = release_path csum.Release.name in
  t.write name (encode (Release.wire csum))
