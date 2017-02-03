open Conex_result
open Conex_utils
open Conex_core
open Conex_resource
open Conex_provider
open Conex_opam_layout

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
    | `File f when d = [] && f = checksum_filename -> acc
    | `File f -> (d@[f]) :: acc
    | `Dir dir ->
      let sub = d @ [ dir ] in
      match t.read_dir (st@sub) with
      | Error _ -> []
      | Ok data -> List.fold_left (fun acc x -> collect1 acc sub x) acc data
  in
  match t.read_dir st with
  | Error _ -> Error (`FileNotFound pv)
  | Ok data ->
    Ok (List.fold_left (fun acc x -> collect1 [] [] x @ acc) [] data)

let compute_checksum t name =
  match t.file_type (checksum_dir name) with
  | Error _ -> Error (`FileNotFound name)
  | Ok File -> Error (`NotADirectory name)
  | Ok Directory ->
    checksum_files t name >>= fun fs ->
    let d = checksum_dir name in
    foldM (fun acc f ->
        match t.read (d@f) with
        | Error _ -> Error (`FileNotFound (path_to_string (d@f)))
        | Ok data -> Ok (data :: acc)) [] fs >>= fun ds ->
    let r = List.(map2 Checksum.checksum (map path_to_string fs) (rev ds)) in
    Ok (Checksum.checksums name r)

let read_dir f t path =
  let xs = match t.read_dir path with
    | Error _ -> []
    | Ok data -> filter_map ~f data
  in
  S.of_list xs

let ids t = read_dir (function `File f -> Some f | _ -> None) t id_path

let dirs = (function `Dir d -> Some d | _ -> None)

let items t = read_dir dirs t data_path
let subitems t name = read_dir dirs t (data_path@[name])

let compute_releases t name = Releases.releases ~releases:(subitems t name) name


type r_err = [ `NotFound of string * string | `ParseError of name * string | `NameMismatch of string * string ]

(*BISECT-IGNORE-BEGIN*)
let pp_r_err ppf = function
  | `NotFound (x, y) -> Format.fprintf ppf "%s %s was not found in repository" x y
  | `ParseError (n, e) -> Format.fprintf ppf "parse error while parsing %a: %s" pp_name n e
  | `NameMismatch (should, is) -> Format.fprintf ppf "%s is named %s" should is
(*BISECT-IGNORE-END*)

let read_team t name =
  match t.read (id_file name) with
  | Error _ -> Error (`NotFound ("team", name))
  | Ok data ->
    match Conex_data.decode data >>= Team.of_wire with
    | Error p -> Error (`ParseError (name, p))
    | Ok team ->
      if id_equal team.Team.name name then
        Ok team
      else
        Error (`NameMismatch (name, team.Team.name))

let write_team t team =
  let id = team.Team.name in
  t.write (id_file id) (Conex_data.encode (Team.wire team))

let read_index t name =
  match t.read (id_file name) with
  | Error _ -> Error (`NotFound ("index", name))
  | Ok data ->
    match Conex_data.decode data >>= Index.of_wire with
    | Error p -> Error (`ParseError (name, p))
    | Ok i ->
      if id_equal i.Index.name name then
        Ok i
      else
        Error (`NameMismatch (name, i.Index.name))

let write_index t i =
  let name = id_file i.Index.name in
  t.write name (Conex_data.encode (Index.wire i))

let read_id t id =
  match read_team t id with
  | Ok team -> Ok (`Team team)
  | Error (`NameMismatch (a, b)) -> Error (`NameMismatch (a, b))
  | Error _ -> match read_index t id with
    | Ok idx -> Ok (`Id idx)
    | Error e -> Error e

let read_authorisation t name =
  match t.read (authorisation_path name) with
  | Error _ -> Error (`NotFound ("authorisation", name))
  | Ok data ->
    match Conex_data.decode data >>= Authorisation.of_wire with
    | Error p -> Error (`ParseError (name, p))
    | Ok auth ->
      if name_equal auth.Authorisation.name name then
        Ok auth
      else
        Error (`NameMismatch (name, auth.Authorisation.name))

let write_authorisation t a =
  t.write (authorisation_path a.Authorisation.name)
    (Conex_data.encode (Authorisation.wire a))

let read_releases t name =
  match t.read (releases_path name) with
  | Error _ -> Error (`NotFound ("releases", name))
  | Ok data ->
    match Conex_data.decode data >>= Releases.of_wire with
    | Error p -> Error (`ParseError (name, p))
    | Ok r ->
      if name_equal r.Releases.name name then
        Ok r
      else
        Error (`NameMismatch (name, r.Releases.name))

let write_releases t r =
  let name = releases_path r.Releases.name in
  t.write name (Conex_data.encode (Releases.wire r))

let read_checksum t name =
  match t.read (checksum_path name) with
  | Error _ -> Error (`NotFound ("checksum", name))
  | Ok data ->
    match Conex_data.decode data >>= Checksum.of_wire with
    | Error p -> Error (`ParseError (name, p))
    | Ok csum ->
      if name_equal csum.Checksum.name name then
        Ok csum
      else
        Error (`NameMismatch (name, csum.Checksum.name))

let write_checksum t csum =
  let name = checksum_path csum.Checksum.name in
  t.write name (Conex_data.encode (Checksum.wire csum))

type m_err = [ r_err | `NotIncreased of resource * name | `Deleted of resource * name | `Msg of string ]

let pp_m_err ppf =
  let s = resource_to_string in
  function
  | #r_err as e -> pp_r_err ppf e
  | `NotIncreased (res, nam) -> Format.fprintf ppf "monotonicity: counter of %s %s was not increased" (s res) nam
  | `Deleted (res, nam) -> Format.fprintf ppf "monotonicity: %s %s was deleted" (s res) nam
  | `Msg s -> Format.fprintf ppf "monotonicity: %s" s

let monotonicity t t' resource name =
  let e = (resource, name) in
  let incr c c' =
    guard (Uint.compare c' c = 1) (`NotIncreased e)
  in
  match resource with
  | `Index ->
    begin match read_id t name, read_id t' name with
      | Ok (`Id idx), Ok (`Id idx') -> incr idx'.Index.counter idx.Index.counter
      | Ok (`Team team), Ok (`Team team') -> incr team'.Team.counter team.Team.counter
      | Error _, Ok _ -> Ok () (* allow creation (could check for valid + unique id) *)
      | Ok _, Error _ -> Error (`Deleted e) (* DO NOT allow index deletions *)
      | Error e, Error _ -> Error e
      | Ok (`Id _), Ok (`Team _) -> Error (`Msg ("id " ^ name ^ " is now a team"))
      | Ok (`Team _), Ok (`Id _) -> Error (`Msg ("team " ^ name ^ " is now an id"))
    end
  | `Checksums ->
    begin match read_checksum t name, read_checksum t' name with
     | Ok cs, Ok cs' -> incr cs'.Checksum.counter cs.Checksum.counter
     | Error _, Ok _ -> Ok () (* allow creation *)
     | Ok _, Error _ -> Ok () (* allow deletion of checksums *)
     | Error e, Error _ -> Error e
    end
  | `Releases ->
    begin match read_releases t name, read_releases t' name with
     | Ok rel, Ok rel' -> incr rel'.Releases.counter rel.Releases.counter
     | Error _, Ok _ -> Ok () (* allow creation *)
     | Ok _, Error _ -> Error (`Deleted e) (* DO NOT allow deletion of releases *)
     | Error e, Error _ -> Error e
    end
  | `Authorisation ->
    begin match read_authorisation t name, read_authorisation t' name with
     | Ok auth, Ok auth' -> incr auth'.Authorisation.counter auth.Authorisation.counter
     | Error _, Ok _ -> Ok () (* allow creation *)
     | Ok _, Error _ -> Error (`Deleted e) (* DO NOT allow deletion of authorsations *)
     | Error e, Error _ -> Error e
      end
  | `PublicKey | `Team -> Error (`Msg "not sure what you wanted to do")
