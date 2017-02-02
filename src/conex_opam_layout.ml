open Conex_result
open Conex_core
open Conex_utils

let lowercase_equal names name =
  let name = String.to_lower name in
  (* check that name is good, here a case-insensitive comparison *)
  not (S.exists
         (fun n -> compare (String.to_lower n) name = 0)
         names)

let unique_id = lowercase_equal
and unique_data = lowercase_equal

let valid_id id = String.is_ascii id

let valid_name package =
  let p = function '-' | '_' -> true | _ -> false in
  String.is_ascii ~p package

let authorisation_of_item x =
  match String.cut '.' x with
  | Some (pre, _) -> Some pre
  | None -> None

let id_dir = "id"

let ids p =
  match p.Provider.read_dir [ id_dir ] with
  | Error _ -> []
  | Ok data ->
    let f = function
      | `File f -> Some f
      | _ -> None
    in
    filter_map ~f data

let id_path ji = [ id_dir ; ji ]

let data_dir = "packages"
let authorisation_filename = "authorisation"

let items p =
  match p.Provider.read_dir [ data_dir ] with
  | Error _ -> []
  | Ok data ->
    let f = function
      | `Dir d -> Some d
      | `File _ -> None
    in
    filter_map ~f data

let authorisation_path id = [ data_dir ; id ; authorisation_filename ]

let releases_filename = "releases"
let releases_path id = [ data_dir ; id ; releases_filename ]

let checksum_filename = "checksum"

let subitems p id =
  match p.Provider.read_dir [ data_dir ; id ] with
  | Error _ -> []
  | Ok data ->
    let f = function
      | `Dir d -> Some d
      | `File _ -> None
    in
    filter_map ~f data

let checksum_dir p =
  match authorisation_of_item p with
  | Some d -> [ data_dir ; d ; p ]
  | None -> [ data_dir ; p ; p ]

let checksum_path p =
  checksum_dir p @ [checksum_filename]

let checksum_files p da =
  let st = match authorisation_of_item da with
    | Some de -> [ data_dir ; de ; da ]
    | None -> [ data_dir ; da ; da ]
  in
  let rec collect1 acc d = function
    | `File f when d = [] && f = checksum_filename -> acc
    | `File f -> (d@[f]) :: acc
    | `Dir dir ->
      let sub = d @ [ dir ] in
      match p.Provider.read_dir (st@sub) with
      | Error _ -> []
      | Ok data ->
        List.fold_left
          (fun acc x -> collect1 acc sub x)
          acc
          data
  in
  match p.Provider.read_dir st with
  | Error _ -> []
  | Ok data ->
    List.fold_left
      (fun acc x -> collect1 [] [] x @ acc)
      []
      data

let categorise = function
  | idx ::id :: [] when idx = id_dir -> `Id id
  | dd :: id :: dfn :: [] when dd = data_dir && dfn = authorisation_filename -> `Authorisation id
  | dd :: id :: dfn :: [] when dd = data_dir && dfn = releases_filename -> `Releases id
  | dd :: id :: dfn :: _ when dd = data_dir ->
    (* current: packages/foo/foo.0.0.1 *)
    (match authorisation_of_item dfn with
     | Some x when String.compare_insensitive x id -> `Package (id, dfn)
     | _ ->
       (* earlier: packages/foo.0.0.1 *)
       match authorisation_of_item id with
       | Some x -> `Package (x, id)
       | _ -> `Unknown)
  | cc :: v :: vm :: _ when cc = "compilers" -> `Compiler (v, vm)
  | _ -> `Unknown
