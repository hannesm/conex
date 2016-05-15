open Core

let lowercase_equal names name =
  let name = Strhelper.lowercase_string name in
  (* check that name is good, here a case-insensitive comparison *)
  not (List.mem name (List.map Strhelper.lowercase_string names))

let unique_keyid = lowercase_equal
and unique_data = lowercase_equal

let delegate_of_item x =
  match Strhelper.cut '.' x with
  | Some (pre, _) -> pre
  | None -> assert false

let private_dir = Filename.concat (Sys.getenv "HOME") ".conex"

let private_keys p =
  let is_private s =
    if Strhelper.is_suffix ~suffix:".private" s then
      let p = string_to_path (p.Provider.name) in
      match List.rev (Strhelper.cuts '.' s) with
      | _::id::path when p = List.rev path -> Some id
      | _ -> None
    else
      None
  in
  List.fold_left
    (fun acc s ->
       Utils.option acc (fun s -> s :: acc) (is_private s))
    []
    (Persistency.collect_dir private_dir)

let private_key_path path id =
  let filename =
    let els = path @ [id ; "private"] in
    String.concat "." els
  in
  string_to_path private_dir @ [ filename ]

let key_dir, key_suffix = ("keys", ".public")

let keys p =
  match p.Provider.read_dir [ key_dir ] with
  | Error _ -> []
  | Ok data ->
    let suffix = key_suffix in
    let f = function
      | `File f when Strhelper.is_suffix ~suffix f ->
        Some (Strhelper.cut_suffix ~suffix f)
      | _ -> None
    in
    Utils.filter_map ~f data

let key_path id = [ key_dir ; id ^ key_suffix ]

let data_dir = "data"
let delegate_filename = "delegate"

let delegates p =
  match p.Provider.read_dir [ data_dir ] with
  | Error _ -> []
  | Ok data ->
    let f = function
      | `Dir d -> Some d
      | `File _ -> None
    in
    Utils.filter_map ~f data

let delegate_path id = [ data_dir ; id ; delegate_filename ]

let checksum_filename = "checksum"

let items p id =
  match p.Provider.read_dir [ data_dir ; id ] with
  | Error _ -> []
  | Ok data ->
    let f = function
      | `Dir d -> Some d
      | `File _ -> None
    in
    Utils.filter_map ~f data

let checksum_path p =
  let d = delegate_of_item p in
  [ data_dir ; d ; p ; checksum_filename ]

let checksum_files p da =
  let de = delegate_of_item da in
  let st = [ data_dir ; de ; da ] in
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

let is_key = function
  | kd :: id :: [] when
      kd = key_dir && Strhelper.is_suffix ~suffix:key_suffix id ->
    Some (String.sub id 0 (String.length id - 7))
  | _ -> None

let is_delegate = function
  | dd :: id :: dfn :: [] when dd = data_dir && dfn = delegate_filename->
    Some id
  | _ -> None

let is_item = function
  | dd :: id :: id2 :: _ when dd = data_dir -> Some (id, id2)
  | _ -> None
