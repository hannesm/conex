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


let keys p =
  let f = function
    | `File f when Strhelper.is_suffix ~suffix:".public" f ->
      Some (String.sub f 0 (String.length f - 7))
    | _ -> None
  in
  Utils.filter_map ~f (p.Provider.read_dir [ "keys" ])

let key_path id = [ "keys" ; id ^ ".public" ]


let delegates p =
  let f = function
    | `Dir d -> Some d
    | `File _ -> None
  in
  Utils.filter_map ~f (p.Provider.read_dir [ "data" ])

let delegate_path id = [ "data" ; id ; "delegate" ]


let items p id =
  let f = function
    | `Dir d -> Some d
    | `File _ -> None
  in
  Utils.filter_map ~f (p.Provider.read_dir [ "data" ; id ])

let checksum_path p =
  let d = delegate_of_item p in
  [ "data" ; d ; p ; "checksum" ]


let checksum_files p da =
  let de = delegate_of_item da in
  let st = [ "data" ; de ; da ] in
  let rec collect1 acc d = function
    | `File f when d = [] && f = "checksum" -> acc
    | `File f -> (d@[f]) :: acc
    | `Dir dir ->
      let sub = d @ [ dir ] in
      List.fold_left
        (fun acc x -> collect1 acc sub x)
        acc
        (p.Provider.read_dir (st@sub))
  in
  List.fold_left
    (fun acc x -> collect1 [] [] x @ acc)
    []
    (p.Provider.read_dir st)

let is_key = function
  | "keys" :: id :: [] when Strhelper.is_suffix ~suffix:".public" id -> Some (String.sub id 0 (String.length id - 7))
  | _ -> None

let is_delegate = function
  | "data" :: id :: "delegate" :: [] -> Some id
  | _ -> None

let is_item = function
  | "data" :: id :: id2 :: _ -> Some (id, id2)
  | _ -> None
