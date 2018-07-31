open Conex_utils
open Conex_resource
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

type r_err = [ `NotFound of typ * name | `ParseError of typ * name * string | `NameMismatch of typ * name * name ]

(*BISECT-IGNORE-BEGIN*)
let pp_r_err ppf = function
  | `NotFound (res, nam) -> Format.fprintf ppf "%a (type %a) was not found in repository" pp_name nam pp_typ res
  | `ParseError (res, n, e) -> Format.fprintf ppf "parse error while parsing %a (type %a): %s" pp_name n pp_typ res e
  | `NameMismatch (res, should, is) -> Format.fprintf ppf "%a (type %a) is named %a" pp_name should pp_typ res pp_name is
(*BISECT-IGNORE-END*)

let read_root t root_file =
  match t.read [ root_file ] with
  | Error _ -> Error (`NotFound (`Root, root_file))
  | Ok data ->
    match decode data >>= Root.of_wire with
    | Error p -> Error (`ParseError (`Root, root_file, p))
    | Ok (root, warn) ->
      guard (id_equal root.Root.name root_file)
        (`NameMismatch (`Root, root_file, root.Root.name)) >>= fun () ->
      Ok (root, warn)

let write_root t root =
  let id = root.Root.name in
  t.write [ id ] (encode (Root.wire root))

let targets t root =
  match t.read_dir root.Root.keydir with
  | Error e ->
    Printf.printf "failed while listing keys with %s\n" e ;
    []
  | Ok datas ->
    List.fold_left (fun acc -> function
        | File, name -> name :: acc
        | Directory, name ->
          Printf.printf "unexpected directory %s in keydir!" name ;
          acc)
      [] datas

let read_targets t root id =
  let path = root.Root.keydir @ [ id ] in
  match t.read path with
  | Error _ -> Error (`NotFound (`Targets, id))
  | Ok data ->
    match decode data >>= Targets.of_wire with
    | Error p -> Error (`ParseError (`Targets, id, p))
    | Ok (targets, warn) ->
      guard (id_equal targets.Targets.name id)
        (`NameMismatch (`Targets, id, targets.Targets.name)) >>= fun () ->
      Ok (targets, warn)

let write_targets t root targets =
  let path = root.Root.keydir @ [ targets.Targets.name ] in
  Printf.printf "writing %s\n" (path_to_string path) ;
  t.write path (encode (Targets.wire targets))

let digest_len f data =
  let digest = f data
  and size = Uint.of_int_exn (String.length data)
  in
  (digest, size)

let target f filename data =
  let digest, size = digest_len f data in
  { Target.digest = [ digest ] ; size ; filename }

let compute_checksum ?(prefix = [ "packages" ]) t f path =
  let rec compute_item prefix otherp acc = function
    | Directory, name ->
      let path = prefix @ [ name ] in
      t.read_dir path >>= fun items ->
      foldM (compute_item path (otherp @ [ name ])) acc items
    | File, name ->
      let filename = prefix @ [ name ] in
      t.read filename >>= fun data ->
      Ok (target f (otherp @ [ name ]) data :: acc)
  in
  let go pre name = compute_item (prefix @ pre) pre [] (Directory, name) in
  match List.rev path with
    | [] ->
      t.read_dir prefix >>= fun items ->
      foldM (fun acc e -> match e with
          | Directory, _ -> compute_item prefix [ ] acc e
          | File, _ -> Ok acc)
        [] items
    | [ name ] -> go [] name
    | name::rest -> go (List.rev rest) name


let compute_checksum_tree ?(prefix = [ "packages" ]) t f =
  let rec compute_item prefix otherp acc = function
    | Directory, name ->
      let path = prefix @ [ name ] in
      t.read_dir path >>= fun items ->
      foldM (compute_item path (otherp @ [ name ])) acc items
    | File, name ->
      let filename = prefix @ [ name ] in
      t.read filename >>= fun data ->
      let path = otherp @ [name] in
      let digestlen = digest_len f data in
      Ok (Tree.insert path digestlen acc)
  in
  t.read_dir prefix >>= fun items ->
  foldM (compute_item prefix []) Tree.empty items
