open Conex_utils
open Conex_core

type hunk = {
  mine_start : int ;
  mine_len : int ;
  mine : string list ;
  their_start : int ;
  their_len : int ;
  their : string list ;
}

let take data num =
  let rec take0 num data acc =
    match num, data with
    | 0, _ -> List.rev acc
    | n, x::xs -> take0 (pred n) xs (x :: acc)
    | _ -> invalid_arg "take0 broken"
  in
  take0 num data []

let rec drop data num =
  match num, data with
  | 0, _ -> data
  | n, _::xs -> drop xs (pred n)
  | _ -> invalid_arg "drop broken"

let apply_hunk data hunk =
  let prefix = take data hunk.mine_start
  and postfix = drop data (hunk.mine_start + hunk.mine_len)
  in
  prefix @ hunk.their @ postfix

let to_start_len data =
  (* input being "?19,23" *)
  match String.cut ',' (String.slice ~start:1 data) with
  | None when data = "+1" -> (0, 1)
  | None when data = "-1" -> (0, 1)
  | None -> invalid_arg ("start_len broken in " ^ data)
  | Some (start, len) ->
     let start = int_of_string start in
     let st = if start = 0 then start else pred start in
     (st, int_of_string len)

let count_to_sl_sl data =
  if String.is_prefix ~prefix:"@@" data then
    (* input: "@@ -19,23 +19,12 @@ bla" *)
    (* output: ((19,23), (19, 12)) *)
    match String.cuts '@' data with
    | numbers::_ ->
       let nums = String.trim numbers in
       (match String.cut ' ' nums with
        | None -> invalid_arg "couldn't find space in count"
        | Some (mine, theirs) -> Some (to_start_len mine, to_start_len theirs))
    | _ -> invalid_arg "broken line!"
  else
    None

let sort_into_bags mine their str =
  match String.get str 0, String.slice ~start:1 str with
  | ' ', data -> Some ((data :: mine), (data :: their))
  | '+', data -> Some (mine, (data :: their))
  | '-', data -> Some ((data :: mine), their)
  | '\\', _ -> Some (mine, their) (* usually: "\No newline at end of file" *)
  | _ -> None

let to_hunk count data =
  match count_to_sl_sl count with
  | None -> (None, count :: data)
  | Some ((mine_start, mine_len), (their_start, their_len)) ->
     let rec step mine their = function
       | [] -> (List.rev mine, List.rev their, [])
       | x::xs ->
          match sort_into_bags mine their x with
          | Some (mine, their) -> step mine their xs
          | None -> (List.rev mine, List.rev their, x :: xs)
     in
     let mine, their, rest = step [] [] data in
     (Some { mine_start ; mine_len ; mine ; their_start ; their_len ; their }, rest)

let rec to_hunks acc = function
  | [] -> (List.rev acc, [])
  | count::data ->
     match to_hunk count data with
     | None, rest -> (List.rev acc, rest)
     | Some hunk, rest -> to_hunks (hunk :: acc) rest

type diff = {
  mine_name : string ;
  their_name : string ;
  hunks : hunk list ;
}

let file diff =
  let name = match diff.mine_name, diff.their_name with
    | "/dev/null", a -> a
    | a, "/dev/null" -> a
    | a, _ -> a
  in
  if String.is_prefix ~prefix:"a/" name || String.is_prefix ~prefix:"b/" name then
    String.slice ~start:2 name
  else
    name

let to_diff data =
  (* first locate --- and +++ lines *)
  let cut4 = String.slice ~start:4 in
  let rec find_start = function
    | [] -> None
    | x::y::xs when String.is_prefix ~prefix:"---" x -> Some (cut4 x, cut4 y, xs)
    | _::xs -> find_start xs
  in
  match find_start data with
  | Some (mine_name, their_name, rest) ->
    let hunks, rest = to_hunks [] rest in
    Some ({ mine_name ; their_name ; hunks }, rest)
  | None -> None

let to_lines = String.cuts '\n'

let to_diffs data =
  let lines = to_lines data in
  let rec doit acc = function
    | [] -> List.rev acc
    | xs -> match to_diff xs with
      | None -> acc
      | Some (diff, rest) -> doit (diff :: acc) rest
  in
  doit [] lines

let patch filedata diff =
  let lines = match filedata with None -> [] | Some x -> to_lines x in
  let lines = List.fold_left apply_hunk lines diff.hunks in
  String.concat "\n" lines

let diffs_to_components diffs =
  List.fold_left (fun (ids, auths, rels, pkgs) diff ->
      match Conex_opam_repository_layout.categorise (string_to_path (file diff)) with
      | `Id id -> S.add id ids, auths, rels, pkgs
      | `Authorisation id -> ids, S.add id auths, rels, pkgs
      | `Releases id -> ids, auths, S.add id rels, pkgs
      | `Package (name, version) ->
        let s = try M.find name pkgs with Not_found -> S.empty in
        ids, auths, rels, M.add name (S.add version s) pkgs
      | _ -> ids, auths, rels, pkgs)
    (S.empty, S.empty, S.empty, M.empty) diffs

let apply provider diff =
  let read path =
    if file diff = path_to_string path then
      match provider.Conex_provider.read path with
      | Ok data -> Ok (patch (Some data) diff)
      | Error _ -> Ok (patch None diff)
    else
      provider.Conex_provider.read path
  and file_type path =
    let pn = path_to_string path
    and name = file diff
    in
    if pn = name then
      Ok File
    else if Conex_utils.String.is_prefix ~prefix:(pn ^ "/") name then
      Ok Directory
    else
      provider.Conex_provider.file_type path
  and read_dir path =
    let name = List.rev (string_to_path (file diff))
    and path = List.rev path
    in
    (* XXX: unlikely to be correct... *)
    let data = match name with
      | fn::xs when xs = path -> Some (`File fn)
      | _ -> None
    in
    match provider.Conex_provider.read_dir path, data with
      | Ok files, Some data -> Ok (data :: files)
      | Ok files, None -> Ok files
      | Error _, Some data -> Ok [data]
      | Error e, None -> Error e
  and write _ _ = Error "read only"
  and exists path =
    let pn = path_to_string path
    and name = file diff
    in
    if pn = name then
      true
    else
      provider.Conex_provider.exists path
  and name = provider.Conex_provider.name
  and description = "Patch provider"
  in
  { Conex_provider.name ; description ; file_type ; read ; write ; read_dir ; exists }
