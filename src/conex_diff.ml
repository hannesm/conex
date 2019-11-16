open Conex_utils

type hunk = {
  mine_start : int ;
  mine_len : int ;
  mine : string list ;
  their_start : int ;
  their_len : int ;
  their : string list ;
}

let unified_diff hunk =
  (* TODO *)
  String.concat "\n" (List.map (fun line -> "-" ^ line) hunk.mine @
                      List.map (fun line -> "+" ^ line) hunk.their)

let pp_hunk ppf hunk =
  Format.fprintf ppf "@@@@ -%d,%d +%d,%d @@@@\n%s"
    hunk.mine_start hunk.mine_len hunk.their_start hunk.their_len
    (unified_diff hunk)

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

(* TODO verify that it applies cleanly *)
let apply_hunk old (index, to_build) hunk =
  let prefix = take (drop old index) (hunk.mine_start - index) in
  (hunk.mine_start + hunk.mine_len, to_build @ prefix @ hunk.their)

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
    match List.filter (function "" -> false | _ -> true) (String.cuts '@' data) with
    | numbers::_ ->
       let nums = String.trim numbers in
       (match String.cut ' ' nums with
        | None -> invalid_arg "couldn't find space in count"
        | Some (mine, theirs) -> Some (to_start_len mine, to_start_len theirs))
    | _ -> invalid_arg "broken line!"
  else
    None

let sort_into_bags dir mine their m_nl t_nl str =
  if String.length str = 0 then
    None
  else if String.is_prefix ~prefix:"---" str then
    None
  else match String.get str 0, String.slice ~start:1 str with
    | ' ', data -> Some (`Both, (data :: mine), (data :: their), m_nl, t_nl)
    | '+', data -> Some (`Their, mine, (data :: their), m_nl, t_nl)
    | '-', data -> Some (`Mine, (data :: mine), their, m_nl, t_nl)
    | '\\', data ->
      (* diff: 'No newline at end of file' turns out to be context-sensitive *)
      (* so: -xxx\n\\No newline... means mine didn't have a newline *)
      (* but +xxx\n\\No newline... means theirs doesn't have a newline *)
      assert (data = " No newline at end of file");
      let my_nl, their_nl = match dir with
        | `Both -> true, true
        | `Mine -> true, t_nl
        | `Their -> m_nl, true
      in
      Some (dir, mine, their, my_nl, their_nl)
    | _ -> None

let to_hunk count data mine_no_nl their_no_nl =
  match count_to_sl_sl count with
  | None -> None, mine_no_nl, their_no_nl, count :: data
  | Some ((mine_start, mine_len), (their_start, their_len)) ->
    let rec step dir mine their mine_no_nl their_no_nl = function
      | [] -> (List.rev mine, List.rev their, mine_no_nl, their_no_nl, [])
      | x::xs -> match sort_into_bags dir mine their mine_no_nl their_no_nl x with
        | Some (dir, mine, their, mine_no_nl', their_no_nl') -> step dir mine their mine_no_nl' their_no_nl' xs
        | None -> (List.rev mine, List.rev their, mine_no_nl, their_no_nl, x :: xs)
    in
    let mine, their, mine_no_nl, their_no_nl, rest = step `Both [] [] mine_no_nl their_no_nl data in
    (Some { mine_start ; mine_len ; mine ; their_start ; their_len ; their }, mine_no_nl, their_no_nl, rest)

let rec to_hunks (mine_no_nl, their_no_nl, acc) = function
  | [] -> (List.rev acc, mine_no_nl, their_no_nl, [])
  | count::data -> match to_hunk count data mine_no_nl their_no_nl with
    | None, mine_no_nl, their_no_nl, rest -> List.rev acc, mine_no_nl, their_no_nl, rest
    | Some hunk, mine_no_nl, their_no_nl, rest -> to_hunks (mine_no_nl, their_no_nl, hunk :: acc) rest

type t = {
  mine_name : string option ;
  their_name : string option ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}

let pp ppf t =
  Format.fprintf ppf "--- b/%s\n"
    (match t.mine_name with None -> "/dev/null" | Some x -> x) ;
  Format.fprintf ppf "+++ a/%s\n"
    (match t.their_name with None -> "/dev/null" | Some x -> x) ;
  List.iter (pp_hunk ppf) t.hunks

let filename name =
  let str = match String.cut ' ' name with None -> name | Some (x, _) -> x in
  let r =
    if String.is_prefix ~prefix:"a/" str || String.is_prefix ~prefix:"b/" str then
      String.slice ~start:2 str
    else
      str
  in
  if r = "/dev/null" then None else Some r

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
    let mine_name, their_name = filename mine_name, filename their_name in
    let hunks, mine_no_nl, their_no_nl, rest = to_hunks (false, false, []) rest in
    Some ({ mine_name ; their_name ; hunks ; mine_no_nl ; their_no_nl }, rest)
  | None -> None

let to_lines = String.cuts '\n'

let to_diffs data =
  let lines = to_lines data in
  let rec doit acc = function
    | [] -> List.rev acc
    | xs ->
      (* TODO is this a good idea here to drop potential errors? *)
      match to_diff xs with
      | None -> acc
      | Some (diff, rest) -> doit (diff :: acc) rest
  in
  doit [] lines

let patch filedata diff =
  let old = match filedata with None -> [] | Some x -> to_lines x in
  let idx, lines = List.fold_left (apply_hunk old) (0, []) diff.hunks in
  let lines = lines @ drop old idx in
  let lines =
    match diff.mine_no_nl, diff.their_no_nl with
    | false, true -> (match List.rev lines with ""::tl -> List.rev tl | _ -> lines)
    | true, false -> lines @ [ "" ]
    | false, false -> lines
    | true, true -> lines
  in
  String.concat "\n" lines

(* TODO which equality to use here? is = ok? *)
let ids root keydir diffs =
  List.fold_left (fun acc diff ->
      let add_id filename (r, ids) =
        match filename with
        | None -> Ok (r, ids)
        | Some name ->
          string_to_path name >>= fun path ->
          if subpath ~parent:keydir path then
            (* TODO according to here, keydir must be flat! *)
            match List.rev path with
            | id :: _ -> Ok (r, S.add id ids)
            | [] -> Error "empty keydir path?"
          else match path with
            | [ x ] when x = root -> Ok (true, ids)
            | _ -> Ok (r, ids)
      in
      acc >>= fun (r, ids) ->
      add_id diff.their_name (r, ids) >>= fun (r', ids') ->
      add_id diff.mine_name (r', ids')
    )
    (Ok (false, S.empty)) diffs
