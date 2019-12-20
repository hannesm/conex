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

let drop data num =
  let rec drop data num =
    match num, data with
    | 0, _ -> data
    | n, _::xs -> drop xs (pred n)
    | _ -> invalid_arg "drop broken"
  in
  try drop data num with
  | Invalid_argument _ -> invalid_arg ("drop " ^ string_of_int num ^ " on " ^ string_of_int (List.length data))

(* TODO verify that it applies cleanly *)
let apply_hunk old (index, to_build) hunk =
  try
    let prefix = take (drop old index) (hunk.mine_start - index) in
    (hunk.mine_start + hunk.mine_len, to_build @ prefix @ hunk.their)
  with
  | Invalid_argument _ -> invalid_arg ("apply_hunk " ^ string_of_int index ^ " old len " ^ string_of_int (List.length old) ^
                                       " hunk start " ^ string_of_int hunk.mine_start ^ " hunk len " ^ string_of_int hunk.mine_len)

let to_start_len data =
  (* input being "?19,23" *)
  match String.cut ',' (String.slice ~start:1 data) with
  | None when data = "+1" || data = "-1" -> (0, 1)
  | None -> invalid_arg ("start_len broken in " ^ data)
  | Some (start, len) ->
     let len = int_of_string len
     and start = int_of_string start
     in
     let st = if len = 0 || start = 0 then start else pred start in
     (st, len)

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

type operation =
  | Edit of string
  | Rename of string * string
  | Delete of string
  | Create of string
  | Rename_only of string * string

let operation_eq a b = match a, b with
  | Edit a, Edit b
  | Delete a, Delete b
  | Create a, Create b -> String.equal a b
  | Rename (a, a'), Rename (b, b')
  | Rename_only (a, a'), Rename_only (b, b') -> String.equal a b && String.equal a' b'
  | _ -> false

let no_file = "/dev/null"

let pp_operation ~git ppf op =
  let real_name direction name =
    if git then name else
      match direction with `Mine -> "a/" ^ name | `Theirs -> "b/" ^ name
  in
  let hdr mine their =
    (* even if create/delete, /dev/null is not used in this header *)
    (* according to git documentation *)
    if git then
      Format.fprintf ppf "diff --git %s %s\n"
        (real_name `Mine mine) (real_name `Theirs their)
  in
  match op with
  | Edit name ->
    hdr name name ;
    Format.fprintf ppf "--- %s\n" (real_name `Mine name) ;
    Format.fprintf ppf "+++ %s\n" (real_name `Theirs name)
  | Rename (old_name, new_name) ->
    hdr old_name new_name ;
    Format.fprintf ppf "--- %s\n" (real_name `Mine old_name) ;
    Format.fprintf ppf "+++ %s\n" (real_name `Theirs new_name)
  | Delete name ->
    hdr name name ;
    Format.fprintf ppf "--- %s\n" (real_name `Mine name) ;
    Format.fprintf ppf "+++ %s\n" no_file
  | Create name ->
    hdr name name ;
    Format.fprintf ppf "--- %s\n" no_file ;
    Format.fprintf ppf "+++ %s\n" (real_name `Theirs name)
  | Rename_only (old_name, new_name) ->
    hdr old_name new_name ;
    Format.fprintf ppf "rename from %s\n" old_name;
    Format.fprintf ppf "rename to %s\n" new_name

type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}

let pp ~git ppf t =
  pp_operation ~git ppf t.operation ;
  List.iter (pp_hunk ppf) t.hunks

let operation_of_strings git mine their =
  let get_filename_opt n =
    let s = match String.cut '\t' n with None -> n | Some (x, _) -> x in
    if s = no_file then None else
    if git && (String.is_prefix ~prefix:"a/" s || String.is_prefix ~prefix:"b/" s) then
      Some (String.slice ~start:2 s)
    else Some s
  in
  match get_filename_opt mine, get_filename_opt their with
  | None, Some n -> Create n
  | Some n, None -> Delete n
  | Some a, Some b -> if String.equal a b then Edit a else Rename (a, b)
  | None, None -> assert false (* ??!?? *)

(* parses a list of lines to a diff.t list *)
let to_diff data =
  (* first locate --- and +++ lines *)
  let rec find_start git ?hdr = function
    | [] -> hdr, []
    | x::xs when String.is_prefix ~prefix:"diff --git" x ->
      begin match hdr with None -> find_start true xs | Some _ -> hdr, x::xs end
    | x::y::xs when String.is_prefix ~prefix:"rename from" x && String.is_prefix ~prefix:"rename to" y ->
      let hdr = Rename_only (String.slice ~start:12 x, String.slice ~start:10 y) in
      find_start git ~hdr xs
    | x::y::xs when String.is_prefix ~prefix:"---" x ->
      let mine = String.slice ~start:4 x and their = String.slice ~start:4 y in
      Some (operation_of_strings git mine their), xs
    | _::xs -> find_start git ?hdr xs
  in
  match find_start false data with
  | Some (Rename_only _ as operation), rest ->
    let hunks = [] and mine_no_nl = false and their_no_nl = false in
    Some ({ operation ; hunks ; mine_no_nl ; their_no_nl }, rest)
  | Some operation, rest ->
    let hunks, mine_no_nl, their_no_nl, rest = to_hunks (false, false, []) rest in
    Some ({ operation ; hunks ; mine_no_nl ; their_no_nl }, rest)
  | None, [] -> None
  | None, _ -> assert false

let to_lines = String.cuts '\n'

let to_diffs data =
  let lines = to_lines data in
  let rec doit acc = function
    | [] -> List.rev acc
    | xs -> match to_diff xs with
      | None -> List.rev acc
      | Some (diff, rest) -> doit (diff :: acc) rest
  in
  doit [] lines

let patch filedata diff =
  match diff.operation with
  | Rename_only _ -> filedata
  | Delete _ -> None
  | Create _ ->
    begin match diff.hunks with
      | [ the_hunk ] ->
        let d = the_hunk.their in
        let lines = if diff.their_no_nl then d else d @ [""] in
        Some (String.concat "\n" lines)
      | _ -> assert false
    end
  | _ ->
    let old = match filedata with None -> [] | Some x -> to_lines x in
    let idx, lines = List.fold_left (apply_hunk old) (0, []) diff.hunks in
    let lines = lines @ drop old idx in
    let lines =
      match diff.mine_no_nl, diff.their_no_nl with
      | false, true -> (match List.rev lines with ""::tl -> List.rev tl | _ -> lines)
      | true, false -> lines @ [ "" ]
      | false, false when filedata = None -> lines @ [ "" ]
      | false, false -> lines
      | true, true -> lines
    in
    Some (String.concat "\n" lines)

(* TODO which equality to use here? is = ok? *)
let ids root keydir diffs =
  List.fold_left (fun acc diff ->
      let add_name name (r, ids) =
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
      match diff.operation with
      | Create a | Delete a | Edit a -> add_name a (r, ids)
      | Rename (a, b) | Rename_only (a, b) -> add_name a (r, ids) >>= add_name b)
    (Ok (false, S.empty)) diffs
