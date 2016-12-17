open Conex_result
open Conex_data_persistency
open Conex_core

(* XXX: improve the Int64 case! *)
let rec encode = function
  | Entry (k, v) -> k ^ ":" ^ (encode v)
  | List xs -> "[" ^ (String.concat ",\n"  (List.map encode xs)) ^ "]"
  | Leaf (String s) -> "\"" ^ s ^ "\""
  | Leaf (Int i) -> Uint.to_string i

let rec parse' data =
  let is_entry buf =
    let ws = try String.index buf ' '
             with Not_found -> String.length buf
    and colon = try String.index buf ':'
                with Not_found -> String.length buf
    in
    colon < ws
  and is_leaf buf =
    let str = String.get buf 0 = '"'
    and num =
      let ws = try String.index buf ' ' with Not_found -> String.length buf
      and comma = try String.index buf ',' with Not_found -> String.length buf
      in
      let bound = min ws comma in
      try int_of_string (String.sub buf 0 bound) >= 0
      with _ -> false
    in
    str || num
  and is_list buf = String.get buf 0 = '['
  and is_nl buf = String.get buf 0 = '\n'
  in
  let parse_entry buf =
    let colon = String.index buf ':' in
    let colon' = succ colon in
    match parse' (String.sub buf colon' (String.length buf - colon')) with
    | Some value, rst -> (Some (Entry (String.sub buf 0 colon, value)), rst)
    | None, _ -> invalid_arg "couldn't parse rhs of an entry"
  and parse_leaf buf =
    if String.get buf 0 = '"' then
      let stop = String.index_from buf 1 '"' in
      let stop' = succ stop in
      (Some (Leaf (String (String.sub buf 1 (pred stop)))),
       String.sub buf stop' (String.length buf - stop'))
    else
      let rec go idx =
        match String.get buf idx with
        | '0' .. '9' -> go (succ idx)
        | _ -> idx
      in
      let stop = go 0 in
      let ss = String.sub buf 0 stop in
      (Some (Leaf (Int (Uint.of_string ss))),
       String.sub buf stop (String.length buf - stop))
  and parse_list buf =
    let rec go str acc =
      if String.get str 0 = ']' then
        let rest = String.sub str 1 (pred (String.length str)) in
        (Some (List (List.rev acc)), rest)
      else
        match parse' str with
        | Some d, rst ->
           if String.get rst 0 = ',' then
             go (String.sub rst 1 (pred (String.length rst))) (d :: acc)
           else if String.get rst 0 = ']' then
             let rest = String.sub rst 1 (pred (String.length rst)) in
             (Some (List (List.rev (d :: acc))), rest)
           else
             invalid_arg "unknown list"
        | None, _ -> invalid_arg "couldn't parse list entry"
    in
    go (String.sub buf 1 (pred (String.length buf))) []
  in
  if data = "" then
    (None, "")
  else if is_nl data then
    parse' (String.sub data 1 (pred (String.length data)))
  else if is_list data then
    parse_list data
  else if is_leaf data then
    parse_leaf data
  else if is_entry data then
    parse_entry data
  else
    invalid_arg ("invalid string: " ^ data)

let decode data =
  let rec go str acc =
    match parse' str with
    | None, "" -> List.rev acc
    | Some x, "" -> List.rev (x :: acc)
    | None, rst -> go rst acc
    | Some x, rst -> go rst (x :: acc)
  in
  try
    match go data [] with
    | [] -> Error "empty"
    | [x] -> Ok x
    | xs -> Ok (List xs)
  with Invalid_argument e -> Error e
