
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
  match Strhelper.cut ',' (Strhelper.slice ~start:1 data) with
  | None when data = "+1" -> (0, 1)
  | None when data = "-1" -> (0, 1)
  | None -> invalid_arg ("start_len broken in " ^ data)
  | Some (start, len) ->
     let start = int_of_string start in
     let st = if start = 0 then start else pred start in
     (st, int_of_string len)

let count_to_sl_sl data =
  if Strhelper.is_prefix ~prefix:"@@" data then
    (* input: "@@ -19,23 +19,12 @@ bla" *)
    (* output: ((19,23), (19, 12)) *)
    match Strhelper.cuts '@' data with
    | numbers::_ ->
       let nums = String.trim numbers in
       (match Strhelper.cut ' ' nums with
        | None -> invalid_arg "couldn't find space in count"
        | Some (mine, theirs) -> Some (to_start_len mine, to_start_len theirs))
    | _ -> invalid_arg "broken line!"
  else
    None

let sort_into_bags mine their str =
  match String.get str 0, Strhelper.slice ~start:1 str with
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
  if Strhelper.is_prefix ~prefix:"a/" name || Strhelper.is_prefix ~prefix:"b/" name then
    Strhelper.slice ~start:2 name
  else
    name

let to_diff data =
  (* first locate --- and +++ lines *)
  let cut4 = Strhelper.slice ~start:4 in
  let rec find_start = function
    | [] -> None
    | x::y::xs when Strhelper.is_prefix ~prefix:"---" x -> Some (cut4 x, cut4 y, xs)
    | _::xs -> find_start xs
  in
  match find_start data with
  | Some (mine_name, their_name, rest) ->
    let hunks, rest = to_hunks [] rest in
    Some ({ mine_name ; their_name ; hunks }, rest)
  | None -> None

let to_lines = Strhelper.cuts '\n'

let to_diffs data =
  let lines = to_lines data in
  let rec doit acc = function
    | [] -> List.rev acc
    | xs -> match to_diff xs with
      | None -> acc
      | Some (diff, rest) -> doit (diff :: acc) rest
  in
  doit [] lines

let apply filedata diff =
  let lines = match filedata with None -> [] | Some x -> to_lines x in
  let lines = List.fold_left apply_hunk lines diff.hunks in
  String.concat "\n" lines
