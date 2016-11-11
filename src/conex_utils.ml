let option none some = function
  | None   -> none
  | Some x -> some x

let rec filter_map ~f = function
  | []    -> []
  | x::xs ->
      match f x with
      | None    ->       filter_map ~f xs
      | Some x' -> x' :: filter_map ~f xs

module String = struct
  type t = string

  let cut sep str =
    try
      let idx = String.index str sep
      and l = String.length str
      in
      let sidx = succ idx in
      Some (String.sub str 0 idx, String.sub str sidx (l - sidx))
    with
      Not_found -> None

  let cuts sep str =
    let rec doit acc s =
      if String.length s = 0 then
        List.rev acc
      else
        match cut sep s with
        | None -> List.rev (s :: acc)
        | Some (a, b) when String.length a > 0 -> doit (a :: acc) b
        | Some (_, b) -> doit acc b
    in
    doit [] str

  let slice ?(start = 0) ?stop str =
    let stop = match stop with
      | None -> String.length str
      | Some x -> x
    in
    let len = stop - start in
    String.sub str start len

  let is_prefix ~prefix str =
    let pl = String.length prefix in
    if String.length str < pl then
      false
    else
      String.sub str 0 (String.length prefix) = prefix

  let is_suffix ~suffix str =
    let sl = String.length suffix in
    if String.length str < sl then
      false
    else
      String.sub str (String.length str - sl) sl = suffix

  let cut_suffix ~suffix str =
    if is_suffix ~suffix str then
      String.sub str 0 (String.length str - String.length suffix)
    else
      invalid_arg ("suffix " ^ suffix ^ " is not a suffix of string " ^ str)

  let lowercase_char = function
    | 'A' .. 'Z' as c -> char_of_int (int_of_char c + 0x20)
    | c -> c

  let to_lower s =
    let last = pred (String.length s)
    and bs = Bytes.of_string s
    in
    for k = 0 to last do
      Bytes.set bs k (lowercase_char (Bytes.get bs k))
    done ;
    Bytes.to_string bs

  let ascii_char ?(p = fun _ -> false) = function
    | '0' .. '9'
    | 'A' .. 'Z'
    | 'a' .. 'z' -> true
    | x -> p x

  let is_ascii ?p s =
    let last = pred (String.length s) in
    let res = ref true in
    for k = 0 to last do
      res := !res && ascii_char ?p (String.get s k)
    done;
    !res

  let trim = String.trim

  let get = String.get

  let concat = String.concat

  let compare = String.compare

  let length = String.length

  let compare_insensitive a b =
    compare (to_lower a) (to_lower b) = 0
end
