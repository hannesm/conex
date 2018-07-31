
type 'a fmt = Format.formatter -> 'a -> unit

(*BISECT-IGNORE-BEGIN*)
let pp_list pe ppf xs =
  match xs with
  | [] -> Format.pp_print_string ppf "empty"
  | xs ->
    Format.pp_print_string ppf "[" ;
    let rec p1 = function
      | [] -> Format.pp_print_string ppf "]" ;
      | [x] -> Format.fprintf ppf "%a]" pe x
      | x::xs -> Format.fprintf ppf "%a;@ " pe x ; p1 xs
    in
    p1 xs
(*BISECT-IGNORE-END*)

module S = struct
  include Set.Make(String)

  let pp fmt t = pp_list Format.pp_print_string fmt (elements t)

  let of_list es = List.fold_right add es empty
end

let str_pp pp e =
  Format.(fprintf str_formatter "%a" pp e) ;
  Format.flush_str_formatter ()

let (>>=) a f =
  match a with
  | Ok x -> f x
  | Error e -> Error e

let guard p err = if p then Ok () else Error err

let rec foldM f n = function
  | [] -> Ok n
  | x::xs -> f n x >>= fun n' -> foldM f n' xs

let rec iterM f = function
  | [] -> Ok ()
  | x::xs -> f x >>= fun () -> iterM f xs

let foldS f a s =
  S.fold (fun id r ->
      r >>= fun r ->
      f r id) s (Ok a)

let err_to_str pp = function
  | Ok a -> Ok a
  | Error e -> Error (str_pp pp e)

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
        | Some (a, b) -> doit (a :: acc) b
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
    compare (to_lower a) (to_lower b)
end

module Uint = struct
  type t = int64

  let zero = 0L

  let max = -1L (* this is 0xFFFFFFFFFFFFFFFF *)

  let compare a b =
    Int64.(compare (sub a min_int) (sub b min_int))

  let succ x =
    if x = max then
      (true, 0L)
    else
      (false, Int64.succ x)

  let to_string s = Printf.sprintf "%LX" s

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf i = Format.pp_print_string ppf (to_string i)
  (*BISECT-IGNORE-END*)

  let decimal s = Printf.sprintf "%Lu" s

  let of_string s =
    try Some (Int64.of_string ("0x" ^ s)) with Failure _ -> None

  let of_float f =
    if f < 0. then
      None
    else
      try Some (Int64.of_float f) with Failure _ -> None

  let of_int_exn i =
    if i < 0 then
      invalid_arg "cannot convert integers smaller than 0"
    else
      Int64.of_int i

  let of_int i = try Some (of_int_exn i) with Failure _ -> None

end

module Uint_map = struct
  include Map.Make(Uint)
  let find k m = try Some (find k m) with Not_found -> None
end

module M = struct
  include Map.Make(String)

  let find k m = try Some (find k m) with Not_found -> None

  (*BISECT-IGNORE-BEGIN*)
  let pp pp_e ppf m =
    iter (fun k v -> Format.fprintf ppf "%s -> %a@ " k pp_e v) m
  (*BISECT-IGNORE-END*)
end

let rec filter_map ~f = function
  | []    -> []
  | x::xs ->
      match f x with
      | None    ->       filter_map ~f xs
      | Some x' -> x' :: filter_map ~f xs

(* this is stripped down from Logs library *)
module type LOGS = sig
  module Tag : sig
    type set
  end

  type ('a, 'b) msgf =
    (?header:string -> ?tags:Tag.set ->
     ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
  type 'a log = ('a, unit) msgf -> unit

  type src

  val warn_count : unit -> int

  val debug : ?src:src -> 'a log
  val info : ?src:src -> 'a log
  val warn : ?src:src -> 'a log
end

type file_type = File | Directory

type path = string list

let root = []

let non_path = [ ".." ; "." ; "" ; "/" ]

let path_to_string path =
  assert (not (List.exists (fun s -> List.mem s non_path) path)) ;
  List.fold_left Filename.concat "" path

let string_to_path_exn str =
  let segments = String.cuts '/' str in
  let segs = match segments with
    | ""::xs -> xs
    | xs -> xs
  in
  if List.exists (fun s -> List.mem s non_path) segs then
    invalid_arg "invalid path"
  else
    segs

let string_to_path str =
  try Ok (string_to_path_exn str) with
  | Invalid_argument m -> Error m

let path_equal a b =
  let str_eq a b = String.compare a b = 0 in
  try List.for_all2 str_eq a b with _ -> false

let rec subpath ~parent b =
  let str_eq a b = String.compare a b = 0 in
  match parent, b with
  | [], [] -> false
  | [], _ -> true
  | _, [] -> false
  | hd::tl, hd'::tl' -> if str_eq hd hd' then subpath ~parent:tl tl' else false

(*BISECT-IGNORE-BEGIN*)
let pp_path fmt p = Format.pp_print_string fmt (path_to_string p)
(*BISECT-IGNORE-END*)

type item = file_type * string

module Tree = struct
  type 'a t = Node of 'a list * 'a t M.t

  let rec equal eq (Node (del, map)) (Node (del', map')) =
    (try List.for_all2 eq del del' with _ -> false) &&
    M.equal (equal eq) map map'

  let empty = Node ([], M.empty)

  let is_empty = function
    | Node ([], m) when M.is_empty m -> true
    | Node (_, _) -> false

  let rec sub path t = match path, t with
    | [], n -> n
    | hd::tl, Node (_, m) -> match M.find hd m with
      | None -> empty
      | Some n -> sub tl n

  let fold f acc root =
    let rec doit path (Node (v, map)) acc =
      let acc' = f path v acc in
      M.fold (fun k v acc -> doit (path @ [k]) v acc) map acc'
    in
    doit [] root acc

(*BISECT-IGNORE-BEGIN*)
  let pp pp_e ppf node =
    let rec pp prefix ppf (Node (dels, map)) =
      let pp_map ppf map =
        List.iter (fun (key, node) ->
            let prefix' = prefix ^ "/" ^ key in
            Format.fprintf ppf "@[<2>%s@ ->@ %a@]@,"
              prefix' (pp prefix') node)
          (M.bindings map)
      in
      Format.fprintf ppf "@[<2>values:@ %a@.%a@,@]@,"
        (pp_list pp_e) dels
        pp_map map
    in
    (pp "") ppf node
(*BISECT-IGNORE-END*)

  let rec lookup path (Node (dels, map)) =
    match path with
    | [] -> Some dels
    | hd::tl -> match M.find hd map with
      | None -> None
      | Some x -> lookup tl x

  let lookup_prefix path node =
    let rec lookup sofar path (Node (dels, map)) =
      let ext = match dels with [] -> sofar | d -> d in
      match path with
      | [] -> ext
      | hd::tl -> match M.find hd map with
        | None -> ext
        | Some x -> lookup ext tl x
    in
    lookup [] path node

  let rec insert path value (Node (dels, map)) =
    match path with
    | [] -> Node (dels @ [ value ], map)
    | hd::tl ->
      let n = match M.find hd map with
        | None -> empty
        | Some x -> x
      in
      let res = insert tl value n in
      Node (dels, M.add hd res map)
end
