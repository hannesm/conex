open Core

(* crappy data implementation using "structured" strings *)
type key = string
type value =
  | String of string
  | Int of int64

type t =
  | Entry of key * t
  | List of t list
  | Leaf of value

(* we do not recurse into the tree, just working on the top *)
let rec get t key =
  match t with
  | Entry (k, v) when k = key -> Some v
  | List xs ->
     let isit = function None -> false | Some _ -> true in
     (try List.find isit (List.map (fun x -> get x key) xs) with Not_found -> None)
  | Leaf _ -> None
  | Entry _ -> None

let get_exn t key =
  match get t key with
  | Some x -> x
  | None -> invalid_arg ("couldn't find key: " ^ key)

let extract_string_exn = function
  | Leaf (String x) -> x
  | _ -> invalid_arg "couldn't find string"

let extract_int_exn = function
  | Leaf (Int x) -> x
  | _ -> invalid_arg "couldn't find int"

let rec to_string = function
  | Entry (k, v) -> k ^ ":" ^ (to_string v)
  | List xs -> "[" ^ (String.concat ","  (List.map to_string xs)) ^ "]"
  | Leaf (String s) -> "\"" ^ s ^ "\""
  | Leaf (Int i) -> Int64.to_string i

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
      let ws = try String.index buf ' '
               with Not_found -> String.length buf
      and comma = try String.index buf ','
                  with Not_found -> String.length buf
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
      (Some (Leaf (Int (Int64.of_string ss))),
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

let parse data =
  let rec go str acc =
    match parse' str with
    | None, "" -> List.rev acc
    | Some x, "" -> List.rev (x :: acc)
    | None, rst -> go rst acc
    | Some x, rst -> go rst (x :: acc)
  in
  match go data [] with
  | [] -> invalid_arg "empty"
  | [x] -> x
  | xs -> List xs

let normalise = to_string


let signature_to_data (id, alg, signature) =
  let alg = algorithm_to_string alg in
  List [ Entry ("keyid", Leaf (String id)) ;
         Entry ("method", Leaf (String alg)) ;
         Entry ("sig", Leaf (String signature)) ]

let data_to_signature signature =
  let keyid = extract_string_exn (get_exn signature "keyid")
  and sig_alg = extract_string_exn (get_exn signature "method")
  and sigval = extract_string_exn (get_exn signature "sig")
  in
  let alg = string_to_algorithm sig_alg in
  (keyid, alg, sigval)

(* not sure where this belongs *)
let parse_signed_data data =
  match get data "signatures" with
  | Some (Entry _) -> invalid_arg "signatures should be a list, not an entry"
  | Some (Leaf _) -> invalid_arg "signatures should be a list, not a leaf"
  | Some (List xs) ->
     let signed = get_exn data "signed" in
     (signed, List.map data_to_signature xs)
  | None ->
     (* everything is signed... signatures are empty *)
     (data, [])

let combine_signed data sigs =
  List [ Entry ("signed", data) ;
         Entry ("signatures", List (List.map signature_to_data sigs)) ]

let publickey_to_data pubkey =
  let pem = match pubkey.Publickey.key with
    | None -> "NONE"
    | Some x -> Publickey.encode_key x
  and role = role_to_string pubkey.Publickey.role
  in
  List [ Entry ("signed", List [ Entry ("counter", Leaf (Int pubkey.Publickey.counter)) ;
                                 Entry ("keyid"  , Leaf (String pubkey.Publickey.keyid)) ;
                                 Entry ("key"    , Leaf (String pem)) ;
                                 Entry ("role"   , Leaf (String role)) ]);
         Entry ("signatures", List (List.map signature_to_data pubkey.Publickey.signatures)) ]

let data_to_publickey data =
  let signed, signatures = parse_signed_data data in
  let counter = extract_int_exn (get_exn signed "counter")
  and keyid = extract_string_exn (get_exn signed "keyid")
  and key = extract_string_exn (get_exn signed "key")
  and role = extract_string_exn (get_exn signed "role")
  in
  let key =
    match key with
    | "NONE" -> None
    | _ -> Some (Publickey.decode_key key)
  and role = string_to_role role
  in
  Publickey.publickey ~counter ~role ~signatures  keyid key

let publickey_raw pk =
  let data = publickey_to_data pk in
  let signed, _ = parse_signed_data data in
  normalise signed

let delegate_to_data d =
  let id s = Leaf (String s) in
  List [ Entry ("signed", List [ Entry ("name"   , Leaf (String d.Delegate.name)) ;
                                 Entry ("counter", Leaf (Int d.Delegate.counter)) ;
                                 Entry ("key-ids", List (List.map id d.Delegate.validids)) ]);
         Entry ("signatures", List (List.map signature_to_data d.Delegate.signatures)) ]

let data_to_delegate data =
  let signed, signatures = parse_signed_data data in
  let id x = extract_string_exn x in
  let name = extract_string_exn (get_exn signed "name")
  and counter = extract_int_exn (get_exn signed "counter")
  and validids = match get_exn signed "key-ids" with
    | List es -> List.map id es
    | _ -> invalid_arg "validids not a list"
  in
  { Delegate.name ; counter ; validids ; signatures }

let delegate_raw del =
  let data = delegate_to_data del in
  let signed, _ = parse_signed_data data in
  normalise signed

let checksum_to_data c =
  List [ Entry ("filename", Leaf (String c.Checksum.filename)) ;
         Entry ("byte-size", Leaf (Int c.Checksum.bytesize)) ;
         Entry ("sha256", Leaf (String c.Checksum.checksum)) ]

let data_to_checksum data =
  let filename = extract_string_exn (get_exn data "filename")
  and bytesize = extract_int_exn (get_exn data "byte-size")
  and checksum = extract_string_exn (get_exn data "sha256")
  in
  { Checksum.filename ; bytesize ; checksum }


let checksums_to_data cs =
  let csums = Checksum.fold (fun c acc -> checksum_to_data c :: acc) cs.Checksum.files [] in
  List [ Entry ("signed", List [ Entry ("counter", Leaf (Int cs.Checksum.counter)) ;
                                 Entry ("name", Leaf (String cs.Checksum.name)) ;
                                 Entry ("files", List csums) ]) ;
         Entry ("signatures", List (List.map signature_to_data cs.Checksum.signatures)) ]

let data_to_checksums data =
  let signed, signatures = parse_signed_data data in
  let counter = extract_int_exn (get_exn signed "counter")
  and name = extract_string_exn (get_exn signed "name")
  and sums = get_exn signed "files"
  in
  let files = match sums with
    | List elements -> List.map data_to_checksum elements
    | _ -> invalid_arg "unknown files"
  in
  Checksum.checksums ~counter ~signatures name files

let checksums_raw cs =
  let data = checksums_to_data cs in
  let signed, _ = parse_signed_data data in
  normalise signed
