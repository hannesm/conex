open Conex_result
open Conex_utils

module Uint = struct
  type t = int64

  let zero = 0L

  let max = -1L (* this is 0xFFFFFFFFFFFFFFFF *)

  let compare a b =
    if a = b then
      0
    else if (a >= 0L && b >= 0L) || (a < 0L && b < 0L) then
      Int64.compare a b
    else if a < 0L then 1 else -1

  let succ x =
    if x = max then
      (true, 0L)
    else
      (false, Int64.succ x)

  let to_string s = Printf.sprintf "%LX" s

  let of_string s = Scanf.sscanf s "%LX" (fun x -> x)

  let of_float f =
    if f < 0. then
      invalid_arg "reading floating point smaller 0 not supported"
    else
      Int64.of_float f

  let of_int i =
    if i < 0 then
      invalid_arg "of_int with < 0 not supported"
    else
      Int64.of_int i
end

module S = Set.Make(String)

let s_of_list es = List.fold_left (fun s v -> S.add v s) S.empty es

module M = Map.Make(String)

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

type file_type = File | Directory

type path = string list

let path_to_string path =
  let skip x = List.mem x [ "." ; "" ; "/" ] in
  List.fold_left (fun d f ->
                  match d, f with
                  | "..", _ -> invalid_arg "there's no escape!"
                  | _, ".." -> invalid_arg "no escape for files!"
                  | d, f when skip d -> f
                  | d, f when skip f -> d
                  | d, f -> Filename.concat d f)
                 "" path

let string_to_path str = String.cuts '/' str

module Provider = struct
  type item = [
    | `File of string
    | `Dir of string
  ]

  type err = [ `NotFound | `UnknownFileType of string ]

  type t = {
    name : string ;
    description : string ;
    file_type : path -> (file_type, err) result ;
    read : path -> (string, err) result ;
    write : path -> string -> unit ;
    read_dir : path -> (item list, err) result ;
    exists : path -> bool ;
  }

  (*BISECT-IGNORE-BEGIN*)
  let pp_provider ppf t =
    Format.fprintf ppf "repository %s: %s" t.name t.description
  (*BISECT-IGNORE-END*)
end

type pub = [ `Pub of string ]
type priv = [ `Priv of string ]


type name = string

(*BISECT-IGNORE-BEGIN*)
let pp_name ppf x = Format.pp_print_string ppf x
(*BISECT-IGNORE-END*)

let name_equal a b = String.compare_insensitive a b


type identifier = string

(*BISECT-IGNORE-BEGIN*)
let pp_id ppf x = Format.pp_print_string ppf x
(*BISECT-IGNORE-END*)

let id_equal a b = String.compare_insensitive a b


type digest = string

(*BISECT-IGNORE-BEGIN*)
let pp_digest ppf x = Format.pp_print_string ppf x
(*BISECT-IGNORE-END*)


type resource = [
  | `PublicKey
  | `Team
  | `Checksums
  | `Releases
  | `Authorisation
  | `Index
]

let resource_equal a b = match a, b with
  | `PublicKey, `PublicKey
  | `Team, `Team
  | `Checksums, `Checksums
  | `Releases, `Releases
  | `Authorisation, `Authorisation
  | `Index, `Index-> true
  | _ -> false

let resource_to_string = function
  | `PublicKey -> "publickey"
  | `Team -> "team"
  | `Checksums -> "checksums"
  | `Releases -> "releases"
  | `Authorisation -> "authorisation"
  | `Index -> "index"

let string_to_resource = function
  | "publickey" -> Some `PublicKey
  | "team" -> Some `Team
  | "checksums" -> Some `Checksums
  | "releases" -> Some `Releases
  | "authorisation" -> Some `Authorisation
  | "index" -> Some `Index
  | _ -> None

(*BISECT-IGNORE-BEGIN*)
let pp_resource ppf k = Format.pp_print_string ppf (resource_to_string k)
(*BISECT-IGNORE-END*)


type verification_error = [
  | `InvalidBase64Encoding of identifier
  | `InvalidSignature of identifier
  | `InvalidPublicKey of identifier
  | `InvalidIdentifier of identifier
  | `NotAuthorised of identifier * identifier
  | `NoSignature of identifier
]

(*BISECT-IGNORE-BEGIN*)
let pp_verification_error ppf = function
  | `InvalidBase64Encoding id -> Format.fprintf ppf "%a signature is not in valid base64 encoding" pp_id id
  | `InvalidSignature id -> Format.fprintf ppf "%a signature is not valid data" pp_id id
  | `InvalidPublicKey id -> Format.fprintf ppf "keystore contained no valid public key for %s" id
  | `InvalidIdentifier id -> Format.fprintf ppf "identifier %s was not found in keystore" id
  | `NotAuthorised (auth, sign) -> Format.fprintf ppf "only %a is authorised to sign this index, but it is signed by %a" pp_id auth pp_id sign
  | `NoSignature s -> Format.fprintf ppf "no signature found on index %a" pp_id s
(*BISECT-IGNORE-END*)

type base_v_err = [ `InvalidBase64 | `InvalidPubKey | `InvalidSig ]

let (>>=) a f =
  match a with
  | Ok x -> f x
  | Error e -> Error e

let guard p err = if p then Ok () else Error err

let rec foldM f n = function
  | [] -> Ok n
  | x::xs -> f n x >>= fun n' -> foldM f n' xs
