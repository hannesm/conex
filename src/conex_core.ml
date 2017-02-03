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

type priv = [ `RSA_priv of string ]

type pub = [ `RSA_pub of string ]

let pub_equal (`RSA_pub a) (`RSA_pub b) = String.compare a b = 0
(*BISECT-IGNORE-BEGIN*)
let pp_pub ppf (`RSA_pub x) = Format.pp_print_int ppf (String.length x)
(*BISECT-IGNORE-END*)

let pubtype_to_string = function
  | `RSA_pub _ -> "RSA"

let string_to_pubtype = function
  | "RSA" -> Some (`RSA_pub "")
  | _ -> None

type sigtype = [ `RSA_PSS_SHA256 ]

let sigtype_to_string = function
  | `RSA_PSS_SHA256 -> "RSA-PSS-SHA256"

let string_to_sigtype = function
  | "RSA-PSS-SHA256" -> Some `RSA_PSS_SHA256
  | _ -> None

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

type sig_hdr = {
  created : Uint.t ;
  sigtyp : sigtype ;
  signame : identifier ;
}
let extend_sig hdr data =
  String.concat "M  A   R   K"
    [ Uint.to_string hdr.created ; sigtype_to_string hdr.sigtyp ; hdr.signame ; data ]

type signature = sig_hdr * string

let pp_signature ppf (hdr, data) =
  Format.fprintf ppf "signature %s (created %s) type %s: %d bytes"
    hdr.signame (Uint.to_string hdr.created) (sigtype_to_string hdr.sigtyp)
    (String.length data)

type digest_typ = [ `SHA256 ]
let digest_typ_to_string = function `SHA256 -> "SHA256"
let string_to_digest_typ = function
  | "SHA256" -> Some `SHA256
  | _ -> None

type digest = digest_typ * string
let digest_to_string (a, b) = digest_typ_to_string a ^ b
let digest_eq (ta, a) (tb, b) = match ta, tb with
  | `SHA256, `SHA256 -> String.compare a b = 0

(*BISECT-IGNORE-BEGIN*)
let pp_digest ppf (`SHA256, x) =
  Format.fprintf ppf "SHA256: %s" x
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
  | `InvalidBase64Encoding
  | `InvalidSignature
  | `InvalidPublicKey
  | `NoSignature
]

(*BISECT-IGNORE-BEGIN*)
let pp_verification_error ppf = function
  | `InvalidBase64Encoding -> Format.fprintf ppf "signature: no valid base64 encoding"
  | `InvalidSignature -> Format.fprintf ppf "signature: invalid"
  | `InvalidPublicKey -> Format.fprintf ppf "invalid public key"
  | `NoSignature -> Format.fprintf ppf "no signature found"
(*BISECT-IGNORE-END*)

let (>>=) a f =
  match a with
  | Ok x -> f x
  | Error e -> Error e

let guard p err = if p then Ok () else Error err

let rec foldM f n = function
  | [] -> Ok n
  | x::xs -> f n x >>= fun n' -> foldM f n' xs
