open Conex_result
open Conex_core
open Conex_utils
open Conex_resource
open Conex_crypto

let private_dir = Filename.concat (Sys.getenv "HOME") ".conex"

let private_keys p =
  let is_private s =
    if String.is_suffix ~suffix:".private" s then
      let p = string_to_path (p.Conex_provider.name) in
      match List.rev (String.cuts '.' s) with
      | _::id::path when p = List.rev path -> Some id
      | _ -> None
    else
      None
  in
  Conex_persistency.collect_dir private_dir >>= fun files ->
  List.fold_left
    (fun acc s ->
       acc >>= fun acc ->
       match is_private s with
       | None -> Ok acc
       | Some s -> Ok (s :: acc))
    (Ok [])
    files

let private_key_path path id =
  let filename =
    let els = string_to_path path @ [id ; "private"] in
    String.concat "." els
  in
  "/" ^ path_to_string (string_to_path private_dir @ [ filename ])

let write_private_key prov id key =
  let base = prov.Conex_provider.name in
  let filename = private_key_path base id in
  (if Conex_persistency.exists filename then begin
      let ts =
        let open Unix in
        let t = gmtime (stat filename).st_mtime in
        Printf.sprintf "%4d%2d%2d%2d%2d%2d" (t.tm_year + 1900) (succ t.tm_mon)
          t.tm_mday t.tm_hour t.tm_min t.tm_sec
      in
      let backfn = String.concat "." [ id ; ts ] in
      let backup = private_key_path base backfn in
      let rec inc n =
        let nam = backup ^ "." ^ string_of_int n in
        if Conex_persistency.exists nam then inc (succ n) else nam
      in
      let backup = if Conex_persistency.exists backup then inc 0 else backup in
      Conex_persistency.rename filename backup
    end else Ok ()) >>= fun () ->
  (if not (Conex_persistency.exists private_dir) then
     Conex_persistency.mkdir ~mode:0o700 private_dir
   else Ok ()) >>= fun () ->
  match Conex_persistency.file_type private_dir with
  | Ok Directory ->
    let data = match key with `Priv (alg, data, created) ->
      String.concat ";" [ (Key.alg_to_string alg) ; data ; (Uint.to_string created) ]
    in
    Conex_persistency.write_file ~mode:0o400 filename data
  | _ -> Error (private_dir ^ " is not a directory!")

type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list | `Msg of string]

(*BISECT-IGNORE-BEGIN*)
let pp_err ppf = function
  | `NotFound x -> Format.fprintf ppf "couldn't find private key %s" x
  | `NoPrivateKey -> Format.pp_print_string ppf "no private key found"
  | `MultiplePrivateKeys keys -> Format.fprintf ppf "multiple private keys found %s" (String.concat ", " keys)
  | `Msg m -> Format.fprintf ppf "error %s while trying to read private key" m
(*BISECT-IGNORE-END*)

let read_private_key ?id prov =
  let read id =
    let base = prov.Conex_provider.name in
    let fn = private_key_path base id in
    if Conex_persistency.exists fn then
      match Conex_persistency.read_file fn with
      | Error e -> Error (`Msg e)
      | Ok key ->
        match String.cuts ';' key with
        | [ alg ; data ; created ] ->
          (match Key.string_to_alg alg, Uint.of_string created with
           | Some alg, Some ts -> Ok (id, `Priv (alg, data, ts))
           | _ -> Error (`Msg "couldn't parse private key type and created"))
        | _ -> Error (`Msg "couldn't parse private key")
    else
      Error (`NotFound id)
  in
  match id with
  | Some x -> read x
  | None -> match private_keys prov with
    | Ok [x] -> read x
    | Ok [] -> Error `NoPrivateKey
    | Ok xs -> Error (`MultiplePrivateKeys xs)
    | Error m -> Error (`Msg m)

module type S = sig
  val generate : ?bits:int -> Uint.t -> unit -> Key.priv

  val pub_of_priv : Key.priv -> (Key.t, string) result

  val sign : Uint.t -> Author.t -> Key.priv -> (Author.t, string) result
end

module Make (C : SIGN) = struct
  let generate ?bits time () =
    let key = C.generate_rsa ?bits () in
    `Priv (`RSA, key, time)

  let pub_of_priv key = match key with
    | `Priv (`RSA, key, created) ->
      C.pub_of_priv_rsa key >>= fun pub ->
      Ok (`RSA, pub, created)

  let sign now idx priv =
    let idx, _overflow = Author.prep_sig idx in
    let data = Wire.to_string (Author.wire_raw idx)
    and id = idx.Author.name
    in
    let hdr = `RSA_PSS_SHA256, now in
    let data = Wire.to_string (Signature.wire id hdr data) in
    (match priv with
     | `Priv (`RSA, key, _) -> C.sign_rsa_pss ~key data) >>= fun signature ->
    Ok (Author.add_sig idx (hdr, signature))
end
