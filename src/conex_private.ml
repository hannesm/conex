open Conex_result
open Conex_core
open Conex_utils
open Conex_resource

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
       Ok (option acc (fun s -> s :: acc) (is_private s)))
    (Ok [])
    files

let private_key_path path id =
  let filename =
    let els = string_to_path path @ [id ; "private"] in
    String.concat "." els
  in
  "/" ^ path_to_string (string_to_path private_dir @ [ filename ])

let sign_index idx priv =
  let idx, _overflow = Index.prep_sig idx in
  let data = Conex_data.encode (Index.wire_resources idx)
  and created = Uint.of_float (Unix.time ())
  and signame = idx.Index.name
  in
  let hdr = { created ; sigtyp = `RSA_PSS_SHA256 ; signame } in
  let data = extend_sig hdr data in
  Conex_nocrypto.sign priv data >>= fun signature ->
  Ok (Index.add_sig idx (hdr, signature))

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
    let data = match key with `RSA_priv k -> k in
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
      | Ok key -> Ok (id, `RSA_priv key)
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
