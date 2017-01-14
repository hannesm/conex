open Conex_result
open Conex_core
open Conex_utils

let sign_index idx priv =
  let idx, _overflow = Conex_resource.Index.prep_sig idx in
  let data = Conex_data.encode (Conex_data_persistency.index_to_t idx)
  and now = Uint.of_float (Unix.time ())
  and id = idx.Conex_resource.Index.name
  in
  let data = Conex_resource.Signature.extend_data data id now in
  Conex_nocrypto.sign priv data >>= fun signature ->
  Ok (Conex_resource.Index.add_sig idx (id, now, signature))

let write_private_key repo id key =
  let base = (Conex_repository.provider repo).Provider.name in
  let filename = Conex_opam_layout.private_key_path base id in
  if Conex_persistency.exists filename then
    (let ts =
       let open Unix in
       let t = gmtime (stat filename).st_mtime in
       Printf.sprintf "%4d%2d%2d%2d%2d%2d" (t.tm_year + 1900) (succ t.tm_mon)
         t.tm_mday t.tm_hour t.tm_min t.tm_sec
     in
     let backfn = String.concat "." [ id ; ts ] in
     let backup = Conex_opam_layout.private_key_path base backfn in
     let rec inc n =
       let nam = backup ^ "." ^ string_of_int n in
       if Conex_persistency.exists nam then inc (succ n) else nam
     in
     let backup = if Conex_persistency.exists backup then inc 0 else backup in
     Conex_persistency.rename filename backup) ;
  if not (Conex_persistency.exists Conex_opam_layout.private_dir) then
    Conex_persistency.mkdir ~mode:0o700 Conex_opam_layout.private_dir ;
  match Conex_persistency.file_type Conex_opam_layout.private_dir with
  | Some Directory ->
    let data = match key with `Priv k -> k in
    Conex_persistency.write_file ~mode:0o400 filename data
  | _ -> invalid_arg (Conex_opam_layout.private_dir ^ " is not a directory!")

type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list ]

(*BISECT-IGNORE-BEGIN*)
let pp_err ppf = function
  | `NotFound x -> Format.fprintf ppf "couldn't find private key %s" x
  | `NoPrivateKey -> Format.pp_print_string ppf "no private key found"
  | `MultiplePrivateKeys keys -> Format.fprintf ppf "multiple private keys found %s" (String.concat ", " keys)
(*BISECT-IGNORE-END*)

let read_private_key ?id repo =
  let read id =
    let base = (Conex_repository.provider repo).Provider.name in
    let fn = Conex_opam_layout.private_key_path base id in
    if Conex_persistency.exists fn then
      let key = Conex_persistency.read_file fn in
      Ok (id, `Priv key)
    else
      Error (`NotFound id)
  in
  match id with
  | Some x -> read x
  | None -> match Conex_opam_layout.private_keys (Conex_repository.provider repo) with
    | [x] -> read x
    | [] -> Error `NoPrivateKey
    | xs -> Error (`MultiplePrivateKeys xs)
