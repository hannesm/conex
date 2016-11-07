open Conex_core

let sign_index idx priv =
  let data = Data.index_to_string idx
  and now = Int64.of_float (Unix.time ())
  and id = idx.Conex_resource.Index.identifier
  in
  let data = Conex_resource.Signature.extend_data data id now in
  Conex_nocrypto.sign priv data >>= fun signature ->
  Ok (Conex_resource.Index.add_sig idx (id, now, signature))

let private_dir = Filename.concat (Sys.getenv "HOME") ".conex"

let private_suffix = "private"

let private_filename repo id =
  String.concat "." (string_to_path (Repository.provider repo).Provider.name @ [id ; private_suffix])

let all_private_keys repo =
  let is_private s =
    if Strhelper.is_suffix ~suffix:private_suffix s then
      let p = string_to_path (Repository.provider repo).Provider.name in
      match List.rev (Strhelper.cuts '.' s) with
      | _::id::path when p = List.rev path -> Some id
      | _ -> None
    else
      None
  in
  if Persistency.exists private_dir then
    List.fold_left
      (fun acc s -> Utils.option acc (fun s -> s :: acc) (is_private s))
      []
      (Persistency.collect_dir private_dir)
  else
    []

let write_private_key repo id key =
  let filename = Filename.concat private_dir (private_filename repo id) in
  if Persistency.exists filename then
    (let ts =
       let open Unix in
       let t = gmtime (stat filename).st_mtime in
       Printf.sprintf "%4d%2d%2d%2d%2d%2d" (t.tm_year + 1900) (succ t.tm_mon)
         t.tm_mday t.tm_hour t.tm_min t.tm_sec
     in
     let backfn = String.concat "." [ id ; ts ; "bak" ] in
     let backup = Filename.concat private_dir (private_filename repo backfn) in
     let rec inc n =
       let nam = backup ^ "." ^ string_of_int n in
       if Persistency.exists nam then
         inc (succ n)
       else
         nam
     in
     let backup =
       if Persistency.exists backup then
         inc 0
       else
         backup
     in
     Persistency.rename filename backup) ;
  if not (Persistency.exists private_dir) then
    Unix.mkdir private_dir 0o700 ;
  if not (Sys.is_directory private_dir) then
    invalid_arg (private_dir ^ " is not a directory!") ;
  let data = match key with `Priv k -> k in
  Persistency.write_file ~mode:0o400 filename data

type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list ]

(*BISECT-IGNORE-BEGIN*)
let pp_err ppf = function
  | `NotFound x -> Format.fprintf ppf "couldn't find private key %s" x
  | `NoPrivateKey -> Format.pp_print_string ppf "no private key found"
  | `MultiplePrivateKeys keys -> Format.fprintf ppf "multiple private keys found %s" (String.concat ", " keys)
(*BISECT-IGNORE-END*)

let read_private_key ?id repo =
  let read id =
    let fn = Filename.concat private_dir (private_filename repo id) in
    if Persistency.exists fn then
      let key = Persistency.read_file fn in
      Ok (id, `Priv key)
    else
      Error (`NotFound id)
  in
  match id with
  | Some x -> read x
  | None -> match all_private_keys repo with
            | [x] -> read x
            | [] -> Error `NoPrivateKey
            | xs -> Error (`MultiplePrivateKeys xs)
