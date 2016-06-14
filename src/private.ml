open Core

type priv =
  | RSA_priv of Nocrypto.Rsa.priv

let pub_of_priv = function
  | RSA_priv k ->
     let pub = Nocrypto.Rsa.pub_of_priv k in
     let pem = X509.Encoding.Pem.Public_key.to_pem_cstruct1 (`RSA pub) in
     let str = Cstruct.to_string pem in
     match Publickey.decode_key str with
     | Some pk -> pk
     | None -> invalid_arg "X509 public key decoding and encoding not identity"

let generate ?(bits = 2048) () = RSA_priv (Nocrypto.Rsa.generate bits)

let decode_priv data =
  match X509.Encoding.Pem.Private_key.of_pem_cstruct (Cstruct.of_string data) with
  | [ `RSA priv ] -> Some (RSA_priv priv)
  | _ -> None

let encode_priv = function
  | RSA_priv priv ->
     let pem = X509.Encoding.Pem.Private_key.to_pem_cstruct1 (`RSA priv) in
     Cstruct.to_string pem

let pp_priv ppf p =
  match p with
  | RSA_priv pr ->
     let len = Nocrypto.Rsa.pub_bits (Nocrypto.Rsa.pub_of_priv pr) in
     Format.fprintf ppf "private %d bit RSA key@ %s" len (encode_priv p)

module Pss_sha256 = Nocrypto.Rsa.PSS (Nocrypto.Hash.SHA256)

let primitive_sign priv data =
  let cs = Cstruct.of_string data in
  let signature =
    match priv with
    | RSA_priv key -> Pss_sha256.sign ~key cs
  in
  let b64 = Nocrypto.Base64.encode signature in
  Cstruct.to_string b64

let sign id priv kind data =
  let data = Signature.extend_data data id kind in
  let sigval = primitive_sign priv data in
  (id, sigval)



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
  let data = encode_priv key in
  if not (Sys.is_directory private_dir) then
    Unix.mkdir private_dir 0o700 ;
  Persistency.write_file ~mode:0o400 filename data

type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list ]
let pp_err ppf = function
  | `NotFound x -> Format.fprintf ppf "couldn't find private key %s" x
  | `NoPrivateKey -> Format.pp_print_string ppf "no private key found"
  | `MultiplePrivateKeys keys -> Format.fprintf ppf "multiple private keys found %s" (String.concat ", " keys)

let read_private_key ?id repo =
  let read id =
    let fn = Filename.concat private_dir (private_filename repo id) in
    if Persistency.exists fn then
      let key = Persistency.read_file fn in
      match decode_priv key with
      | Some k -> Ok (id, k)
      | None -> Error `NoPrivateKey
    else
      Error (`NotFound id)
  in
  match id with
  | Some x -> read x
  | None -> match all_private_keys repo with
            | [x] -> read x
            | [] -> Error `NoPrivateKey
            | xs -> Error (`MultiplePrivateKeys xs)
