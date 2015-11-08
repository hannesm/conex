open Core

type priv =
  | RSA_priv of Nocrypto.Rsa.priv

let pub_of_priv = function
  | RSA_priv k ->
     let pub = Nocrypto.Rsa.pub_of_priv k in
     let pem = X509.Encoding.Pem.Public_key.to_pem_cstruct1 (`RSA pub) in
     let str = Cstruct.to_string pem in
     Publickey.decode_key str

let generate () = RSA_priv (Nocrypto.Rsa.generate 2048)

let decode_priv data =
  match X509.Encoding.Pem.Private_key.of_pem_cstruct (Cstruct.of_string data) with
  | [ `RSA priv ] -> RSA_priv priv
  | _ -> invalid_arg "invalid private key"

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

let primitive_sign algorithm priv data =
  let cs = Cstruct.of_string data in
  let signature =
    match priv, algorithm with
    | RSA_priv key, `RSA_PSS -> Pss_sha256.sign ~key cs
    | RSA_priv key, `RSA_PKCS ->
       let data = Nocrypto.Hash.SHA256.digest cs in
       Nocrypto.Rsa.PKCS1.sig_encode ~key data
  in
  let b64 = Nocrypto.Base64.encode signature in
  Cstruct.to_string b64

let sign ?(algorithm = `RSA_PSS) id priv data =
  let data = Signature.extend_data data algorithm id in
  let sigval = primitive_sign algorithm priv data in
  (id, algorithm, sigval)



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
    (let backup = Filename.concat private_dir (private_filename repo (id ^ ".bak")) in
     if Persistency.exists backup then
       invalid_arg "backup already exists"
     else
       Persistency.rename filename backup) ;
  let data = encode_priv key in
  if not (Sys.is_directory private_dir) then
    Unix.mkdir private_dir 0o700 ;
  Persistency.write_file ~mode:0o400 filename data

let read_private_key ?id repo =
  let read id =
    let fn = Filename.concat private_dir (private_filename repo id) in
    if Persistency.exists fn then
      let key = Persistency.read_file fn in
      Some (id, decode_priv key)
    else
      None
  in
  match id with
  | Some x -> read x
  | None -> match all_private_keys repo with
            | [x] -> read x
            | [] -> None
            | _ -> None
