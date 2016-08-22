open Core

module S = Set.Make(String)

module SM = Map.Make(String)
type valid = (name * resource * identifier list) SM.t

type t = {
  store : Keystore.t ;
  quorum : int ;
  data : Provider.t ;
  valid : valid ;
}

type ok = [ `Identifier of identifier | `Quorum of identifier list | `Both of identifier * identifier list ]
let pp_ok ppf = function
  | `Identifier id -> Format.fprintf ppf "id %s" id
  | `Both (id, js) -> Format.fprintf ppf "id %s and quorum %s" id (String.concat ", " js)
  | `Quorum js -> Format.fprintf ppf "quorum %s" (String.concat ", " js)

type res = (ok, error) result

let pp_res ppf = function
  | Ok ok -> pp_ok ppf ok
  | Error e -> pp_error ppf e

let repository ?(store = Keystore.empty) ?(quorum = 3) data =
  { store ; quorum ; data ; valid = SM.empty }

let quorum r = r.quorum

let provider t = t.data

let change_provider t data = { t with data }

let add_trusted_key repo key =
  let store = Keystore.add repo.store key in
  { repo with store }

let add_csums repo janitor rs =
  let add_csum t (name, resource, hash) =
    try
      let (n, r, ids) = SM.find hash t in
      (* TODO: remove asserts here, deal with errors! *)
      assert (n = name) ; assert (resource_equal r resource) ;
      SM.add hash (n, r, janitor :: ids) t
    with Not_found -> SM.add hash (name, resource, [janitor]) t
  in
  let valid = List.fold_left add_csum repo.valid rs in
  { repo with valid }

let has_quorum id repo resource data =
  let csum = digest data in
  let (n, r, js) =
    if SM.mem csum repo.valid then
      SM.find csum repo.valid
    else
      (id, resource, [])
  in
  (* TODO: define equality on names and ids? *)
  match n = id, resource_equal r resource, List.length js >= repo.quorum with
  | true, true, true -> Ok (`Quorum js)
  | _ -> Error (`InsufficientQuorum (id, js))

let verify_data repo authorised_ids resource data signatures =
  let ver = Keystore.verify repo.store resource data in
  let ok, errs =
    List.fold_left (fun (b, err) (id, s) ->
        if b = None && List.mem id authorised_ids then
          match ver (id, s) with
          | Ok id -> (Some id, err)
          | Error e -> (None, e :: err)
        else
          (b, `NotAuthorised id :: err))
      (None, [])
      signatures
  in
  (* TODO: revise return type *)
  match ok, errs with
  | None, e::_ -> Error e
  | None, [] -> Error (`NotAuthorised "bla")
  | Some x, _ -> Ok (`Identifier x)

let verify_key repo key =
  let id = key.Publickey.keyid
  and raw = Data.publickey_raw key
  and sigs = key.Publickey.signatures
  in
  verify_data (add_trusted_key repo key) [id] `PublicKey raw sigs >>= fun (`Identifier id) ->
  has_quorum id repo `PublicKey raw >>= fun (`Quorum js) ->
  Ok (`Both (id, js))

let verify_authorisation repo ?authorised auth =
  let raw = Data.authorisation_raw auth
  and signatures = auth.Authorisation.signatures
  in
  let valid = Utils.option auth.Authorisation.authorised (fun x -> x) authorised in
  verify_data repo valid `Authorisation raw signatures <<|>>
    has_quorum auth.Authorisation.name repo `Authorisation raw

let verify_checksum repo a cs =
  let raw = Data.checksums_raw cs
  and signatures = cs.Checksum.signatures
  in
  let nam = Layout.authorisation_of_item cs.Checksum.name in
  guard (nam = a.Authorisation.name) (`InvalidAuthorisation (a.Authorisation.name, cs.Checksum.name)) >>= fun () ->
  verify_data repo a.Authorisation.authorised `Checksum raw signatures <<|>>
    has_quorum cs.Checksum.name repo `Checksum raw

let verify_releases repo a r =
  let raw = Data.releases_raw r
  and signatures = r.Releases.signatures
  in
  guard (r.Releases.name = a.Authorisation.name) (`InvalidReleases (a.Authorisation.name, r.Releases.name)) >>= fun () ->
  verify_data repo a.Authorisation.authorised `Releases raw signatures <<|>>
    has_quorum r.Releases.name repo `Releases raw

let verify_janitorindex repo ji =
  let raw = Data.janitorindex_raw ji
  and signatures = ji.Janitorindex.signatures
  in
  verify_data repo [ji.Janitorindex.identifier] `JanitorIndex raw signatures

type r_err = [ `NotFound of string | `NameMismatch of string * string ]

let pp_r_err ppf = function
  | `NotFound x -> Format.fprintf ppf "%s was not found in repository" x
  | `NameMismatch (should, is) -> Format.fprintf ppf "%s is named %s" should is

type 'a r_res = ('a, r_err) result

let read_key repo keyid =
  match repo.data.Provider.read (Layout.key_path keyid) with
  | Error _ -> Error (`NotFound keyid)
  | Ok data ->
    let pubkey = Data.data_to_publickey (Data.parse data) in
    if pubkey.Publickey.keyid <> keyid then
      Error (`NameMismatch (keyid, pubkey.Publickey.keyid))
    else
      Ok pubkey

let write_key repo key =
  let data = Data.publickey_to_data key in
  repo.data.Provider.write
    (Layout.key_path key.Publickey.keyid)
    (Data.normalise data)

let all_keyids repo = Layout.keys repo.data

let all_janitors repo = Layout.janitors repo.data

let read_janitorindex repo name =
  match repo.data.Provider.read (Layout.janitorindex_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    let r = Data.data_to_janitorindex (Data.parse data) in
    if r.Janitorindex.identifier <> name then
      Error (`NameMismatch (name, r.Janitorindex.identifier))
    else
      Ok r

let write_janitorindex repo j =
  let data = Data.janitorindex_to_data j in
  let name = Layout.janitorindex_path j.Janitorindex.identifier in
  repo.data.Provider.write name (Data.normalise data)

let read_authorisation repo name =
  match repo.data.Provider.read (Layout.authorisation_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    let auth = Data.data_to_authorisation (Data.parse data) in
    if auth.Authorisation.name <> name then
      Error (`NameMismatch (name, auth.Authorisation.name))
    else
      Ok auth

let write_authorisation repo a =
  let data = Data.authorisation_to_data a in
  repo.data.Provider.write
    (Layout.authorisation_path a.Authorisation.name)
    (Data.normalise data)

let all_authorisations repo = Layout.authorisations repo.data

let read_releases repo name =
  match repo.data.Provider.read (Layout.releases_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    let r = Data.data_to_releases (Data.parse data) in
    if r.Releases.name <> name then
      Error (`NameMismatch (name, r.Releases.name))
    else
      Ok r

let write_releases repo r =
  let data = Data.releases_to_data r in
  let name = Layout.releases_path r.Releases.name in
  repo.data.Provider.write name (Data.normalise data)

let read_checksum repo name =
  match repo.data.Provider.read (Layout.checksum_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    let csum = Data.data_to_checksums (Data.parse data) in
    if csum.Checksum.name <> name then
      Error (`NameMismatch (name, csum.Checksum.name))
    else
      Ok csum

let write_checksum repo csum =
  let data = Data.checksums_to_data csum in
  let name = Layout.checksum_path csum.Checksum.name in
  repo.data.Provider.write name (Data.normalise data)

let compute_checksum repo name =
  match repo.data.Provider.file_type (Layout.checksum_dir name) with
  | Error _ -> Error (`NotFound name)
  | Ok File -> Error (`NotFound name) (* more precise? *)
  | Ok Directory ->
    let files = Layout.checksum_files repo.data name in
    let d = Layout.checksum_dir name in
    let datas =
      foldM
        (fun acc f ->
           match repo.data.Provider.read (d@f) with
           | Error _ -> Error (`NotFound (path_to_string (d@f)))
           | Ok data -> Ok (data :: acc))
        []
        files
    in
    let names = List.map path_to_string files in
    match datas with
    | Ok datas ->
      let csums = List.map2 Checksum.checksum names (List.rev datas) in
      Ok (Checksum.checksums name csums)
    | Error e -> Error e

let maybe_load repo ids =
  List.fold_left
    (fun repo id ->
       match read_key repo id with
      | Error _ -> invalid_arg ("error while loading key " ^ id)
      | Ok key ->
        match verify_key repo key with
        | Ok _ -> add_trusted_key repo key
        | Error e -> pp_error Format.std_formatter e ; repo)
    repo ids

(* TODO: invalid_arg are bad! *)
let load_keys ?(verify = false) repo ids =
  if verify then
    maybe_load repo ids
  else
    List.fold_left
      (fun repo id ->
         match read_key repo id with
         | Error _ -> invalid_arg ("could not find key " ^ id)
         | Ok k -> add_trusted_key repo k)
      repo ids

let load_janitor ?(verify = false) repo janitor =
  match read_janitorindex repo janitor with
  | Ok ji ->
    if verify then
      let repo = maybe_load repo [janitor] in
      match verify_janitorindex repo ji with
      | Ok _ -> add_csums repo janitor ji.Janitorindex.resources
      | Error _ -> invalid_arg "verification failed"
    else
      add_csums repo janitor ji.Janitorindex.resources
  | Error _ -> invalid_arg "unlikely to happen"

let load_janitors ?(verify = false) repo =
  let all = all_janitors repo in
  List.fold_left (load_janitor ~verify) repo all
