open Core

module SM = Map.Make(String)
type valid_resources = (name * resource * S.t) SM.t

type t = {
  store : Keystore.t ;
  quorum : int ;
  data : Provider.t ;
  valid : valid_resources ;
  janitors : S.t
}

type ok = [ `Identifier of identifier | `Quorum of S.t | `Both of identifier * S.t ]

(*BISECT-IGNORE-BEGIN*)
let pp_ok ppf = function
  | `Identifier id -> Format.fprintf ppf "id %s" id
  | `Both (id, js) -> Format.fprintf ppf "id %s and quorum %s" id (String.concat ", " (S.elements js))
  | `Quorum js -> Format.fprintf ppf "quorum %s" (String.concat ", " (S.elements js))
(*BISECT-IGNORE-END*)

type res = (ok, error) result

(*BISECT-IGNORE-BEGIN*)
let pp_res ppf = function
  | Ok ok -> pp_ok ppf ok
  | Error e -> pp_error ppf e
(*BISECT-IGNORE-END*)

let repository ?(store = Keystore.empty) ?(quorum = 3) data =
  { store ; quorum ; data ; valid = SM.empty ; janitors = S.empty }

let quorum r = r.quorum

let provider t = t.data

let change_provider t data = { t with data }

let add_trusted_key repo key =
  let store = Keystore.add repo.store key in
  let janitors = match key.Publickey.role with
    | `Janitor -> S.add key.Publickey.keyid repo.janitors
    | _ -> repo.janitors
  in
  { repo with store ; janitors }

let verify_index repo idx =
  let data = Data.index_raw idx
  and aid = idx.Index.identifier
  in
  match idx.Index.signature with
  | Some (id, s) when id_equal id aid -> Keystore.verify repo.store data (id, s)
  | Some (id, _) -> Error (`NotAuthorised (aid, id))
  | None -> Error (`NoSignature aid)

let verify_resource repo authorised name resource data =
  let csum = digest data in
  let n, r, s =
    if SM.mem csum repo.valid then
      SM.find csum repo.valid
    else
      (name, resource, S.empty)
  in
  let js = S.filter (fun j -> S.mem j repo.janitors) s in
  let id a = S.choose (S.filter (fun a -> S.mem a s) a) in
  match
    name_equal n name,
    resource_equal r resource,
    S.exists (fun a -> S.mem a s) authorised,
    S.cardinal js >= repo.quorum
  with
  | false, _    , _    , _     -> Error (`InvalidName (name, n))
  | true , false, _    , _     -> Error (`InvalidResource (resource, r))
  | true , true , false, false -> Error (`NotSigned (n, r, js))
  | true , true , false, true  -> Ok (`Quorum js)
  | true , true , true , false -> Ok (`NoQuorum (id authorised, js))
  | true , true , true , true  -> Ok (`Both (id authorised, js))

let verify_key repo key =
  let id = key.Publickey.keyid
  and raw = Data.publickey_raw key
  in
  verify_resource repo (S.singleton id) id `PublicKey raw >>= function
  | `Both b -> Ok (`Both b)
  | `NoQuorum (id, js) -> Error (`InsufficientQuorum (id, js))
  | `Quorum js when key.Publickey.key = None -> Ok (`Quorum js)
  | `Quorum _ -> Error (`MissingSignature id)

let verify_authorisation repo auth =
  let raw = Data.authorisation_raw auth
  and name = auth.Authorisation.name
  in
  match verify_resource repo S.empty name `Authorisation raw with
  | Error (`NotSigned (n, _, js)) -> Error (`InsufficientQuorum (n, js))
  | Error e -> Error e
  | Ok (`Quorum js) -> Ok (`Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`NoQuorum (_, js)) -> Error (`InsufficientQuorum (name, js))
  | Ok (`Both b) -> Ok (`Both b)

let verify_releases repo a r =
  guard (name_equal a.Authorisation.name r.Releases.name)
    (* XXX: maybe different tag? *)
    (`InvalidName (r.Releases.name, a.Authorisation.name)) >>= fun () ->
  let raw = Data.releases_raw r in
  verify_resource repo a.Authorisation.authorised r.Releases.name `Releases raw >>= function
  | `Both b -> Ok (`Both b)
  | `Quorum js -> Ok (`Quorum js)
  | `NoQuorum (id, _) -> Ok (`Identifier id)
  (* need to verify that package dir contains all mentioned releases! *)

let verify_checksum repo a r cs =
  (* XXX: different tag for each guard? *)
  guard (name_equal a.Authorisation.name r.Releases.name)
    (`InvalidName (r.Releases.name, a.Authorisation.name)) >>= fun () ->
  guard (List.mem cs.Checksum.name r.Releases.releases)
    (`InvalidName (r.Releases.name, cs.Checksum.name)) >>= fun () ->
  let raw = Data.checksums_raw cs in
  verify_resource repo a.Authorisation.authorised cs.Checksum.name `Checksum raw >>= function
  | `Both b -> Ok (`Both b)
  | `Quorum js -> Ok (`Quorum js)
  | `NoQuorum (id, _) -> Ok (`Identifier id)
  (* still need to verify checksums against the concrete files *)

type r_err = [ `NotFound of string | `NameMismatch of string * string ]

(*BISECT-IGNORE-BEGIN*)
let pp_r_err ppf = function
  | `NotFound x -> Format.fprintf ppf "%s was not found in repository" x
  | `NameMismatch (should, is) -> Format.fprintf ppf "%s is named %s" should is
(*BISECT-IGNORE-END*)

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

let read_index repo name =
  match repo.data.Provider.read (Layout.index_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    let r = Data.data_to_index (Data.parse data) in
    if r.Index.identifier <> name then
      Error (`NameMismatch (name, r.Index.identifier))
    else
      Ok r

let write_index repo j =
  let data = Data.index_to_data j in
  let name = Layout.index_path j.Index.identifier in
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

(* TODO: invalid_args are bad below!!! *)
let add_csums repo id rs =
  let add_csum t (name, resource, hash) =
    try
      let n, r, ids = SM.find hash t in
      (* TODO: remove asserts here, deal with errors! *)
      assert (name_equal n name) ; assert (resource_equal r resource) ;
      SM.add hash (n, r, S.add id ids) t
    with Not_found ->
      SM.add hash (name, resource, S.singleton id) t
  in
  let valid = List.fold_left add_csum repo.valid rs in
  { repo with valid }

let add_index r idx =
  (* XXX: replace with a convenience function : PK.t -> I.t -> or_error *)
  add_csums r idx.Index.identifier idx.Index.resources

let maybe_load repo ids =
  List.fold_left
    (fun repo id ->
       match read_key repo id with
      | Error _ -> invalid_arg ("error while loading key " ^ id)
      | Ok key ->
        (* XXX: might only be able to do it afterwards... (sig of key k is in index i) *)
        match verify_key repo key with
        | Ok _ -> add_trusted_key repo key
        | Error e -> pp_error Format.std_formatter e ; repo)
    repo ids

let load_keys ?(verify = false) repo ids =
  let repo =
    if verify then
      maybe_load repo ids
    else
      List.fold_left
        (fun repo id ->
           match read_key repo id with
           | Error _ -> invalid_arg ("could not find key " ^ id)
           | Ok k -> add_trusted_key repo k)
        repo ids
  in
  List.fold_left
    (fun repo id ->
       match read_index repo id with
       | Ok i ->
         begin match verify_index repo i with
           | Ok _ -> add_csums repo id i.Index.resources
           | Error _ -> invalid_arg "verification failed"
         end
       | Error _ -> invalid_arg "couldn't read index")
    repo ids

(* XXX: load janitor key as well! *)
let load_janitor ?(verify = false) repo janitor =
  match read_index repo janitor with
  | Ok ji ->
    let repo =
      if verify then
        let repo = maybe_load repo [janitor] in
        match verify_index repo ji with
        | Ok _ -> add_csums repo janitor ji.Index.resources
        | Error _ -> invalid_arg "verification failed"
      else
        add_csums repo janitor ji.Index.resources
    in
    { repo with janitors = S.add janitor repo.janitors }
  | Error _ -> invalid_arg "unlikely to happen, error reading index"

let load_janitors ?(verify = false) repo =
  let all = all_janitors repo in
  List.fold_left (load_janitor ~verify) repo all
