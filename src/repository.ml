open Core

module SM = Map.Make(String)
type valid_resources = (name * resource * S.t) SM.t

type teams = S.t SM.t

type t = {
  store : Keystore.t ;
  quorum : int ;
  data : Provider.t ;
  valid : valid_resources ;
  teams : teams ;
}

let repository ?(store = Keystore.empty) ?(quorum = 3) data =
  { store ; quorum ; data ; valid = SM.empty ; teams = SM.empty }

let quorum r = r.quorum

let provider r = r.data

let teams r = r.teams

let team r id = try SM.find id r.teams with Not_found -> S.empty

let janitors r = team r "janitors"

let valid r digest =
  try Some (SM.find digest r.valid) with Not_found -> None

let change_provider t data = { t with data }

let add_trusted_key repo key =
  let store = Keystore.add repo.store key in
  { repo with store }

let add_team repo team =
  { repo with teams = SM.add team.Team.name team.Team.members repo.teams }

let verify_index repo idx =
  let aid = idx.Index.identifier in
  match idx.Index.signature with
  | Some (id, s) when id_equal id aid -> Keystore.verify repo.store (Data.index_to_string idx) (id, s)
  | Some (id, _) -> Error (`NotAuthorised (aid, id))
  | None -> Error (`NoSignature aid)

(*BISECT-IGNORE-BEGIN*)
let pp_ok ppf = function
  | `Signed id -> Format.fprintf ppf "id %s" id
  | `Both (id, js) -> Format.fprintf ppf "id %s and quorum %s" id (String.concat ", " (S.elements js))
  | `Quorum js -> Format.fprintf ppf "quorum %s" (String.concat ", " (S.elements js))
(*BISECT-IGNORE-END*)

type base_error = [
  | `InvalidName of name * name
  | `InvalidResource of name * resource * resource
  | `NotSigned of name * resource * S.t
]

(*BISECT-IGNORE-BEGIN*)
let pp_cs ppf (a, b) =
  Format.fprintf ppf "have %a want %a@ " Checksum.pp_checksum a Checksum.pp_checksum b

let pp_error ppf = function
  | `InvalidName (w, h) -> Format.fprintf ppf "invalid name, looking for %a but got %a" pp_name w pp_name h
  | `InvalidResource (n, w, h) -> Format.fprintf ppf "invalid resource %a, looking for %a but got %a" pp_name n pp_resource w pp_resource h
  | `NotSigned (n, r, js) -> Format.fprintf ppf "missing signature on %a, a %a, quorum not reached (valid %a)" pp_name n pp_resource r (pp_list pp_id) (S.elements js)
  | `InsufficientQuorum (name, goods) -> Format.fprintf ppf "quorum for %a not reached (valid: %a)" pp_name name (pp_list pp_id) (S.elements goods)
  | `MissingSignature id -> Format.fprintf ppf "missing self-signature on public key %a" pp_id id
  | `AuthRelMismatch (a, r) -> Format.fprintf ppf "the package name in the authorisation %a does not match the one in releases %a" pp_name a pp_name r
  | `InvalidReleases (n, h, w) when S.equal h S.empty -> Format.fprintf ppf "several releases of %a are missing on disk: %a" pp_name n (pp_list pp_name) (S.elements w)
  | `InvalidReleases (n, h, w) when S.equal w S.empty -> Format.fprintf ppf "several releases of %a are not in the signed releases file %a" pp_name n (pp_list pp_name) (S.elements h)
  | `InvalidReleases (n, h, w) -> Format.fprintf ppf "the releases file of %a diverges: %a are on disk, but not in the file, %a are in the file, but not on disk" pp_name n (pp_list pp_name) (S.elements h) (pp_list pp_name) (S.elements w)
  | `NotInReleases (c, rs) -> Format.fprintf ppf "the package name %a is not in the set of released versions %a" pp_name c (pp_list pp_name) (S.elements rs)
  | `FileNotFound n -> Format.fprintf ppf "couldn't find file %a" pp_name n
  | `NotADirectory n -> Format.fprintf ppf "expected %a to be a directory, but it is a file" pp_name n
  | `ChecksumsDiff (n, miss, too, diffs) -> Format.fprintf ppf "checksums for %a differ, missing on disk: %a, missing in checksums file: %a, checksums differ: %a" pp_name n (pp_list pp_name) miss (pp_list pp_name) too (pp_list pp_cs) diffs
(*BISECT-IGNORE-END*)

let expand_owner r os =
  S.fold
    (fun id s ->
       if SM.mem id r.teams then S.union s (SM.find id r.teams) else S.add id s)
    os
    S.empty

let verify_resource repo owners name resource data =
  let csum = digest data in
  let n, r, s =
    if SM.mem csum repo.valid then
      SM.find csum repo.valid
    else
      (name, resource, S.empty)
  in
  let js = S.filter (fun j -> S.mem j (janitors repo)) s in
  let owners = expand_owner repo owners in
  let id a = S.choose (S.filter (fun a -> S.mem a s) a) in
  match
    name_equal n name,
    resource_equal r resource,
    S.exists (fun a -> S.mem a s) owners,
    S.cardinal js >= repo.quorum
  with
  | false, _    , _    , _     -> Error (`InvalidName (name, n))
  | true , false, _    , _     -> Error (`InvalidResource (name, resource, r))
  | true , true , false, false -> Error (`NotSigned (n, r, js))
  | true , true , false, true  -> Ok (`Quorum js)
  | true , true , true , false -> Ok (`IdNoQuorum (id owners, js))
  | true , true , true , true  -> Ok (`Both (id owners, js))

let verify_key repo key =
  let id = key.Publickey.keyid in
  verify_resource repo (S.singleton id) id `PublicKey (Data.publickey_to_string key) >>= function
  | `Both b -> Ok (`Both b)
  | `IdNoQuorum (id, js) -> Error (`InsufficientQuorum (id, js))
  | `Quorum js when key.Publickey.key = None -> Ok (`Quorum js)
  | `Quorum _ -> Error (`MissingSignature id)

let verify_team repo team =
  let id = team.Team.name in
  match verify_resource repo S.empty id `Team (Data.team_to_string team) with
  | Error (`NotSigned (n, _, js)) -> Error (`InsufficientQuorum (n, js))
  | Error e -> Error e
  | Ok (`Quorum js) -> Ok (`Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"
  (* should verify that all members are on disk (and are keys, not teams, and no index files!!!) *)

let verify_authorisation repo auth =
  let name = auth.Authorisation.name in
  match verify_resource repo S.empty name `Authorisation (Data.authorisation_to_string auth) with
  | Error (`NotSigned (n, _, js)) -> Error (`InsufficientQuorum (n, js))
  | Error e -> Error e
  | Ok (`Quorum js) -> Ok (`Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"

let ensure_releases repo r =
  let dirs = Layout.items repo.data r.Releases.name in
  let dirs = S.of_list dirs in
  let rels = r.Releases.releases in
  if S.equal rels dirs then
    Ok ()
  else
    let have = S.diff dirs rels
    and want = S.diff rels dirs
    in
    Error (have, want)

let verify_releases repo a r =
  guard (name_equal a.Authorisation.name r.Releases.name)
    (`AuthRelMismatch (a.Authorisation.name, r.Releases.name)) >>= fun () ->
  verify_resource repo a.Authorisation.authorised r.Releases.name `Releases (Data.releases_to_string r) >>= fun res ->
  match ensure_releases repo r with
  | Error (h, w) -> Error (`InvalidReleases (r.Releases.name, h, w))
  | Ok () -> match res with
    | `Both b -> Ok (`Both b)
    | `Quorum js -> Ok (`Quorum js)
    | `IdNoQuorum (id, _) -> Ok (`Signed id)

let compute_checksum repo name =
  match repo.data.Provider.file_type (Layout.checksum_dir name) with
  | Error _ -> Error (`FileNotFound name)
  | Ok File -> Error (`NotADirectory name)
  | Ok Directory ->
    let fs = Layout.checksum_files repo.data name in
    let d = Layout.checksum_dir name in
    foldM (fun acc f ->
        match repo.data.Provider.read (d@f) with
        | Error _ -> Error (`FileNotFound (path_to_string (d@f)))
        | Ok data -> Ok (data :: acc)) [] fs >>= fun ds ->
    let r = List.(map2 Checksum.checksum (map path_to_string fs) (rev ds)) in
    Ok (Checksum.checksums name r)

let verify_checksum repo a r cs =
  guard (name_equal a.Authorisation.name r.Releases.name)
    (`AuthRelMismatch (a.Authorisation.name, r.Releases.name)) >>= fun () ->
  guard (S.mem cs.Checksum.name r.Releases.releases)
    (`NotInReleases (cs.Checksum.name, r.Releases.releases)) >>= fun () ->
  verify_resource repo a.Authorisation.authorised cs.Checksum.name `Checksum (Data.checksums_to_string cs) >>= fun r ->
  let name = cs.Checksum.name in
  compute_checksum repo name >>= fun css ->
  match Checksum.compare_checksums cs css with
  | Error (`InvalidName (a, b)) -> Error (`InvalidName (a, b)) (* this can never happen since name is taken from cs *)
  | Error (`ChecksumsDiff (a, b, c, d)) -> Error (`ChecksumsDiff (a, b, c, d))
  | Ok () ->
    match r with
    | `Both b -> Ok (`Both b)
    | `Quorum js -> Ok (`Quorum js)
    | `IdNoQuorum (id, _) -> Ok (`Signed id)

type r_err = [ `NotFound of string | `ParseError of name * string | `NameMismatch of string * string ]

(*BISECT-IGNORE-BEGIN*)
let pp_r_err ppf = function
  | `NotFound x -> Format.fprintf ppf "%s was not found in repository" x
  | `ParseError (n, e) -> Format.fprintf ppf "parse error while parsing %a: %s" pp_name n e
  | `NameMismatch (should, is) -> Format.fprintf ppf "%s is named %s" should is
(*BISECT-IGNORE-END*)

type 'a r_res = ('a, r_err) result

let read_key repo keyid =
  match repo.data.Provider.read (Layout.key_path keyid) with
  | Error _ -> Error (`NotFound keyid)
  | Ok data ->
    match Data.string_to_publickey data with
    | Error p -> Error (`ParseError (keyid, p))
    | Ok pubkey ->
      if id_equal pubkey.Publickey.keyid keyid then
        Ok pubkey
      else
        Error (`NameMismatch (keyid, pubkey.Publickey.keyid))

let write_key repo key =
  let id = key.Publickey.keyid in
  repo.data.Provider.write (Layout.key_path id) (Data.publickey_to_string key)

let read_team repo name =
  match repo.data.Provider.read (Layout.key_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    match Data.string_to_team data with
    | Error p -> Error (`ParseError (name, p))
    | Ok team ->
      if id_equal team.Team.name name then
        Ok team
      else
        Error (`NameMismatch (name, team.Team.name))

let write_team repo t =
  let id = t.Team.name in
  repo.data.Provider.write (Layout.key_path id) (Data.team_to_string t)

let read_id repo id =
  match read_key repo id with
  | Ok key -> Ok (`Key key)
  | Error (`NameMismatch (a, b)) -> Error (`NameMismatch (a, b))
  | Error _ -> match read_team repo id with
    | Ok team -> Ok (`Team team)
    | Error e -> Error e

let all_ids repo = S.of_list (Layout.ids repo.data)

let read_index repo name =
  match repo.data.Provider.read (Layout.index_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    match Data.string_to_index data with
    | Error p -> Error (`ParseError (name, p))
    | Ok i ->
      if id_equal i.Index.identifier name then
        Ok i
      else
        Error (`NameMismatch (name, i.Index.identifier))

let write_index repo i =
  let name = Layout.index_path i.Index.identifier in
  repo.data.Provider.write name (Data.index_to_string i)

let read_authorisation repo name =
  match repo.data.Provider.read (Layout.authorisation_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    match Data.string_to_authorisation data with
    | Error p -> Error (`ParseError (name, p))
    | Ok auth ->
      if name_equal auth.Authorisation.name name then
        Ok auth
      else
        Error (`NameMismatch (name, auth.Authorisation.name))

let write_authorisation repo a =
  repo.data.Provider.write
    (Layout.authorisation_path a.Authorisation.name)
    (Data.authorisation_to_string a)

let all_authorisations repo = S.of_list (Layout.authorisations repo.data)

let read_releases repo name =
  match repo.data.Provider.read (Layout.releases_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    match Data.string_to_releases data with
    | Error p -> Error (`ParseError (name, p))
    | Ok r ->
      if name_equal r.Releases.name name then
        Ok r
      else
        Error (`NameMismatch (name, r.Releases.name))

let write_releases repo r =
  let name = Layout.releases_path r.Releases.name in
  repo.data.Provider.write name (Data.releases_to_string r)

let read_checksum repo name =
  match repo.data.Provider.read (Layout.checksum_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    match Data.string_to_checksums data with
    | Error p -> Error (`ParseError (name, p))
    | Ok csum ->
      if name_equal csum.Checksum.name name then
        Ok csum
      else
        Error (`NameMismatch (name, csum.Checksum.name))

let write_checksum repo csum =
  let name = Layout.checksum_path csum.Checksum.name in
  repo.data.Provider.write name (Data.checksums_to_string csum)

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
  add_csums r idx.Index.identifier idx.Index.resources
