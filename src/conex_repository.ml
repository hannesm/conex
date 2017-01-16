open Conex_result
open Conex_core
open Conex_resource
open Conex_utils

type keystore = Publickey.t M.t

type valid_resources = (name * Uint.t * resource * S.t) M.t

type teams = S.t M.t

type t = {
  store : keystore ;
  quorum : int ;
  data : Provider.t ;
  valid : valid_resources ;
  teams : teams ;
}

let repository ?(store = M.empty) ?(quorum = 3) data =
  { store ; quorum ; data ; valid = M.empty ; teams = M.empty }

let quorum r = r.quorum

let provider r = r.data

let find_team r id = try Some (M.find id r.teams) with Not_found -> None

let find_id r email =
  M.fold (fun k v acc ->
      let contains =
        let f = function `Email e when e = email -> true | _ -> false in
        try Some (List.find f v.Publickey.accounts) with Not_found -> None
      in
      match contains, acc with
      | Some _, None -> Some k
      | None, None -> None
      | Some _, Some x -> Printf.printf "same mail used multiple times %s %s" k x ; Some x
      | None, Some x -> Some x)
    r.store None

let find_key t id = try Some (M.find id t.store) with Not_found -> None

let valid r digest =
  try Some (M.find digest r.valid) with Not_found -> None

let authorised r a id =
  let set = a.Authorisation.authorised in
  let is_member tid = S.mem id (match find_team r tid with None -> S.empty | Some s -> s) in
  S.mem id set || (S.exists is_member set)

let add_valid_resource repo id res =
  let open Index in
  let t = repo.valid in
  try
    let n, s, r, ids = M.find res.digest t in
    if not (name_equal n res.rname) then
      Error ("name not equal: " ^ n ^ " vs " ^ res.rname)
    else if not (resource_equal r res.resource) then
      Error ("resource not equal: " ^ resource_to_string r ^ " vs " ^ resource_to_string res.resource)
    else if not (s = res.size) then
      Error ("size not equal: " ^ Uint.to_string s ^ " vs " ^ Uint.to_string res.size)
    else
      Ok ({ repo with valid = M.add res.digest (n, s, r, S.add id ids) t })
  with Not_found ->
    Ok ({ repo with valid = M.add res.digest (res.rname, res.size, res.resource, S.singleton id) t})

let change_provider t data = { t with data }

let add_trusted_key repo key =
  let store = M.add key.Publickey.name key repo.store in
  { repo with store }

let remove_key repo id =
  let store = M.remove id repo.store in
  { repo with store }

let add_team repo team =
  { repo with teams = M.add team.Team.name team.Team.members repo.teams }

let remove_team repo id =
  { repo with teams = M.remove id repo.teams }

let verify pub data (id, ts, sigval) =
  let data = Signature.extend_data data id ts in
  match pub.Publickey.key with
  | None -> Error (`InvalidPublicKey id)
  | Some key ->
    match Conex_nocrypto.verify key data sigval with
    | Ok () -> Ok id
    | Error `InvalidBase64 -> Error (`InvalidBase64Encoding id)
    | Error `InvalidPubKey -> Error (`InvalidPublicKey id)
    | Error `InvalidSig -> Error (`InvalidSignature id)

let verify_ks store data (id, ts, signature) =
  if M.mem id store then
    let pub = M.find id store in
    verify pub data (id, ts, signature)
  else
    Error (`InvalidIdentifier id)

let verify_index repo idx =
  let aid = idx.Index.name in
  let to_consider, others =
    List.partition (fun (id, _, _) -> id_equal id aid) idx.Index.signatures
  in
  let verify = verify_ks repo.store (Conex_data.encode (Conex_data_persistency.index_to_t idx)) in
  let err = match others with
    | [] -> `NoSignature aid
    | (xid, _, _)::_ -> `NotAuthorised (aid, xid)
  in
  List.fold_left (fun r s -> match r, verify s with Ok x, _ -> Ok x | _, b -> b)
    (Error err) to_consider >>= fun id ->
  let r, warn = List.fold_left (fun (repo, warn) res ->
      match add_valid_resource repo id res with
      | Ok r -> r, warn
      | Error msg -> repo, msg :: warn)
      (repo, [])
      idx.Index.resources
  in
  let warn = warn @ List.map (fun r ->
      Format.(fprintf str_formatter "ignored queued %a" Index.pp_resource r) ;
      Format.flush_str_formatter ())
      idx.Index.queued
  in
  Ok (r, warn, id)

(*BISECT-IGNORE-BEGIN*)
let pp_ok ppf = function
  | `Signed id -> Format.fprintf ppf "ok by id %s" id
  | `Both (id, js) -> Format.fprintf ppf "ok by id %s and quorum %s" id (String.concat ", " (S.elements js))
  | `Quorum js -> Format.fprintf ppf "ok by quorum %s" (String.concat ", " (S.elements js))
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
  | `InvalidName (w, h) -> Format.fprintf ppf "invalid resource name, looking for %a but got %a" pp_name w pp_name h
  | `InvalidResource (n, w, h) -> Format.fprintf ppf "invalid resource type %a, looking for %a but got %a" pp_name n pp_resource w pp_resource h
  | `NotSigned (n, r, _) -> Format.fprintf ppf "unsigned %a %a" pp_resource r pp_name n
  | `InsufficientQuorum (name, r, goods) -> Format.fprintf ppf "unsigned %a %a (quorum not reached: %a)" pp_resource r pp_name name (pp_list pp_id) (S.elements goods)
  | `MissingSignature id -> Format.fprintf ppf "publickey %a: missing self-signature" pp_id id
  | `AuthRelMismatch (a, r) -> Format.fprintf ppf "package name in authorisation of %a is different from releases %a" pp_name a pp_name r
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
       if M.mem id r.teams then S.union s (M.find id r.teams) else S.add id s)
    os
    S.empty

let verify_resource repo owners name resource data =
  let csum = Conex_nocrypto.digest data in
  let n, _s, r, ids =
    if M.mem csum repo.valid then
      M.find csum repo.valid
    else
      (name, Uint.of_int (String.length data), resource, S.empty)
  in
  let janitors = match find_team repo "janitors" with None -> S.empty | Some s -> s in
  let js = S.filter (fun j -> S.mem j janitors) ids in
  let owners = expand_owner repo owners in
  let id a = S.choose (S.filter (fun a -> S.mem a ids) a) in
  match
    name_equal n name,
    resource_equal r resource,
    S.exists (fun a -> S.mem a ids) owners,
    S.cardinal js >= repo.quorum
  with
  | false, _    , _    , _     -> Error (`InvalidName (name, n))
  | true , false, _    , _     -> Error (`InvalidResource (name, resource, r))
  | true , true , false, false -> Error (`NotSigned (n, r, js))
  | true , true , false, true  -> Ok (`Quorum js)
  | true , true , true , false -> Ok (`IdNoQuorum (id owners, js))
  | true , true , true , true  -> Ok (`Both (id owners, js))

let verify_key repo key =
  let id = key.Publickey.name in
  verify_resource repo (S.singleton id) id `PublicKey (Conex_data.encode (Conex_data_persistency.publickey_to_t key)) >>= function
  | `Both b -> Ok (add_trusted_key repo key, `Both b)
  | `Quorum js when key.Publickey.key = None -> Ok (add_trusted_key repo key, `Quorum js)
  | `Quorum _ -> Error (`MissingSignature id)
  | `IdNoQuorum (id, js) -> Error (`InsufficientQuorum (id, `PublicKey, js))

let verify_team repo team =
  let id = team.Team.name in
  match verify_resource repo S.empty id `Team (Conex_data.encode (Conex_data_persistency.team_to_t team)) with
  | Error (`NotSigned (n, _, js)) -> Error (`InsufficientQuorum (n, `Team, js))
  | Error e -> Error e
  | Ok (`Quorum js) -> Ok (add_team repo team, `Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"
  (* should verify that all members are on disk (and are keys, not teams, and no index files!!!) *)

let verify_authorisation repo auth =
  let name = auth.Authorisation.name in
  match verify_resource repo S.empty name `Authorisation (Conex_data.encode (Conex_data_persistency.authorisation_to_t auth)) with
  | Error (`NotSigned (n, _, js)) -> Error (`InsufficientQuorum (n, `Authorisation, js))
  | Error e -> Error e
  | Ok (`Quorum js) -> Ok (`Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"

let ensure_releases repo r =
  let dirs = Conex_opam_layout.subitems repo.data r.Releases.name in
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
  verify_resource repo a.Authorisation.authorised r.Releases.name `Releases (Conex_data.encode (Conex_data_persistency.releases_to_t r)) >>= fun res ->
  match ensure_releases repo r with
  | Error (h, w) -> Error (`InvalidReleases (r.Releases.name, h, w))
  | Ok () -> match res with
    | `Both b -> Ok (`Both b)
    | `Quorum js -> Ok (`Quorum js)
    | `IdNoQuorum (id, _) -> Ok (`Signed id)

let compute_checksum repo name =
  match repo.data.Provider.file_type (Conex_opam_layout.checksum_dir name) with
  | Error _ -> Error (`FileNotFound name)
  | Ok File -> Error (`NotADirectory name)
  | Ok Directory ->
    let fs = Conex_opam_layout.checksum_files repo.data name in
    let d = Conex_opam_layout.checksum_dir name in
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
  verify_resource repo a.Authorisation.authorised cs.Checksum.name `Checksums (Conex_data.encode (Conex_data_persistency.checksums_to_t cs)) >>= fun r ->
  let name = cs.Checksum.name in
  compute_checksum repo name >>= fun css ->
  Checksum.compare_checksums cs css >>= fun () ->
  match r with
  | `Both b -> Ok (`Both b)
  | `Quorum js -> Ok (`Quorum js)
  | `IdNoQuorum (id, _) -> Ok (`Signed id)

type r_err = [ `NotFound of string * string | `ParseError of name * string | `NameMismatch of string * string ]

(*BISECT-IGNORE-BEGIN*)
let pp_r_err ppf = function
  | `NotFound (x, y) -> Format.fprintf ppf "%s %s was not found in repository" x y
  | `ParseError (n, e) -> Format.fprintf ppf "parse error while parsing %a: %s" pp_name n e
  | `NameMismatch (should, is) -> Format.fprintf ppf "%s is named %s" should is
(*BISECT-IGNORE-END*)

let read_key repo keyid =
  match repo.data.Provider.read (Conex_opam_layout.key_path keyid) with
  | Error _ -> Error (`NotFound ("key", keyid))
  | Ok data ->
    match Conex_data.decode data >>= Conex_data_persistency.t_to_publickey with
    | Error p -> Error (`ParseError (keyid, p))
    | Ok pubkey ->
      if id_equal pubkey.Publickey.name keyid then
        Ok pubkey
      else
        Error (`NameMismatch (keyid, pubkey.Publickey.name))

let write_key repo key =
  let id = key.Publickey.name in
  repo.data.Provider.write (Conex_opam_layout.key_path id) (Conex_data.encode (Conex_data_persistency.publickey_to_t key))

let read_team repo name =
  match repo.data.Provider.read (Conex_opam_layout.key_path name) with
  | Error _ -> Error (`NotFound ("team", name))
  | Ok data ->
    match Conex_data.decode data >>= Conex_data_persistency.t_to_team with
    | Error p -> Error (`ParseError (name, p))
    | Ok team ->
      if id_equal team.Team.name name then
        Ok team
      else
        Error (`NameMismatch (name, team.Team.name))

let write_team repo t =
  let id = t.Team.name in
  repo.data.Provider.write (Conex_opam_layout.key_path id) (Conex_data.encode (Conex_data_persistency.team_to_t t))

let read_id repo id =
  match read_key repo id with
  | Ok key -> Ok (`Key key)
  | Error (`NameMismatch (a, b)) -> Error (`NameMismatch (a, b))
  | Error _ -> match read_team repo id with
    | Ok team -> Ok (`Team team)
    | Error e -> Error e

let ids repo = S.of_list (Conex_opam_layout.ids repo.data)

let read_index repo name =
  match repo.data.Provider.read (Conex_opam_layout.index_path name) with
  | Error _ -> Error (`NotFound ("index", name))
  | Ok data ->
    match Conex_data.decode data >>= Conex_data_persistency.t_to_index with
    | Error p -> Error (`ParseError (name, p))
    | Ok i ->
      if id_equal i.Index.name name then
        Ok i
      else
        Error (`NameMismatch (name, i.Index.name))

let write_index repo i =
  let name = Conex_opam_layout.index_path i.Index.name in
  repo.data.Provider.write name (Conex_data.encode (Conex_data_persistency.index_sigs_to_t i))

let read_authorisation repo name =
  match repo.data.Provider.read (Conex_opam_layout.authorisation_path name) with
  | Error _ -> Error (`NotFound ("authorisation", name))
  | Ok data ->
    match Conex_data.decode data >>= Conex_data_persistency.t_to_authorisation with
    | Error p -> Error (`ParseError (name, p))
    | Ok auth ->
      if name_equal auth.Authorisation.name name then
        Ok auth
      else
        Error (`NameMismatch (name, auth.Authorisation.name))

let write_authorisation repo a =
  repo.data.Provider.write
    (Conex_opam_layout.authorisation_path a.Authorisation.name)
    (Conex_data.encode (Conex_data_persistency.authorisation_to_t a))

let items repo = S.of_list (Conex_opam_layout.items repo.data)
let subitems repo name = S.of_list (Conex_opam_layout.subitems repo.data name)

let read_releases repo name =
  match repo.data.Provider.read (Conex_opam_layout.releases_path name) with
  | Error _ -> Error (`NotFound ("releases", name))
  | Ok data ->
    match Conex_data.decode data >>= Conex_data_persistency.t_to_releases with
    | Error p -> Error (`ParseError (name, p))
    | Ok r ->
      if name_equal r.Releases.name name then
        Ok r
      else
        Error (`NameMismatch (name, r.Releases.name))

let write_releases repo r =
  let name = Conex_opam_layout.releases_path r.Releases.name in
  repo.data.Provider.write name (Conex_data.encode (Conex_data_persistency.releases_to_t r))

let read_checksum repo name =
  match repo.data.Provider.read (Conex_opam_layout.checksum_path name) with
  | Error _ -> Error (`NotFound ("checksum", name))
  | Ok data ->
    match Conex_data.decode data >>= Conex_data_persistency.t_to_checksums with
    | Error p -> Error (`ParseError (name, p))
    | Ok csum ->
      if name_equal csum.Checksum.name name then
        Ok csum
      else
        Error (`NameMismatch (name, csum.Checksum.name))

let write_checksum repo csum =
  let name = Conex_opam_layout.checksum_path csum.Checksum.name in
  repo.data.Provider.write name (Conex_data.encode (Conex_data_persistency.checksums_to_t csum))
