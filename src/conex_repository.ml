open Conex_result
open Conex_core
open Conex_resource
open Conex_utils

type valid_resources = (name * Uint.t * resource * S.t) M.t

type teams = S.t M.t

type t = {
  quorum : int ;
  strict : bool ;
  data : Provider.t ;
  valid : valid_resources ;
  teams : teams ;
  ids : S.t ;
}

let repository ?(quorum = 3) ?(strict = false) data =
  { quorum ; strict ; data ; valid = M.empty ; teams = M.empty ; ids = S.empty }

let quorum r = r.quorum

let strict r = r.strict

let provider r = r.data

let find_team r id = try Some (M.find id r.teams) with Not_found -> None

let id_loaded t id = S.mem id t.ids

let add_id t id = { t with ids = S.add id t.ids }

let expand_owner r os =
  S.fold
    (fun id s -> match find_team r id with
       | None -> S.add id s
       | Some mems -> S.union s mems)
    os
    S.empty

let authorised r a id =
  let owners = expand_owner r a.Authorisation.authorised in
  S.mem id owners

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

let add_team repo team =
  { repo with
    teams = M.add team.Team.name team.Team.members repo.teams ;
    ids = S.add team.Team.name repo.ids
  }

let verify key data (ts, sigval) =
  let data = Signature.extend_data data ts in
  Conex_nocrypto.verify key data sigval


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
  Format.fprintf ppf "have %a want %a" Checksum.pp_checksum a Checksum.pp_checksum b

let pp_error ppf = function
  | `InvalidName (w, h) -> Format.fprintf ppf "invalid resource name, looking for %a but got %a" pp_name w pp_name h
  | `InvalidResource (n, w, h) -> Format.fprintf ppf "invalid resource type %a, looking for %a but got %a" pp_name n pp_resource w pp_resource h
  | `NotSigned (n, r, _) -> Format.fprintf ppf "unsigned %a %a" pp_resource r pp_name n
  | `InsufficientQuorum (name, r, goods) -> Format.fprintf ppf "quorum for %a %a insufficient: %a" pp_resource r pp_name name (pp_list pp_id) (S.elements goods)
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

let verify_resource repo owners name resource data =
  let csum = Conex_nocrypto.digest data in
  let n, _s, r, ids =
    if M.mem csum repo.valid then
      M.find csum repo.valid
    else
      (name, Uint.of_int (String.length data), resource, S.empty)
  in
  let janitors = match find_team repo "janitors" with None -> S.empty | Some s -> s in
  let js = S.inter janitors ids in
  let owners = expand_owner repo owners in
  let signed_owners = S.inter ids owners in
  match
    name_equal n name,
    resource_equal r resource,
    S.cardinal signed_owners > 0,
    S.cardinal js >= repo.quorum
  with
  | false, _    , _    , _     -> Error (`InvalidName (name, n))
  | true , false, _    , _     -> Error (`InvalidResource (name, resource, r))
  | true , true , false, false -> Error (`NotSigned (n, r, js))
  | true , true , false, true  -> Ok (`Quorum js)
  | true , true , true , false -> Ok (`IdNoQuorum (S.choose signed_owners, js))
  | true , true , true , true  -> Ok (`Both (S.choose signed_owners, js))

let verify_key repo id key =
  verify_resource repo (S.singleton id) id `PublicKey (Conex_data.encode (Conex_data_persistency.publickey_to_t id key)) >>= function
  | `Both b -> Ok (`Both b)
  | `Quorum _ -> Error (`MissingSignature id)
  | `IdNoQuorum (id, js) -> Error (`InsufficientQuorum (id, `PublicKey, js))

let verify_signatures idx =
  let tbv = Conex_data.encode (Conex_data_persistency.index_to_t idx) in
  List.fold_left (fun (good, warn) k ->
      match List.fold_left (fun r s ->
          match r, verify k tbv s with
          | Ok (), _ -> Ok ()
          | _, b -> b)
          (Error `NoSignature)
          idx.Index.signatures
      with
      | Ok () -> (k :: good, warn)
      | Error e -> (good, (k, e) :: warn))
    ([], []) idx.Index.keys

let contains ?(queued = false) idx name typ data =
  let encoded = Conex_data.encode data in
  let digest = Conex_nocrypto.digest encoded in
  let r = Index.r Uint.zero name (Uint.of_int (String.length encoded)) typ digest in
  let xs = if queued then idx.Index.resources @ idx.Index.queued else idx.Index.resources in
  List.exists (Index.r_equal r) xs

let add_index repo idx =
  List.fold_left (fun (repo, warn) res ->
      match add_valid_resource repo idx.Index.name res with
      | Ok r -> r, warn
      | Error msg -> repo, msg :: warn)
    (repo, [])
    idx.Index.resources

let verify_index repo idx =
  (* this is a multi step process:
     - use all keys of the idx, filter:
     - those with a valid signature for the idx
     - those with a valid resource entry
     - those that can be verified (quorum)

     --> or else maybe the (empty!) idx can be verified by janitors
 *)
  let id = idx.Index.name in
  let signed_keys, _s_warn = verify_signatures idx in
  let valid_keys, _r_warn = List.partition
      (fun key -> contains idx id `PublicKey (Conex_data_persistency.publickey_to_t id key))
      signed_keys
  in
  match valid_keys, idx.Index.resources with
  | [], [] ->
    begin match verify_resource repo S.empty id `Index (Conex_data.encode (Conex_data_persistency.index_sigs_to_t idx)) with
      | Ok _ -> Ok (add_id repo id, [], id)
      | Error _ -> Error `NoSignature
    end
  | [], _ -> Error `NoSignature
  | _, _ ->
    let quorum, _q_warn =
      List.fold_left (fun (good, warn) pub ->
          match verify_key repo id pub with
          | Error (`MissingSignature _) (* we checked above that the publickey is in the resources list! *)
          | Ok (`Both _) -> pub :: good, warn
          | _ -> good, pub :: warn)
        ([], []) valid_keys
    in
    match quorum with
    | [] -> Error `NoSignature
    | _ ->
      let repo, warn = add_index repo idx in
      let repo = add_id repo id in
      Ok (repo, warn, id)

let verify_team repo team =
  let id = team.Team.name in
  match verify_resource repo S.empty id `Team (Conex_data.encode (Conex_data_persistency.team_to_t team)) with
  | Error (`NotSigned (n, _, js)) -> Error (`InsufficientQuorum (n, `Team, js))
  | Error e -> Error e
  | Ok (`Quorum js) -> Ok (add_team repo team, `Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"
  (* should verify that all members are on disk (and are index, not teams!!!) *)

let verify_authorisation repo auth =
  let name = auth.Authorisation.name in
  match verify_resource repo S.empty name `Authorisation (Conex_data.encode (Conex_data_persistency.authorisation_to_t auth)) with
  | Error (`NotSigned (n, _, js)) -> Error (`InsufficientQuorum (n, `Authorisation, js))
  | Error e -> Error e
  | Ok (`Quorum js) -> Ok (`Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"
  (* should verify that all ids exist? *)

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

let read_team repo name =
  match repo.data.Provider.read (Conex_opam_layout.id_path name) with
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
  repo.data.Provider.write (Conex_opam_layout.id_path id) (Conex_data.encode (Conex_data_persistency.team_to_t t))

let read_index repo name =
  match repo.data.Provider.read (Conex_opam_layout.id_path name) with
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
  let name = Conex_opam_layout.id_path i.Index.name in
  repo.data.Provider.write name (Conex_data.encode (Conex_data_persistency.index_sigs_to_t i))

let read_id repo id =
  match read_team repo id with
  | Ok team -> Ok (`Team team)
  | Error (`NameMismatch (a, b)) -> Error (`NameMismatch (a, b))
  | Error _ -> match read_index repo id with
    | Ok idx -> Ok (`Id idx)
    | Error e -> Error e

let ids repo = S.of_list (Conex_opam_layout.ids repo.data)

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

type m_err = [ r_err | `NotIncreased of resource * name | `Deleted of resource * name | `Msg of string ]

let pp_m_err ppf =
  let s = resource_to_string in
  function
  | #r_err as e -> pp_r_err ppf e
  | `NotIncreased (res, nam) -> Format.fprintf ppf "monotonicity: counter of %s %s was not increased" (s res) nam
  | `Deleted (res, nam) -> Format.fprintf ppf "monotonicity: %s %s was deleted" (s res) nam
  | `Msg s -> Format.fprintf ppf "monotonicity: %s" s

let monotonicity repo repo' resource name =
  let e = (resource, name) in
  let incr c c' =
    guard (Uint.compare c' c = 1) (`NotIncreased e)
  in
  match resource with
  | `Index ->
    begin match read_id repo name, read_id repo' name with
     | Ok (`Id idx), Ok (`Id idx') -> incr idx'.Index.counter idx.Index.counter
     | Ok (`Team team), Ok (`Team team') -> incr team'.Team.counter team.Team.counter
     | Error _, Ok _ -> Ok () (* allow creation (could check for valid + unique id) *)
     | Ok _, Error _ -> Error (`Deleted e) (* DO NOT allow index deletions *)
     | Error e, Error _ -> Error e
     | Ok (`Id _), Ok (`Team _) -> Error (`Msg ("id " ^ name ^ " is now a team"))
     | Ok (`Team _), Ok (`Id _) -> Error (`Msg ("team " ^ name ^ " is now an id"))
    end
  | `Checksums ->
    begin match read_checksum repo name, read_checksum repo' name with
     | Ok cs, Ok cs' -> incr cs'.Checksum.counter cs.Checksum.counter
     | Error _, Ok _ -> Ok () (* allow creation *)
     | Ok _, Error _ -> Ok () (* allow deletion of checksums *)
     | Error e, Error _ -> Error e
    end
  | `Releases ->
    begin match read_releases repo name, read_releases repo' name with
     | Ok rel, Ok rel' -> incr rel'.Releases.counter rel.Releases.counter
     | Error _, Ok _ -> Ok () (* allow creation *)
     | Ok _, Error _ -> Error (`Deleted e) (* DO NOT allow deletion of releases *)
     | Error e, Error _ -> Error e
    end
  | `Authorisation ->
    begin match read_authorisation repo name, read_authorisation repo' name with
     | Ok auth, Ok auth' -> incr auth'.Authorisation.counter auth.Authorisation.counter
     | Error _, Ok _ -> Ok () (* allow creation *)
     | Ok _, Error _ -> Error (`Deleted e) (* DO NOT allow deletion of authorsations *)
     | Error e, Error _ -> Error e
    end
  | `PublicKey | `Team -> Error (`Msg "not sure what you wanted to do")

