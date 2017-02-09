open Conex_resource
open Conex_utils

type valid_resources = (name * typ * S.t) M.t

type teams = S.t M.t

type t = {
  quorum : int ;
  strict : bool ;
  valid : valid_resources ;
  teams : teams ;
  ids : S.t ;
  digestf : (Wire.t -> Digest.t);
}

let repository ?(quorum = 3) ?(strict = false) digestf () =
  { quorum ; strict ; valid = M.empty ; teams = M.empty ; ids = S.empty ; digestf }

let quorum t = t.quorum

let strict t = t.strict

let digestf t = t.digestf

let find_team t id = try Some (M.find id t.teams) with Not_found -> None

let id_loaded t id = S.mem id t.ids || find_team t id <> None

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

type conflict = [
  | `NameConflict of name * Author.r
  | `TypConflict of typ * Author.r
]

(*BISECT-IGNORE-BEGIN*)
let pp_conflict ppf = function
  | `NameConflict (have, inmem) -> Format.fprintf ppf "resource name conflict %a vs %a" pp_name have Author.pp_resource inmem
  | `TypConflict (have, inmem) -> Format.fprintf ppf "resource type conflict %a vs %a" pp_typ have Author.pp_resource inmem
(*BISECT-IGNORE-END*)

let add_valid_resource repo id res =
  let open Author in
  let t = repo.valid in
  let dgst_str = Digest.to_string res.digest in
  try
    let n, r, ids = M.find dgst_str t in
    if not (name_equal n res.rname) then
      Error (`NameConflict (n, res))
    else if not (typ_equal r res.rtyp) then
      Error (`TypConflict (r, res))
    else
      Ok ({ repo with valid = M.add dgst_str (n, r, S.add id ids) t })
  with Not_found ->
    Ok ({ repo with valid = M.add dgst_str (res.rname, res.rtyp, S.singleton id) t})

let add_index repo idx =
  List.fold_left (fun (repo, warn) res ->
      match add_valid_resource repo idx.Author.name res with
      | Ok r -> r, warn
      | Error msg -> repo, msg :: warn)
    (repo, [])
    idx.Author.resources

let add_team repo team =
  { repo with teams = M.add team.Team.name team.Team.members repo.teams  }

(*BISECT-IGNORE-BEGIN*)
let pp_ok ppf = function
  | `Approved id -> Format.fprintf ppf "ok by id %s" id
  | `Both (id, js) -> Format.fprintf ppf "ok by id %s and quorum %s" id (String.concat ", " (S.elements js))
  | `Quorum js -> Format.fprintf ppf "ok by quorum %s" (String.concat ", " (S.elements js))
(*BISECT-IGNORE-END*)

type base_error = [
  | `InvalidName of name * name
  | `InvalidResource of name * typ * typ
  | `NotApproved of name * typ * S.t
]

(*BISECT-IGNORE-BEGIN*)
let pp_cs ppf (a, b) =
  Format.fprintf ppf "have %a want %a" Release.pp_checksum a Release.pp_checksum b

let pp_error ppf = function
  | `InvalidName (w, h) -> Format.fprintf ppf "invalid resource name, looking for %a but got %a" pp_name w pp_name h
  | `InvalidResource (n, w, h) -> Format.fprintf ppf "invalid resource type %a, looking for %a but got %a" pp_name n pp_typ w pp_typ h
  | `NotApproved (n, r, _) -> Format.fprintf ppf "not approved %a %a" pp_typ r pp_name n
  | `InsufficientQuorum (name, r, goods) -> Format.fprintf ppf "quorum for %a %a insufficient: %a" pp_typ r pp_name name (pp_list pp_id) (S.elements goods)
  | `AuthorWithoutKeys id -> Format.fprintf ppf "author %a does not have any public keys" pp_id id
  | `IdNotPresent (n, s) -> Format.fprintf ppf "packages %a, authorised ids %a missing" pp_name n (pp_list pp_id) (S.elements s)
  | `MemberNotPresent (n, s) -> Format.fprintf ppf "team %a, members %a missing" pp_id n (pp_list pp_id) (S.elements s)
  | `AuthRelMismatch (a, r) -> Format.fprintf ppf "package name in authorisation of %a is different from releases %a" pp_name a pp_name r
  | `InvalidReleases (n, h, w) when S.equal h S.empty -> Format.fprintf ppf "several releases of %a are missing on disk: %a" pp_name n (pp_list pp_name) (S.elements w)
  | `InvalidReleases (n, h, w) when S.equal w S.empty -> Format.fprintf ppf "several releases of %a are not in the signed releases file %a" pp_name n (pp_list pp_name) (S.elements h)
  | `InvalidReleases (n, h, w) -> Format.fprintf ppf "the releases file of %a diverges: %a are on disk, but not in the file, %a are in the file, but not on disk" pp_name n (pp_list pp_name) (S.elements h) (pp_list pp_name) (S.elements w)
  | `NoSharedPrefix (n, rels) -> Format.fprintf ppf "releases %a contains releases where its name is not a prefix %a" pp_name n (pp_list pp_name) (S.elements rels)
  | `NotInReleases (c, rs) -> Format.fprintf ppf "the package name %a is not in the set of released versions %a" pp_name c (pp_list pp_name) (S.elements rs)
  | `ChecksumsDiff (n, miss, too, diffs) -> Format.fprintf ppf "checksums for %a differ, missing on disk: %a, missing in checksums file: %a, checksums differ: %a" pp_name n (pp_list pp_name) miss (pp_list pp_name) too (pp_list pp_cs) diffs
(*BISECT-IGNORE-END*)

let validate_resource repo owners name resource wire =
  let digest = repo.digestf wire in
  let csum_str = Digest.to_string digest in
  let n, r, ids =
    if M.mem csum_str repo.valid then
      M.find csum_str repo.valid
    else
      (name, resource, S.empty)
  in
  let janitors = match find_team repo "janitors" with None -> S.empty | Some s -> s in
  let js = S.inter janitors ids in
  let owners = expand_owner repo owners in
  let signed_owners = S.inter ids owners in
  match
    name_equal n name,
    typ_equal r resource,
    S.cardinal signed_owners > 0,
    S.cardinal js >= repo.quorum
  with
  | false, _    , _    , _     -> Error (`InvalidName (name, n))
  | true , false, _    , _     -> Error (`InvalidResource (name, resource, r))
  | true , true , false, false -> Error (`NotApproved (n, r, js))
  | true , true , false, true  -> Ok (`Quorum js)
  | true , true , true , false -> Ok (`IdNoQuorum (S.choose signed_owners, js))
  | true , true , true , true  -> Ok (`Both (S.choose signed_owners, js))

let validate_key repo id key =
  validate_resource repo (S.singleton id) id `Key (Key.wire id key) >>= function
  | `Both b -> Ok (`Both b)
  | `Quorum _ -> Error (`NotApproved (id, `Key, S.empty))
  | `IdNoQuorum (id, js) -> Error (`InsufficientQuorum (id, `Key, js))

let contains ?queued repo idx name typ data =
  let digest = repo.digestf data in
  let r = Author.r Uint.zero name typ digest in
  Author.contains ?queued idx r

let validate_account repo author a =
  let name = author.Author.name
  and wired = Author.wire_account a
  in
  validate_resource repo (S.singleton name) name `Account wired >>= function
  | `Both _ -> Ok (`Approved name)
  | `Quorum _ -> Error (`NotApproved (name, `Account, S.empty)) (* TODO: loses account *)
  | `IdNoQuorum (id, js) -> Error (`InsufficientQuorum (id, `Account, js))

let validate_author repo author =
  let id = author.Author.name in
  match author.Author.keys, author.Author.resources with
  | [], [] ->
    (match validate_resource repo S.empty id `Author (Author.wire_raw author) with
     | Ok (`Quorum _) -> Ok (add_id repo id, [])
     | _ -> Error (`AuthorWithoutKeys id))
  | [], _ -> Error (`AuthorWithoutKeys id)
  | _k :: _keys, _rs ->
    let repo', warnings = add_index repo author in
    List.fold_left (fun r (k, _) ->
        match r, validate_key repo' id k with
        | Ok (), _ -> Ok ()
        | Error _, Ok _ -> Ok ()
        | Error _, Error e -> Error e)
      (Error (`NotApproved (id, `Key, S.empty)))
      author.Author.keys >>= fun () ->
    Ok (repo', warnings)

let validate_team repo team =
  let id = team.Team.name in
  match validate_resource repo S.empty id `Team (Team.wire team) with
  | Error (`NotApproved (n, _, js)) -> Error (`InsufficientQuorum (n, `Team, js))
  | Error e -> Error e
  | Ok (`Quorum js) ->
    guard (S.subset team.Team.members repo.ids)
      (`MemberNotPresent (id, S.diff team.Team.members repo.ids)) >>= fun () ->
    Ok (add_team repo team, `Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"

let validate_authorisation repo auth =
  let name = auth.Authorisation.name in
  match validate_resource repo S.empty name `Authorisation (Authorisation.wire auth) with
  | Error (`NotApproved (n, _, js)) -> Error (`InsufficientQuorum (n, `Authorisation, js))
  | Error e -> Error e
  | Ok (`Quorum js) ->
    let all = auth.Authorisation.authorised in
    guard (S.for_all (id_loaded repo) all)
      (`IdNotPresent (name, S.filter (fun id -> not (id_loaded repo id)) all)) >>= fun () ->
    Ok (`Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"

let ensure_releases rel disk =
  let rels = rel.Package.releases
  and dirs = disk.Package.releases
  in
  if S.equal rels dirs then
    Ok ()
  else
    let have = S.diff dirs rels
    and want = S.diff rels dirs
    in
    Error (have, want)

let is_release name a =
  match Conex_opam_repository_layout.authorisation_of_item a with
  | Some x -> name_equal name x
  | _ -> false

let validate_package repo ?on_disk a r =
  guard (name_equal a.Authorisation.name r.Package.name)
    (`AuthRelMismatch (a.Authorisation.name, r.Package.name)) >>= fun () ->
  guard (S.for_all (is_release r.Package.name) r.Package.releases)
    (`NoSharedPrefix (r.Package.name, r.Package.releases)) >>= fun () ->
  validate_resource repo a.Authorisation.authorised r.Package.name `Package (Package.wire r) >>= fun res ->
  let res = match res with
    | `Both b -> Ok (`Both b)
    | `Quorum js -> Ok (`Quorum js)
    | `IdNoQuorum (id, _) -> Ok (`Approved id)
  in
  match on_disk with
  | None -> res
  | Some rels ->
    match ensure_releases r rels with
    | Error (h, w) -> Error (`InvalidReleases (r.Package.name, h, w))
    | Ok () -> res

let validate_release repo ?on_disk a r cs =
  guard (name_equal a.Authorisation.name r.Package.name)
    (`AuthRelMismatch (a.Authorisation.name, r.Package.name)) >>= fun () ->
  guard (S.mem cs.Release.name r.Package.releases)
    (`NotInReleases (cs.Release.name, r.Package.releases)) >>= fun () ->
  validate_resource repo a.Authorisation.authorised cs.Release.name `Release (Release.wire cs) >>= fun res ->
  let res = match res with
    | `Both b -> Ok (`Both b)
    | `Quorum js -> Ok (`Quorum js)
    | `IdNoQuorum (id, _) -> Ok (`Approved id)
  in
  match on_disk with
  | None -> res
  | Some css -> Release.compare_checksums cs css >>= fun () -> res

type m_err = [ `NotIncreased of typ * name | `Deleted of typ * name | `Msg of typ * string ]

let pp_m_err ppf = function
  | `NotIncreased (res, nam) -> Format.fprintf ppf "monotonicity: counter of %a %a was not increased" pp_typ res pp_name nam
  | `Deleted (res, nam) -> Format.fprintf ppf "monotonicity: %a %a was deleted" pp_typ res pp_name nam
  | `Msg (res, str) -> Format.fprintf ppf "monotonicity: %a %s" pp_typ res str

let increased old ne r nam = guard (Uint.compare ne old = 1) (`NotIncreased (r, nam))

let monoton_author ?old ?now _t =
  match old, now with
  | Some idx, Some idx' -> increased idx.Author.counter idx'.Author.counter `Author idx.Author.name
  | None, Some _ -> Ok () (* allow creation of ids *)
  | Some idx, None -> Error (`Deleted (`Author, idx.Author.name)) (* DO NOT allow author deletions *)
  | None, None -> Error (`Msg (`Author, "both are none"))

let monoton_team ?old ?now _t =
  match old, now with
  | Some team, Some team' -> increased team.Team.counter team'.Team.counter `Team team.Team.name
  | None, Some _ -> Ok () (* allow creation of ids *)
  | Some team, None -> Error (`Deleted (`Team, team.Team.name)) (* DO NOT allow team deletions *)
  | None, None -> Error (`Msg (`Team, "both are none"))

let monoton_authorisation ?old ?now _t =
  match old, now with
  | Some a, Some a' -> increased a.Authorisation.counter a'.Authorisation.counter `Authorisation a.Authorisation.name
  | None, Some _ -> Ok () (* allow creation *)
  | Some a, None -> Error (`Deleted (`Authorisation, a.Authorisation.name)) (* DO NOT allow deletion of authorsations *)
  | None, None -> Error (`Msg (`Authorisation, "both are none"))

let monoton_package ?old ?now _t =
  match old, now with
  | Some rel, Some rel' -> increased rel.Package.counter rel'.Package.counter `Package rel.Package.name
  | None, Some _ -> Ok () (* allow creation *)
  | Some rel, None -> Error (`Deleted (`Package, rel.Package.name)) (* DO NOT allow deletion of packages *)
  | None, None -> Error (`Msg (`Package, "both are none"))

let monoton_release ?old ?now _t =
  match old, now with
  | Some cs, Some cs' -> increased cs.Release.counter cs'.Release.counter `Release cs.Release.name
  | None, Some _ -> Ok () (* allow creation *)
  | Some _, None -> Ok () (* allow deletion of releases *)
  | None, None -> Error (`Msg (`Release, "both are none"))
