open Conex_result
open Conex_core
open Conex_resource
open Conex_utils
open Conex_opam_encoding

type valid_resources = (name * Uint.t * resource * S.t) M.t

type teams = S.t M.t

type t = {
  quorum : int ;
  strict : bool ;
  valid : valid_resources ;
  teams : teams ;
  ids : S.t ;
}

let repository ?(quorum = 3) ?(strict = false) () =
  { quorum ; strict ; valid = M.empty ; teams = M.empty ; ids = S.empty }

let quorum t = t.quorum

let strict t = t.strict

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

let add_valid_resource repo id res =
  let open Index in
  let t = repo.valid in
  let dgst_str = digest_to_string res.digest in
  try
    let n, s, r, ids = M.find dgst_str t in
    if not (name_equal n res.rname) then
      Error ("name not equal: " ^ n ^ " vs " ^ res.rname)
    else if not (resource_equal r res.resource) then
      Error ("resource not equal: " ^ resource_to_string r ^ " vs " ^ resource_to_string res.resource)
    else if not (s = res.size) then
      Error ("size not equal: " ^ Uint.to_string s ^ " vs " ^ Uint.to_string res.size)
    else
      Ok ({ repo with valid = M.add dgst_str (n, s, r, S.add id ids) t })
  with Not_found ->
    Ok ({ repo with valid = M.add dgst_str (res.rname, res.size, res.resource, S.singleton id) t})

let add_team repo team =
  { repo with teams = M.add team.Team.name team.Team.members repo.teams  }

let verify key data (hdr, sigval) =
  let data = extend_sig hdr data in
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

let verify_resource repo owners name resource data =
  let csum = Conex_nocrypto.digest data in
  let csum_str = digest_to_string csum in
  let n, _s, r, ids =
    if M.mem csum_str repo.valid then
      M.find csum_str repo.valid
    else
      (name, Uint.of_int_exn (String.length data), resource, S.empty)
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
  verify_resource repo (S.singleton id) id `PublicKey (encode (Wire.wire_pub id key)) >>= function
  | `Both b -> Ok (`Both b)
  | `Quorum _ -> Error (`MissingSignature id)
  | `IdNoQuorum (id, js) -> Error (`InsufficientQuorum (id, `PublicKey, js))

let verify_signatures idx =
  let tbv = encode (Index.wire_resources idx) in
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
  let encoded = encode data in
  let digest = Conex_nocrypto.digest encoded in
  let r = Index.r Uint.zero name (Uint.of_int_exn (String.length encoded)) typ digest in
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
     - those that can be verified (quorum) *)
  let id = idx.Index.name in
  let signed_keys, _s_warn = verify_signatures idx in
  let valid_keys, _r_warn = List.partition
      (fun key -> contains idx id `PublicKey (Wire.wire_pub id key))
      signed_keys
  in
  match valid_keys, idx.Index.resources with
  | [], [] ->
    (* this is the case where deleted ids will end *)
    begin match verify_resource repo S.empty id `Index (encode (Index.wire idx)) with
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
  match verify_resource repo S.empty id `Team (encode (Team.wire team)) with
  | Error (`NotSigned (n, _, js)) -> Error (`InsufficientQuorum (n, `Team, js))
  | Error e -> Error e
  | Ok (`Quorum js) ->
    guard (S.subset team.Team.members repo.ids)
      (`MemberNotPresent (id, S.diff team.Team.members repo.ids)) >>= fun () ->
    Ok (add_team repo team, `Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"

let verify_authorisation repo auth =
  let name = auth.Authorisation.name in
  match verify_resource repo S.empty name `Authorisation (encode (Authorisation.wire auth)) with
  | Error (`NotSigned (n, _, js)) -> Error (`InsufficientQuorum (n, `Authorisation, js))
  | Error e -> Error e
  | Ok (`Quorum js) ->
    let all = auth.Authorisation.authorised in
    guard (S.for_all (id_loaded repo) all)
      (`IdNotPresent (name, S.filter (fun id -> not (id_loaded repo id)) all)) >>= fun () ->
      Ok (`Quorum js)
  (* the following two cases will never happen, since authorised is S.empty! *)
  | Ok (`IdNoQuorum _) | Ok (`Both _) -> invalid_arg "can not happen"

let ensure_releases rel disk =
  let rels = rel.Releases.releases
  and dirs = disk.Releases.releases
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

let verify_releases repo ?on_disk a r =
  guard (name_equal a.Authorisation.name r.Releases.name)
    (`AuthRelMismatch (a.Authorisation.name, r.Releases.name)) >>= fun () ->
  guard (S.for_all (is_release r.Releases.name) r.Releases.releases)
    (`NoSharedPrefix (r.Releases.name, r.Releases.releases)) >>= fun () ->
  verify_resource repo a.Authorisation.authorised r.Releases.name `Releases (encode (Releases.wire r)) >>= fun res ->
  let res = match res with
    | `Both b -> Ok (`Both b)
    | `Quorum js -> Ok (`Quorum js)
    | `IdNoQuorum (id, _) -> Ok (`Signed id)
  in
  match on_disk with
  | None -> res
  | Some rels ->
    match ensure_releases r rels with
    | Error (h, w) -> Error (`InvalidReleases (r.Releases.name, h, w))
    | Ok () -> res

let verify_checksum repo ?on_disk a r cs =
  guard (name_equal a.Authorisation.name r.Releases.name)
    (`AuthRelMismatch (a.Authorisation.name, r.Releases.name)) >>= fun () ->
  guard (S.mem cs.Checksum.name r.Releases.releases)
    (`NotInReleases (cs.Checksum.name, r.Releases.releases)) >>= fun () ->
  verify_resource repo a.Authorisation.authorised cs.Checksum.name `Checksums (encode (Checksum.wire cs)) >>= fun res ->
  let res = match res with
    | `Both b -> Ok (`Both b)
    | `Quorum js -> Ok (`Quorum js)
    | `IdNoQuorum (id, _) -> Ok (`Signed id)
  in
  match on_disk with
  | None -> res
  | Some css -> Checksum.compare_checksums cs css >>= fun () -> res


type m_err = [ `NotIncreased of resource * name | `Deleted of resource * name | `Msg of resource * string ]

let pp_m_err ppf =
  let s = resource_to_string in
  function
  | `NotIncreased (res, nam) -> Format.fprintf ppf "monotonicity: counter of %s %s was not increased" (s res) nam
  | `Deleted (res, nam) -> Format.fprintf ppf "monotonicity: %s %s was deleted" (s res) nam
  | `Msg (res, str) -> Format.fprintf ppf "monotonicity: %s %s" (s res) str

let increased old ne r nam = guard (Uint.compare ne old = 1) (`NotIncreased (r, nam))

let monoton_index ?old ?now _t =
  match old, now with
  | Some idx, Some idx' -> increased idx.Index.counter idx'.Index.counter `Index idx.Index.name
  | None, Some _ -> Ok () (* allow creation of ids *)
  | Some idx, None -> Error (`Deleted (`Index, idx.Index.name)) (* DO NOT allow index deletions *)
  | None, None -> Error (`Msg (`Index, "both are none"))

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

let monoton_releases ?old ?now _t =
  match old, now with
  | Some rel, Some rel' -> increased rel.Releases.counter rel'.Releases.counter `Releases rel.Releases.name
  | None, Some _ -> Ok () (* allow creation *)
  | Some rel, None -> Error (`Deleted (`Releases, rel.Releases.name)) (* DO NOT allow deletion of releases *)
  | None, None -> Error (`Msg (`Releases, "both are none"))

let monoton_checksum ?old ?now _t =
  match old, now with
  | Some cs, Some cs' -> increased cs.Checksum.counter cs'.Checksum.counter `Checksums cs.Checksum.name
  | None, Some _ -> Ok () (* allow creation *)
  | Some _, None -> Ok () (* allow deletion of checksums *)
  | None, None -> Error (`Msg (`Checksums, "both are none"))
