open Conex_result
open Conex_core
open Conex_resource
open Conex_repository

(* this is stripped down from Logs library *)
module type LOGS = sig
  module Tag : sig
    type set
  end

  type ('a, 'b) msgf =
    (?header:string -> ?tags:Tag.set ->
     ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
  type 'a log = ('a, unit) msgf -> unit

  type src

  val debug : ?src:src -> 'a log
  val info : ?src:src -> 'a log
  val warn : ?src:src -> 'a log
end

let str pp e =
  Format.(fprintf str_formatter "%a" pp e) ;
  Format.flush_str_formatter ()

module IO = Conex_io

module Make (L : LOGS) = struct

  let maybe repo pp res = function
    | Ok a -> Ok a
    | Error err ->
      if strict repo then Error (str pp err) else
        (L.warn (fun m -> m "%a" pp err) ; Ok res)

  let load_id_aux io repo id =
    match IO.read_id io id with
    | Error e ->
      if strict repo then Error (str IO.pp_r_err e) else
        Ok (`Nothing, repo)
    | Ok (`Id idx) ->
      (L.debug (fun m -> m "%a" Index.pp_index idx) ;
       maybe repo pp_verification_error (`Nothing, repo)
         (verify_index repo idx >>= fun (repo, msgs, _) ->
          List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) msgs ;
          Ok (`Id idx, repo)))
    | Ok (`Team t) ->
      (L.debug (fun m -> m "%a" Team.pp_team t) ;
       maybe repo pp_error (`Nothing, repo)
         (verify_team repo t >>= fun (r, ok) ->
          L.debug (fun m -> m "%a" pp_ok ok) ;
          Ok (`Team t, r)))

  let load_id io repo id =
    if id_loaded repo id then
      Ok repo
    else
      let repo = add_id repo id in
      load_id_aux io repo id >>= function
      | `Id _, repo ->  Ok repo
      | `Team t, repo ->
        let load_id repo id =
          load_id_aux io repo id >>= function
          | `Nothing, repo -> Ok repo
          | `Id _, repo -> Ok repo
          | `Team t', _ -> Error ("team " ^ t'.Team.name ^ " is a member of " ^ t.Team.name ^ ", another team")
        in
        foldM load_id repo (S.elements t.Team.members)
      | `Nothing, repo -> Ok repo

  let load_janitors ?(valid = fun _ _ -> true) io repo =
    match IO.read_team io "janitors" with
    | Error e ->
      if strict repo then Error (str IO.pp_r_err e) else
        (L.warn (fun m -> m "%a" IO.pp_r_err e) ; Ok repo)
    | Ok team ->
      L.debug (fun m -> m "%a" Team.pp_team team) ;
      foldM (fun acc id ->
          maybe repo IO.pp_r_err acc
            (IO.read_index io id >>= fun idx ->
             L.debug (fun m -> m "%a" Index.pp_index idx) ;
             Ok (idx :: acc)))
        [] (S.elements team.Team.members) >>= fun idxs ->
      (* for each, validate public key(s): signs the index, resource is contained *)
      let valid_idxs =
        List.fold_left (fun acc idx ->
            let id = idx.Index.name in
            let good, warnings = verify_signatures idx in
            List.iter (fun (_, e) -> L.warn (fun m -> m "%a" pp_verification_error e)) warnings ;
            let f k = contains idx id `PublicKey (Wire.wire_pub id k) in
            match List.filter f good with
            | [] -> L.warn (fun m -> m "ignoring %s due to missing self-signature" id) ; acc
            | xs -> L.debug (fun m -> m "janitor index %s is self-signed" id) ; (xs, idx) :: acc)
          [] idxs
      in
      (* filter by fingerprint, create a repository *)
      let approved, notyet =
        List.fold_left (fun (good, bad) (keys, idx) ->
            let id = idx.Index.name in
            let approved = List.filter (fun k -> valid id (Conex_nocrypto.id k)) keys in
            if List.length approved > 0 then begin
              L.debug (fun m -> m "fingerprint valid for janitor %s" id) ;
              ((approved, idx) :: good, bad)
            end else begin
              L.debug (fun m -> m "ignoring janitor %s: missing valid fingerprint" id) ;
              (good, (keys, idx) :: bad)
            end)
          ([], []) valid_idxs
      in
      let add_warn repo idx =
        let repo, warns = add_index repo idx in
        List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) warns ;
        repo
      in
      let jrepo = List.fold_left add_warn repo (List.map snd approved) in

      (* repo is full with all good indexes! *)
      (* verify all the keys! *)
      let good_idxs, others =
        List.fold_left (fun (good, bad) (keys, idx) ->
            match
              List.fold_left (fun r k ->
                  match r, verify_key jrepo idx.Index.name k with
                  | Ok x, _ -> Ok x
                  | Error _, x -> x)
                (Error (`NotSigned (idx.Index.name, `PublicKey, S.empty)))
                keys
            with
            | Ok _ ->
              L.debug (fun m -> m "janitor index %s has an approved key" idx.Index.name) ;
              (idx :: good, bad)
            | Error _ ->
              L.debug (fun m -> m "janitor index %s has no approved key" idx.Index.name) ;
              good, (keys, idx) :: bad)
          ([], []) approved
      in
      (* verify the team janitor *)
      let good_j_repo = List.fold_left add_warn repo good_idxs in
      (maybe good_j_repo pp_error (add_team good_j_repo team)
         (verify_team good_j_repo team >>= fun (repo, ok) ->
          L.debug (fun m -> m "team janitors verified %a" pp_ok ok) ;
          Ok repo)) >>= fun repo ->
      (* team is good, there may be more janitors: esp notyet and others *)
      (* load in a similar style:
         - add all the idxs
         - verify at least one key of each idx
         - insert all verified idx *)
      let jrepo' = List.fold_left add_warn repo (List.map snd (notyet@others)) in
      foldM (fun acc (keys, idx) ->
          maybe repo pp_error acc
            (List.fold_left (fun r k ->
                match r, verify_key jrepo' idx.Index.name k with
                | Ok x, _ -> Ok x
                | Error _, x -> x)
                (Error (`NotSigned (idx.Index.name, `PublicKey, S.empty)))
                keys >>= fun ok ->
             L.debug (fun m -> m "janitor key %s approved by %a" idx.Index.name pp_ok ok) ;
             Ok (idx :: acc)))
        [] (notyet@others) >>= fun good_idx ->
      Ok (List.fold_left add_warn repo good_idx)

  let verify_single_release io repo auth rel version =
    match IO.read_checksum io version with
    | Error e -> if strict repo then Error (str IO.pp_r_err e) else
        (L.warn (fun m -> m "%a" IO.pp_r_err e) ; Ok ())
    | Ok cs ->
      L.debug (fun m -> m "%a" Checksum.pp_checksums cs) ;
      maybe repo Conex_io.pp_cc_err None
        (IO.compute_checksum io version >>= fun cs ->
         Ok (Some cs)) >>= fun on_disk ->
      maybe repo pp_error ()
        (verify_checksum repo ?on_disk auth rel cs >>= fun ok ->
         L.debug (fun m -> m "%a" pp_ok ok) ;
         Ok ())

  let verify_item ?(authorised = fun _authorised -> true) ?(release = fun _release -> true) io repo name =
    (match IO.read_authorisation io name with
     | Error e -> if strict repo then Error (str IO.pp_r_err e) else
         (L.warn (fun m -> m "%a" IO.pp_r_err e) ;
          Ok (Authorisation.authorisation name, repo))
     | Ok auth ->
       L.debug (fun m ->m "%a" Authorisation.pp_authorisation auth) ;
       maybe repo pp_error false
         (verify_authorisation repo auth >>= fun ok->
          L.debug (fun m -> m "%a" pp_ok ok) ;
          Ok true) >>= (function
           | false -> Ok (auth, repo)
           | true ->
             foldM (load_id io) repo (S.elements auth.Authorisation.authorised) >>= fun repo ->
             Ok (auth, repo))) >>= fun (auth, repo) ->
    (if authorised auth.Authorisation.authorised then
       (match IO.read_releases io name with
        | Error e ->
          (match Releases.releases ~releases:(IO.subitems io name) name with
           | Ok rel -> if strict repo then Error (str IO.pp_r_err e) else
               (L.warn (fun m -> m "%a" IO.pp_r_err e) ; Ok rel)
           | Error e -> Error e)
        | Ok rel ->
          L.debug (fun m -> m "%a" Releases.pp_releases rel) ;
          (match IO.compute_releases io name with
           | Error e -> if strict repo then Error e else
               (L.warn (fun m -> m "couldn't compute releases %s: %s" name e) ;
                Ok None)
           | Ok on_disk -> Ok (Some on_disk)) >>= fun on_disk ->
          maybe repo pp_error rel
            (verify_releases repo ?on_disk auth rel >>= fun ok ->
             L.debug (fun m -> m "%a" pp_ok ok) ;
             Ok rel)) >>= fun rel ->
       foldM (fun () n ->
           if release n then
             verify_single_release io repo auth rel n
           else
             Ok ())
         ()
         (S.elements rel.Releases.releases)
     else
       (L.debug (fun m -> m "ignoring %s" name) ; Ok ())) >>= fun () ->
    Ok repo

  let verify_patch repo io newio (ids, auths, rels, pkgs) =
    let packages = M.fold (fun _name versions acc -> S.elements versions @ acc) pkgs [] in
    L.debug (fun m -> m "verifying a diff with %d ids %d auths %d rels %d pkgs"
                (S.cardinal ids) (S.cardinal auths) (S.cardinal rels) (List.length packages)) ;
    (* all public keys of janitors in repo are valid. *)
    load_janitors io repo >>= fun repo ->
    let janitor_keys =
      S.fold (fun id acc ->
          match IO.read_index io id with
          | Ok idx -> S.union acc (s_of_list (List.map Conex_nocrypto.id idx.Index.keys))
          | Error _ -> acc)
        (match find_team repo "janitors" with None -> S.empty | Some x -> x)
        S.empty
    in
    (* load all those janitors in newrepo which are valid (and verify janitors team) *)
    let valid _id key = S.mem key janitor_keys in
    let fresh_repo = repository ~quorum:(quorum repo) ~strict:(strict repo) () in
    load_janitors ~valid newio fresh_repo >>= fun newrepo ->
    (* now we do a full verification of the new repository *)
    foldM (verify_item newio) newrepo (S.elements (IO.items newio)) >>= fun newrepo ->
    (* we could try to be more smart:
       - only check all modified authorisations, releases, packages
       --> but if a team is modified, or an index is modified (add/remove!), we
         need to go through everything potentially affected as well:
          diff for index?
          teams may appear in authorisations, recheck all pkg where the team belongs to!

     key rollover:
      add key + sig to id/foo (merged immediately)
      janitors sign new public key || keep signing with both old and new key
      remove old key at some point

     key revocation:
      replace key + sig to id/foo
      janitors verify and sign, PR merged

     removal (how it should be done) of an id:
       id/foo <- keys = [] resources = [], signed by quorum of janitors

     removal of a release:
       packages/foo/releases <- remove item from list
       packages/foo/foo.0.2.3 <- remove directory

     removal of a package:
       packages/foo/authorisation (maybe empty)
       packages/foo/releases <- empty
*)

  (* foreach changed item, we need to ensure monotonicity (counter incremented):
     some resources may be removed (read of new is error), or be created (read of old is error)

     there is no need to verify anything again (out of scope to treat the old repo as untrusted)

     error behaviour: use maybe!
 *)

    let maybe_m = maybe repo pp_m_err () in
    (* foreach x in changes: monotonicity! *)
    foldM (fun () id ->
        match IO.read_id io id, IO.read_id newio id with
        | Ok (`Id old), Ok (`Id now) -> maybe_m (monoton_index ~old ~now repo)
        | Ok (`Team old), Ok (`Team now) -> maybe_m (monoton_team ~old ~now repo)
        | Ok (`Id old), Error _ -> maybe_m (monoton_index ~old repo)
        | Ok (`Team old), Error _ -> maybe_m (monoton_team ~old repo)
        | Ok (`Team _), Ok (`Id _)
        | Ok (`Id _), Ok (`Team _) -> Error "team/id changes unsupported"
        | Error _, Ok (`Id now) -> maybe_m (monoton_index ~now repo)
        | Error _, Ok (`Team now) -> maybe_m (monoton_team ~now repo)
        | Error _, Error _ -> maybe_m (monoton_index repo))
      () (S.elements ids) >>= fun () ->
    foldM (fun () id ->
        match IO.read_authorisation io id, IO.read_authorisation newio id with
        | Ok old, Ok now -> maybe_m (monoton_authorisation ~old ~now repo)
        | Ok old, Error _ -> maybe_m (monoton_authorisation ~old repo)
        | Error _, Ok now -> maybe_m (monoton_authorisation ~now repo)
        | Error _, Error _ -> maybe_m (monoton_authorisation repo))
      () (S.elements auths) >>= fun () ->
    foldM (fun () id ->
        match IO.read_releases io id, IO.read_releases newio id with
        | Ok old, Ok now -> maybe_m (monoton_releases ~old ~now repo)
        | Ok old, Error _ -> maybe_m (monoton_releases ~old repo)
        | Error _, Ok now -> maybe_m (monoton_releases ~now repo)
        | Error _, Error _ -> maybe_m (monoton_releases repo))
      () (S.elements rels) >>= fun () ->
    foldM (fun () id ->
        match IO.read_checksum io id, IO.read_checksum newio id with
        | Ok old, Ok now -> maybe_m (monoton_checksum ~old ~now repo)
        | Ok old, Error _ -> maybe_m (monoton_checksum ~old repo)
        | Error _, Ok now -> maybe_m (monoton_checksum ~now repo)
        | Error _, Error _ -> maybe_m (monoton_checksum repo))
      () packages >>= fun () ->
    Ok newrepo

  let verify_diff io repo data =
    let diffs = Conex_diff.to_diffs data in
    let comp = Conex_diff.diffs_to_components diffs in
    let newio = List.fold_left Conex_diff.apply io diffs in
    verify_patch repo io newio comp
end
