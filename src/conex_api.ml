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

module Make (L : LOGS) = struct

  let load_id_aux repo id =
    match read_id repo id with
    | Error e ->
      if strict repo then Error (str pp_r_err e) else
        (L.warn (fun m -> m "%a" pp_r_err e) ; Ok (`Nothing, repo))
    | Ok (`Id idx) ->
      (L.debug (fun m -> m "%a" Index.pp_index idx) ;
       match verify_index repo idx with
       | Error e -> if strict repo then Error (str pp_verification_error e) else
           (L.warn (fun m -> m "%a" pp_verification_error e) ; Ok (`Nothing, repo))
       | Ok (repo, msgs, _) -> List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) msgs ; Ok (`Id idx, repo))
    | Ok (`Team t) ->
      (L.debug (fun m -> m "%a" Team.pp_team t) ;
       match verify_team repo t with
       | Error e -> if strict repo then Error (str pp_error e) else
           (L.warn (fun m -> m "%a" pp_error e) ; Ok (`Nothing, repo))
       | Ok (r, ok) -> L.debug (fun m -> m "%a" pp_ok ok) ; Ok (`Team t, r))

  let load_id repo id =
    if id_loaded repo id then
      Ok repo
    else
      let repo = add_id repo id in
      load_id_aux repo id >>= function
      | `Id _, repo ->  Ok repo
      | `Team t, repo ->
        let load_id repo id =
          load_id_aux repo id >>= function
          | `Nothing, repo -> Ok repo
          | `Id _, repo -> Ok repo
          | `Team t', _ -> Error ("team " ^ t'.Team.name ^ " is a member of " ^ t.Team.name ^ ", another team")
        in
        foldM load_id repo (S.elements t.Team.members)
      | `Nothing, repo -> Ok repo

  let load_janitors ?(valid = fun _ _ -> true) repo =
    match read_team repo "janitors" with
    | Error e -> if strict repo then Error (str pp_r_err e) else
        (L.warn (fun m -> m "%a" pp_r_err e) ; Ok repo)
    | Ok team ->
      L.debug (fun m -> m "%a" Team.pp_team team) ;
      foldM (fun acc id ->
          match read_index repo id with
          | Error e -> if strict repo then Error (str pp_r_err e) else
              (L.warn (fun m -> m "%a" pp_r_err e) ; Ok acc)
          | Ok idx ->
            L.debug (fun m -> m "%a" Index.pp_index idx) ;
            Ok (idx :: acc))
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
      (match verify_team good_j_repo team with
       | Error e -> if strict repo then Error (str pp_error e) else
           (L.warn (fun m -> m "%a" pp_error e) ; Ok (add_team good_j_repo team))
       | Ok (repo, ok) -> L.debug (fun m -> m "team janitors verified %a" pp_ok ok) ; Ok repo) >>= fun repo ->
      (* team is good, there may be more janitors: esp notyet and others *)
      (* load in a similar style:
         - add all the idxs
         - verify at least one key of each idx
         - insert all verified idx *)
      let jrepo' = List.fold_left add_warn repo (List.map snd (notyet@others)) in
      foldM (fun acc (keys, idx) ->
          match
            List.fold_left (fun r k ->
                match r, verify_key jrepo' idx.Index.name k with
                | Ok x, _ -> Ok x
                | Error _, x -> x)
              (Error (`NotSigned (idx.Index.name, `PublicKey, S.empty)))
              keys
          with
          | Ok ok -> L.debug (fun m -> m "janitor key %s approved by %a" idx.Index.name pp_ok ok) ; Ok (idx :: acc)
          | Error e -> if strict repo then Error (str pp_error e) else
              (L.warn (fun m -> m "%a" pp_error e) ; Ok acc))
        [] (notyet@others) >>= fun good_idx ->
      Ok (List.fold_left add_warn repo good_idx)

  let verify_single_release repo auth rel version =
    match read_checksum repo version with
    | Error e -> if strict repo then Error (str pp_r_err e) else
        (L.warn (fun m -> m "%a" pp_r_err e) ; Ok ())
    | Ok cs ->
      L.debug (fun m -> m "%a" Checksum.pp_checksums cs) ;
      match verify_checksum repo auth rel cs with
      | Error e -> if strict repo then Error (str pp_error e) else
          (L.warn (fun m -> m "%a" pp_error e) ; Ok ())
      | Ok ok -> L.debug (fun m -> m "%a" pp_ok ok) ; Ok ()

  let verify_item ?(authorised = fun _authorised -> true) ?(release = fun _release -> true) repo name =
    (match read_authorisation repo name with
     | Error e -> if strict repo then Error (str pp_r_err e) else
         (L.warn (fun m -> m "%a" pp_r_err e) ;
          Ok (Authorisation.authorisation name, repo))
     | Ok auth ->
       L.debug (fun m ->m "%a" Authorisation.pp_authorisation auth) ;
       match verify_authorisation repo auth with
       | Error e -> if strict repo then Error (str pp_error e) else
           (L.warn (fun m ->m "%a" pp_error e) ; Ok (auth, repo))
       | Ok ok ->
         L.debug (fun m -> m "%a" pp_ok ok) ;
         foldM load_id repo (S.elements auth.Authorisation.authorised) >>= fun repo ->
         Ok (auth, repo)) >>= fun (auth, repo) ->
    (if authorised auth.Authorisation.authorised then
       (match read_releases repo name with
        | Error e ->
          (match Releases.releases ~releases:(subitems repo name) name with
           | Ok rel -> if strict repo then Error (str pp_r_err e) else
               (L.warn (fun m -> m "%a" pp_r_err e) ; Ok rel)
           | Error e -> Error e)
        | Ok rel ->
          L.debug (fun m -> m "%a" Releases.pp_releases rel) ;
          match verify_releases repo auth rel with
          | Error e -> if strict repo then Error (str pp_error e) else
              (L.warn (fun m -> m "%a" pp_error e) ; Ok rel)
          | Ok ok -> L.debug (fun m -> m "%a" pp_ok ok) ; Ok rel) >>= fun rel ->
       foldM (fun () n ->
           if release n then
             verify_single_release repo auth rel n
           else
             Ok ())
         ()
         (S.elements rel.Releases.releases)
     else
       (L.debug (fun m -> m "ignoring %s" name) ; Ok ())) >>= fun () ->
    Ok repo

  let verify_patch repo newrepo (ids, auths, rels, pkgs) =
    let packages = M.fold (fun _name versions acc -> S.elements versions @ acc) pkgs [] in
    L.debug (fun m -> m "verifying a diff with %d ids %d auths %d rels %d pkgs"
                (S.cardinal ids) (S.cardinal auths) (S.cardinal rels) (List.length packages)) ;
    (* all public keys of janitors in repo are valid. *)
    load_janitors repo >>= fun repo ->
    let janitor_keys =
      S.fold (fun id acc ->
          match read_index repo id with
          | Ok idx -> S.union acc (s_of_list (List.map Conex_nocrypto.id idx.Index.keys))
          | Error _ -> acc)
        (match find_team repo "janitors" with None -> S.empty | Some x -> x)
        S.empty
    in
    (* load all those janitors in newrepo which are valid (and verify janitors team) *)
    let valid _id key = S.mem key janitor_keys in
    load_janitors ~valid newrepo >>= fun newrepo ->
    (* now we do a full verification of the new repository *)
    foldM verify_item newrepo (S.elements (items newrepo)) >>= fun newrepo ->
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

    (* foreach id in ids: monotonicity! *)
    foldM (fun () id ->
        match monotonicity repo newrepo `Index id with
        | Ok () -> Ok ()
        | Error e -> if strict repo then Error (str pp_m_err e) else
            (L.warn (fun m -> m "%a" pp_m_err e) ; Ok ()))
      () (S.elements ids) >>= fun () ->
    foldM (fun () id ->
        match monotonicity repo newrepo `Authorisation id with
        | Ok () -> Ok ()
        | Error e -> if strict repo then Error (str pp_m_err e) else
            (L.warn (fun m -> m "%a" pp_m_err e) ; Ok ()))
      () (S.elements auths) >>= fun () ->
    foldM (fun () id ->
        match monotonicity repo newrepo `Releases id with
        | Ok () -> Ok ()
        | Error e -> if strict repo then Error (str pp_m_err e) else
            (L.warn (fun m -> m "%a" pp_m_err e) ; Ok ()))
      () (S.elements rels) >>= fun () ->
    foldM (fun () id ->
        match monotonicity repo newrepo `Checksums id with
        | Ok () -> Ok ()
        | Error e -> if strict repo then Error (str pp_m_err e) else
            (L.warn (fun m -> m "%a" pp_m_err e) ; Ok ()))
      () packages >>= fun () ->
    Ok newrepo

  let verify_diff repo data =
    let diffs = Conex_diff.to_diffs data in
    let comp = Conex_diff.diffs_to_components diffs in
    let repo' = List.fold_left Conex_diff.apply repo diffs in
    verify_patch repo repo' comp
end
