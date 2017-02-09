open Conex_utils
open Conex_resource
open Conex_repository

let str pp e =
  Format.(fprintf str_formatter "%a" pp e) ;
  Format.flush_str_formatter ()

module IO = Conex_io

module Make (L : LOGS) (C : Conex_crypto.VERIFY) = struct

  let maybe repo pp res = function
    | Ok a -> Ok a
    | Error err ->
      if strict repo then Error (str pp err) else
        (L.warn (fun m -> m "%a" pp err) ; Ok res)

  let verify_and_validate repo author =
    L.debug (fun m -> m "%a" Author.pp author) ;
    match C.verify author with
    | Error e ->
      if strict repo then Error (str Conex_crypto.pp_verification_error e) else
        (L.warn (fun m -> m "%a" Conex_crypto.pp_verification_error e) ; Ok repo)
    | Ok () ->
      maybe repo pp_error repo
        (validate_author repo author >>= fun (repo, conflicts) ->
         L.info (fun m -> m "%a verified" pp_id author.Author.name) ;
         List.iter (fun c -> L.warn (fun m -> m "%a" pp_conflict c)) conflicts ;
         Ok repo)

  let load_id io repo id =
    if id_loaded repo id then begin
      L.debug (fun m -> m "%a already loaded" pp_id id) ;
      Ok repo
    end else
      match IO.read_author io id with
      | Ok idx -> verify_and_validate repo idx
      | Error e -> if strict repo then Error (str IO.pp_r_err e) else Ok repo

  let load_ids ?ids io repo =
    (match ids with
     | Some x -> Ok x
     | None -> IO.ids io) >>= fun ids ->
    let ids = S.filter (fun id -> not (id_loaded repo id)) ids in
    foldS (fun acc id ->
        maybe repo IO.pp_r_err acc (IO.read_id io id >>= fun x -> Ok (x :: acc)))
      [] ids >>= fun ids ->
    let users, teams =
      List.fold_left (fun (is, ts) -> function
          | `Author id -> id :: is, ts
          | `Team t -> is, t :: ts)
        ([], []) ids
    in
    foldM verify_and_validate repo users >>= fun repo ->
    foldM (fun repo t ->
        foldS (load_id io) repo t.Team.members >>= fun repo ->
        maybe repo pp_error repo
          (validate_team repo t >>= fun (r, ok) ->
           L.debug (fun m -> m "%a" pp_ok ok) ;
           Ok r))
      repo teams

  let load_janitors ?(valid = fun _ _ -> false) io repo =
    match IO.read_team io "janitors" with
    | Error e ->
      if strict repo then Error (str IO.pp_r_err e) else
        (L.warn (fun m -> m "%a" IO.pp_r_err e) ; Ok repo)
    | Ok team ->
      L.debug (fun m -> m "%a" Team.pp team) ;
      foldS (fun acc id ->
          maybe repo IO.pp_r_err acc
            (IO.read_author io id >>= fun idx ->
             L.debug (fun m -> m "%a" Author.pp idx) ;
             Ok (idx :: acc)))
        [] team.Team.members >>= fun idxs ->
      (* for each, validate public key(s): signs the index *)
      (* TODO: error behaviour - should we exit on verification error here? *)
      let valid_idxs =
        List.fold_left (fun acc author ->
            match C.verify author with
            | Ok () -> author :: acc
            | Error pv ->
              L.warn (fun m -> m "%a %a" Author.pp author Conex_crypto.pp_verification_error pv) ;
              acc)
          [] idxs
      in
      (* filter by fingerprint, create a repository *)
      let approved, notyet =
        List.fold_left (fun (v, i) author ->
            let id = author.Author.name in
            match
              List.filter (fun (k, _) -> valid id (C.keyid k)) author.Author.keys
            with
            | [] ->
              L.debug (fun m -> m "ignoring janitor %s: missing valid fingerprint" id) ;
              (v, author :: i)
            | _ ->
              L.debug (fun m -> m "fingerprint valid for janitor %s" id) ;
              (author :: v, i))
          ([], []) valid_idxs
      in
      let add_warn ign repo idx =
        let repo, warns = add_index repo idx in
        if not ign then
          List.iter (fun msg -> L.warn (fun m -> m "%a" pp_conflict msg)) warns ;
        repo
      in
      let jrepo = List.fold_left (add_warn true) repo approved in

      (* repo is full with all good indexes! *)
      (* validate all the authors! *)
      (* TODO: error handling: what should happen if we fail to validate some
         janitors? go further with the subset?  might actually happen in cases
         where a janitor does a key rollover, and gets signed by those not part
         of the trust anchor *)
      let good, others =
        List.fold_left (fun (v, i) author ->
            match validate_author jrepo author with
            | Ok _ -> (author :: v, i)
            | Error _ -> (v, author :: i))
          ([], []) approved
      in

      (* verify the team janitor *)
      let good_j_repo = List.fold_left (add_warn false) repo good in
      match validate_team good_j_repo team with
      | Error e -> Error (str pp_error e)
      | Ok (repo, ok) ->
        L.info (fun m -> m "team janitors verified %a" pp_ok ok) ;
        (* team is good, there may be more janitors: esp notyet and others *)
        (* load in a similar style:
           - add all the idxs
           - verify at least one key of each idx
           - insert all verified idx *)
        let rest = notyet @ others in
        let jrepo' = List.fold_left (add_warn true) repo rest in
        foldM (fun (repo, acc) author ->
            match validate_author jrepo author with
            | Ok (repo, _) ->
              L.info (fun m -> m "validated janitor %a" Author.pp author) ;
              Ok (repo, author :: acc)
            | Error e ->
              L.warn (fun m -> m "couldn't validate (%a) janitor %a" pp_error e Author.pp author) ;
              Ok (repo, acc))
          (jrepo', []) rest >>= fun (_, good) ->
        Ok (List.fold_left (add_warn false) repo good)

  let verify_single_release io repo auth rel version =
    match IO.read_release io version with
    | Error e -> if strict repo then Error (str IO.pp_r_err e) else
        (L.warn (fun m -> m "%a" IO.pp_r_err e) ; Ok ())
    | Ok cs ->
      L.debug (fun m -> m "%a" Release.pp cs) ;
      maybe repo Conex_io.pp_cc_err None
        (IO.compute_release C.raw_digest io Uint.zero version >>= fun cs ->
         Ok (Some cs)) >>= fun on_disk ->
      maybe repo pp_error ()
        (validate_release repo ?on_disk auth rel cs >>= fun ok ->
         L.debug (fun m -> m "%a" pp_ok ok) ;
         Ok ())

  (* TODO: rename "authorised"..., do digest computation inline *)
  let verify_item ?(authorised = fun _authorised -> true) ?(release = fun _release -> true) io repo name =
    (match IO.read_authorisation io name with
     | Error e -> if strict repo then Error (str IO.pp_r_err e) else
         (L.warn (fun m -> m "%a" IO.pp_r_err e) ;
          Ok (Authorisation.t Uint.zero name, repo))
     | Ok auth ->
       L.debug (fun m ->m "%a" Authorisation.pp auth) ;
       (* need to load indexes before verifying! *)
       load_ids ~ids:auth.Authorisation.authorised io repo >>= fun repo ->
       maybe repo pp_error ()
         (validate_authorisation repo auth >>= fun ok ->
          L.debug (fun m -> m "%a" pp_ok ok) ; Ok ()) >>= fun () ->
       Ok (auth, repo)) >>= fun (auth, repo) ->
    (if authorised auth.Authorisation.authorised then
       (match IO.read_package io name with
        | Error e ->
          if strict repo then Error (str IO.pp_r_err e) else
            (L.warn (fun m -> m "%a" IO.pp_r_err e) ;
             IO.compute_package io Uint.zero name)
        | Ok rel ->
          L.debug (fun m -> m "%a" Package.pp rel) ;
          (match IO.compute_package io Uint.zero name with
           | Error e -> if strict repo then Error e else
               (L.warn (fun m -> m "couldn't compute releases %s: %s" name e) ;
                Ok None)
           | Ok on_disk -> Ok (Some on_disk)) >>= fun on_disk ->
          maybe repo pp_error rel
            (validate_package repo ?on_disk auth rel >>= fun ok ->
             L.debug (fun m -> m "%a" pp_ok ok) ;
             Ok rel)) >>= fun rel ->
       foldM (fun () n ->
           if release n then
             verify_single_release io repo auth rel n
           else
             Ok ())
         ()
         (S.elements rel.Package.releases)
     else
       (L.debug (fun m -> m "ignoring %s" name) ; Ok ())) >>= fun () ->
    Ok repo

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
  let verify_patch repo io newio (ids, auths, pkgs, rels) =
    let releases = M.fold (fun _name versions acc -> S.union versions acc) rels S.empty in
    L.debug (fun m -> m "verifying a diff with %d ids %d auths %d pkgs %d rels"
                (S.cardinal ids) (S.cardinal auths) (S.cardinal pkgs) (S.cardinal releases)) ;
    (* all public keys of janitors in repo are valid. *)
    load_janitors ~valid:(fun _ _ -> true) io repo >>= fun repo ->
    let janitor_keys =
      S.fold (fun id acc ->
          match IO.read_author io id with
          | Ok idx ->
            let id (k, _) = C.keyid k in
            List.map id idx.Author.keys @ acc
          | Error _ -> acc)
        (match find_team repo "janitors" with None -> S.empty | Some x -> x)
        []
    in
    (* load all those janitors in newrepo which are valid (and verify janitors team) *)
    let valid _id key = List.exists (fun k -> Digest.equal k key) janitor_keys in
    let fresh_repo = repository ~quorum:(quorum repo) ~strict:(strict repo) (digestf repo) () in
    load_janitors ~valid newio fresh_repo >>= fun newrepo ->
    (* now we do a full verification of the new repository *)
    load_ids newio newrepo >>= fun newrepo ->
    IO.items newio >>= fun items ->
    foldS (verify_item newio) newrepo items >>= fun newrepo ->

    (* foreach changed item, we need to ensure monotonicity (counter incremented) *)
    let maybe_m = maybe repo pp_m_err () in
    foldS (fun () id ->
        match IO.read_id io id, IO.read_id newio id with
        | Ok (`Author old), Ok (`Author now) -> maybe_m (monoton_author ~old ~now repo)
        | Ok (`Team old), Ok (`Team now) -> maybe_m (monoton_team ~old ~now repo)
        | Ok (`Author old), Error _ -> maybe_m (monoton_author ~old repo)
        | Ok (`Team old), Error _ -> maybe_m (monoton_team ~old repo)
        | Ok (`Team _), Ok (`Author _)
        | Ok (`Author _), Ok (`Team _) -> Error "team/author changes unsupported"
        | Error _, Ok (`Author now) -> maybe_m (monoton_author ~now repo)
        | Error _, Ok (`Team now) -> maybe_m (monoton_team ~now repo)
        | Error _, Error _ -> maybe_m (monoton_author repo))
      () ids >>= fun () ->
    foldS (fun () id ->
        match IO.read_authorisation io id, IO.read_authorisation newio id with
        | Ok old, Ok now -> maybe_m (monoton_authorisation ~old ~now repo)
        | Ok old, Error _ -> maybe_m (monoton_authorisation ~old repo)
        | Error _, Ok now -> maybe_m (monoton_authorisation ~now repo)
        | Error _, Error _ -> maybe_m (monoton_authorisation repo))
      () auths >>= fun () ->
    foldS (fun () id ->
        match IO.read_package io id, IO.read_package newio id with
        | Ok old, Ok now -> maybe_m (monoton_package ~old ~now repo)
        | Ok old, Error _ -> maybe_m (monoton_package ~old repo)
        | Error _, Ok now -> maybe_m (monoton_package ~now repo)
        | Error _, Error _ -> maybe_m (monoton_package repo))
      () pkgs >>= fun () ->
    foldS (fun () id ->
        match IO.read_release io id, IO.read_release newio id with
        | Ok old, Ok now -> maybe_m (monoton_release ~old ~now repo)
        | Ok old, Error _ -> maybe_m (monoton_release ~old repo)
        | Error _, Ok now -> maybe_m (monoton_release ~now repo)
        | Error _, Error _ -> maybe_m (monoton_release repo))
      () releases >>= fun () ->
    Ok newrepo

  let verify_diff io repo data =
    let diffs = Conex_diff.to_diffs data in
    let comp = Conex_diff.diffs_to_components diffs in
    let newio = List.fold_left Conex_diff_provider.apply io diffs in
    verify_patch repo io newio comp
end
