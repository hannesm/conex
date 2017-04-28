open Conex_utils
open Conex_resource
open Conex_repository

let str pp e =
  Format.(fprintf str_formatter "%a" pp e) ;
  Format.flush_str_formatter ()

module IO = Conex_io

module Make (L : LOGS) (C : Conex_verify.S) = struct

  let to_str pp = function
    | Ok a -> Ok a
    | Error e -> Error (str pp e)

  let verify_and_validate repo author =
    L.debug (fun m -> m "%a" Author.pp author) ;
    to_str Conex_verify.pp_error (C.verify author) >>= fun () ->
    to_str pp_error (validate_author repo author) >>= fun repo ->
    L.info (fun m -> m "%a verified" pp_id author.Author.name) ;
    Ok repo

  let verify_id io repo id =
    if id_loaded repo id then begin
      L.debug (fun m -> m "%a already loaded" pp_id id) ;
      Ok repo
    end else
      to_str IO.pp_r_err (IO.read_author io id) >>= fun author ->
      verify_and_validate repo author

  let verify_ids ?ids io repo =
    (match ids with
     | Some x -> Ok x
     | None -> IO.ids io) >>= fun ids ->
    let ids = S.filter (fun id -> not (id_loaded repo id)) ids in
    foldS (fun acc id -> to_str IO.pp_r_err (IO.read_id io id) >>= fun x ->
            Ok (x :: acc))
      [] ids >>= fun ids ->
    let users, teams =
      List.fold_left (fun (is, ts) -> function
          | `Author id -> id :: is, ts
          | `Team t -> is, t :: ts)
        ([], []) ids
    in
    foldM verify_and_validate repo users >>= fun repo ->
    foldM (fun repo t ->
        foldS (verify_id io) repo t.Team.members >>= fun repo ->
        to_str pp_error (validate_team repo t) >>= fun (r, ok) ->
        L.info (fun m -> m "%a verified %a" Team.pp t pp_ok ok) ;
        Ok r)
      repo teams

  let verify_janitors ?(valid = fun _ _ -> false) io repo =
    to_str IO.pp_r_err (IO.read_team io "janitors") >>= fun team ->
    L.debug (fun m -> m "read %a" Team.pp team) ;
    foldS (fun acc id ->
        to_str IO.pp_r_err (IO.read_author io id) >>= fun idx ->
        L.debug (fun m -> m "read %a" Author.pp idx) ;
        Ok (idx :: acc))
      [] team.Team.members >>= fun idxs ->

    (* for each, validate public key(s): signs the index *)
    foldM (fun acc author ->
        to_str Conex_verify.pp_error (C.verify author) >>= fun () ->
        L.info (fun m -> m "verified %a" Author.pp author) ;
        Ok (author :: acc))
      [] idxs >>= fun valid_idxs ->

    (* filter by fingerprint, create a repository *)
    let approved, notyet =
      List.fold_left (fun (v, i) author ->
          let id = author.Author.name in
          match
            List.filter (fun (k, _) -> valid id (C.keyid id k)) author.Author.keys
          with
          | [] ->
            L.debug (fun m -> m "ignoring janitor %s: missing valid fingerprint" id) ;
            (v, author :: i)
          | _ ->
            L.debug (fun m -> m "fingerprint valid for janitor %s" id) ;
            (author :: v, i))
        ([], []) valid_idxs
    in

    to_str pp_conflict (foldM add_index repo approved) >>= fun jrepo ->
    let jrepo = add_team jrepo team in

    (* jrepo is full with all good indexes! *)
    (* validate all the authors! *)
    let good, others =
      List.fold_left (fun (v, i) author ->
          match validate_author jrepo author with
          | Ok _ -> L.debug (fun m -> m "author %s is valid" author.Author.name) ; (author :: v, i)
          | Error e -> L.debug (fun m -> m "author %s invalid: %a" author.Author.name pp_error e) ; (v, author :: i))
        ([], []) approved
    in

    let add_author repo author =
      add_index repo author >>= fun r ->
      Ok (add_id r author.Author.name)
    in
    (* verify the team janitor *)
    let trepo = add_team repo team in
    to_str pp_conflict (foldM add_author trepo good) >>= fun good_j_repo ->
    to_str pp_error (validate_team good_j_repo team) >>= fun (repo, ok) ->
    L.info (fun m -> m "team janitors verified %a" pp_ok ok) ;
    (* team is good, there may be more janitors: esp notyet and others *)
    (* load in a similar style:
       - add all the idxs
       - verify at least one key of each idx
       - insert all verified idx *)
    let rest = notyet @ others in
    to_str pp_conflict (foldM add_index repo rest) >>= fun jrepo' ->
    to_str pp_error (foldM (fun repo author ->
        validate_author repo author >>= fun repo ->
        L.info (fun m -> m "successfully verified janitor %a" Author.pp author) ;
        Ok repo) jrepo' rest)

  let verify_single_release io repo auth rel version =
    to_str IO.pp_r_err (IO.read_checksums io version) >>= fun cs ->
    L.debug (fun m -> m "%a" Checksums.pp cs) ;
    to_str Conex_io.pp_cc_err
      (IO.compute_checksums C.raw_digest io Uint.zero version) >>= fun on_disk ->
    to_str pp_error (validate_checksums repo ~on_disk auth rel cs) >>= fun ok ->
    L.debug (fun m -> m "%a" pp_ok ok) ;
    Ok ()

  let verify_package ?(ignore_missing = false) io repo name =
    match IO.read_authorisation io name with
    | Error e ->
      if ignore_missing then
        (L.warn (fun m -> m "ignoring %a %a" pp_name name IO.pp_r_err e) ;
         Ok repo)
      else
        Error (str IO.pp_r_err e)
    | Ok auth ->
      L.debug (fun m ->m "%a" Authorisation.pp auth) ;
      (* need to load indexes before verifying! *)
      verify_ids ~ids:auth.Authorisation.authorised io repo >>= fun repo ->
      to_str pp_error (validate_authorisation repo auth) >>= fun ok ->
      L.debug (fun m -> m "validated %a %a" pp_ok ok Authorisation.pp auth) ;
      match IO.read_releases io name with
      | Error e ->
        if ignore_missing then
          (L.warn (fun m -> m "ignoring package index %a %a" pp_name name IO.pp_r_err e) ;
           Ok repo)
        else
          Error (str IO.pp_r_err e)
      | Ok rel ->
        L.debug (fun m -> m "%a" Releases.pp rel) ;
        IO.compute_releases io Uint.zero name >>= fun on_disk ->
        to_str pp_error (validate_releases repo ~on_disk auth rel) >>= fun ok ->
        L.debug (fun m -> m "validated %a %a" pp_ok ok Releases.pp rel) ;
        foldM
          (fun () n -> verify_single_release io repo auth rel n)
          ()
          (S.elements rel.Releases.versions) >>= fun () ->
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
       packages/foo/releases <- remove package from list
       packages/foo/foo.0.2.3 <- remove directory

     removal of a package:
       packages/foo/authorisation (maybe empty)
       packages/foo/releases <- empty
*)
  let verify_patch ?ignore_missing repo io newio (ids, auths, pkgs, rels) =
    let releases = M.fold (fun _name versions acc -> S.union versions acc) rels S.empty in
    L.debug (fun m -> m "verifying a diff with %d ids %d auths %d pkgs %d rels"
                (S.cardinal ids) (S.cardinal auths) (S.cardinal pkgs) (S.cardinal releases)) ;
    (* all public keys of janitors in repo are valid. *)
    verify_janitors ~valid:(fun _ _ -> true) io repo >>= fun repo ->
    let janitor_keys =
      S.fold (fun id acc ->
          match IO.read_author io id with
          | Ok idx ->
            let id (k, _) = C.keyid id k in
            List.map id idx.Author.keys @ acc
          | Error _ -> acc)
        (match find_team repo "janitors" with None -> S.empty | Some x -> x)
        []
    in
    (* load all those janitors in newrepo which are valid (and verify janitors team) *)
    let valid _id key = List.exists (fun k -> Digest.equal k key) janitor_keys in
    let fresh_repo = repository ~quorum:(quorum repo) (digestf repo) () in
    verify_janitors ~valid newio fresh_repo >>= fun newrepo ->
    (* now we do a full verification of the new repository *)
    verify_ids newio newrepo >>= fun newrepo ->
    IO.packages newio >>= fun packages ->
    foldS (verify_package ?ignore_missing newio) newrepo packages >>= fun newrepo ->

    (* foreach changed resource, we need to ensure monotonicity (counter incremented) *)
    let maybe_m = to_str pp_m_err in
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
        match IO.read_releases io id, IO.read_releases newio id with
        | Ok old, Ok now -> maybe_m (monoton_releases ~old ~now repo)
        | Ok old, Error _ -> maybe_m (monoton_releases ~old repo)
        | Error _, Ok now -> maybe_m (monoton_releases ~now repo)
        | Error _, Error _ -> maybe_m (monoton_releases repo))
      () pkgs >>= fun () ->
    foldS (fun () id ->
        match IO.read_checksums io id, IO.read_checksums newio id with
        | Ok old, Ok now -> maybe_m (monoton_checksums ~old ~now repo)
        | Ok old, Error _ -> maybe_m (monoton_checksums ~old repo)
        | Error _, Ok now -> maybe_m (monoton_checksums ~now repo)
        | Error _, Error _ -> maybe_m (monoton_checksums repo))
      () releases >>= fun () ->
    Ok newrepo

  let verify_diff ?ignore_missing io repo data =
    let diffs = Conex_diff.to_diffs data in
    let comp = Conex_diff.diffs_to_components diffs in
    let newio = List.fold_left Conex_diff_provider.apply io diffs in
    verify_patch ?ignore_missing repo io newio comp
end
