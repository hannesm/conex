open Conex_result
open Conex_core
open Conex_resource
open Conex_repository

let dbg = function
  | None -> (fun _ -> ())
  | Some x -> (fun d -> Format.pp_print_string x d ; Format.pp_print_newline x ())

let warn = function
  | None -> (fun m -> Format.(pp_print_string std_formatter m) ; Format.(pp_print_newline std_formatter ()))
  | Some x -> (fun m -> Format.pp_print_string x m ; Format.pp_print_newline x ())

let str pp e =
  Format.(fprintf str_formatter "%a" pp e) ;
  Format.flush_str_formatter ()

let maybe repo warn msg ok =
  if strict repo then Error msg else (warn msg ; Ok ok)

let load_id_aux dbg warn repo id =
  match read_id repo id with
  | Error e -> maybe repo warn (str pp_r_err e) (`Nothing, repo)
  | Ok (`Id idx) ->
    (dbg (str Index.pp_index idx) ;
     match verify_index repo idx with
     | Error e -> maybe repo warn (str pp_verification_error e) (`Nothing, repo)
     | Ok (repo, msgs, _) -> List.iter warn msgs ; Ok (`Id idx, repo))
  | Ok (`Team t) ->
    (dbg (str Team.pp_team t) ;
     match verify_team repo t with
     | Error e -> maybe repo warn (str pp_error e) (`Nothing, repo)
     | Ok (r, ok) -> dbg (str pp_ok ok) ; Ok (`Team t, r))

let load_id ?debug ?out repo id =
  if id_loaded repo id then
    Ok repo
  else
    let repo = add_id repo id in
    let dbg = dbg debug
    and warn = warn out
    in
    load_id_aux dbg warn repo id >>= function
    | `Id _, repo ->  Ok repo
    | `Team t, repo ->
      let load_id repo id =
        load_id_aux dbg warn repo id >>= function
        | `Nothing, repo -> Ok repo
        | `Id _, repo -> Ok repo
        | `Team t', _ -> Error ("team " ^ t'.Team.name ^ " is a member of " ^ t.Team.name ^ ", another team")
      in
      foldM load_id repo (S.elements t.Team.members)
   | `Nothing, repo -> Ok repo

let load_janitors ?(valid = fun _ _ -> true) ?debug ?out repo =
  let dbg = dbg debug
  and warn = warn out
  in
  match read_team repo "janitors" with
  | Error e -> maybe repo warn (str pp_r_err e) repo
  | Ok team ->
    dbg (str Team.pp_team team) ;
    foldM (fun acc id ->
        match read_index repo id with
        | Error e -> maybe repo warn (str pp_r_err e) acc
        | Ok idx ->
          (dbg (str Index.pp_index idx) ;
           Ok (idx :: acc)))
      [] (S.elements team.Team.members) >>= fun idxs ->
  (* for each, validate public key(s): signs the index, resource is contained *)
  let valid_idxs =
    List.fold_left (fun acc idx ->
        let id = idx.Index.name in
        let good, warnings = verify_signatures idx in
        List.iter (fun (_, e) -> warn (str pp_verification_error e)) warnings ;
        let f k = contains idx id `PublicKey (Conex_data_persistency.publickey_to_t id k) in
        match List.filter f good with
        | [] -> warn ("ignoring " ^ id ^ " due to missing self-signature") ; acc
        | xs -> dbg ("janitor index " ^ id ^ " is self-signed") ; (xs, idx) :: acc)
      [] idxs
  in
  (* filter by fingerprint, create a repository *)
  let approved, notyet =
    List.fold_left (fun (good, bad) (keys, idx) ->
        let id = idx.Index.name in
        let approved = List.filter (fun k -> valid id (Conex_nocrypto.id k)) keys in
        if List.length approved > 0 then
          (dbg ("fingerprint valid for janitor " ^ id) ;
           ((approved, idx) :: good, bad))
        else
          (dbg ("ignoring janitor " ^ id ^ ": missing valid fingerprint") ;
           (good, (keys, idx) :: bad)))
      ([], []) valid_idxs
  in
  let add_warn repo idx =
    let repo, warns = add_index repo idx in
    List.iter warn warns ;
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
          dbg ("janitor index " ^ idx.Index.name ^ " has an approved key") ;
          (idx :: good, bad)
        | Error _ ->
          dbg ("janitor index " ^ idx.Index.name ^ " has no approved key") ;
          good, (keys, idx) :: bad)
      ([], []) approved
  in
  (* verify the team janitor *)
  let good_j_repo = List.fold_left add_warn repo good_idxs in
  (match verify_team good_j_repo team with
   | Error e -> maybe repo warn (str pp_error e) good_j_repo
   | Ok (repo, ok) -> dbg ("team janitors verified " ^ str pp_ok ok) ; Ok repo) >>= fun repo ->
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
      | Ok ok -> dbg ("janitor key " ^ idx.Index.name ^ " approved by " ^ str pp_ok ok) ; Ok (idx :: acc)
      | Error e -> maybe repo warn (str pp_error e) acc)
    [] (notyet@others) >>= fun good_idx ->
  Ok (List.fold_left add_warn repo good_idx)

let verify_single_release dbg warn repo auth rel version =
  match read_checksum repo version with
  | Error e -> maybe repo warn (str pp_r_err e) ()
  | Ok cs ->
    dbg (str Checksum.pp_checksums cs) ;
    match verify_checksum repo auth rel cs with
    | Error e -> maybe repo warn (str pp_error e) ()
    | Ok ok -> dbg (str pp_ok ok) ; Ok ()

let verify_item ?(authorised = fun _authorised -> true) ?(release = fun _release -> true) ?debug ?out repo name =
  let dbg = dbg debug
  and warn = warn out
  in
  (match read_authorisation repo name with
   | Error e -> maybe repo warn (str pp_r_err e) (Authorisation.authorisation name, repo)
   | Ok auth ->
     dbg (str Authorisation.pp_authorisation auth) ;
     match verify_authorisation repo auth with
     | Error e -> maybe repo warn (str pp_error e) (auth, repo)
     | Ok ok ->
       dbg (str pp_ok ok) ;
       foldM (load_id ?debug ?out) repo (S.elements auth.Authorisation.authorised) >>= fun repo ->
       Ok (auth, repo)) >>= fun (auth, repo) ->
  (if authorised auth.Authorisation.authorised then
     (match read_releases repo name with
      | Error e ->
        (match Releases.releases ~releases:(subitems repo name) name with
         | Ok rel -> maybe repo warn (str pp_r_err e) rel
         | Error e -> Error e)
      | Ok rel ->
        dbg (str Releases.pp_releases rel) ;
        match verify_releases repo auth rel with
        | Error e -> maybe repo warn (str pp_error e) rel
        | Ok ok -> dbg (str pp_ok ok) ; Ok rel) >>= fun rel ->
     foldM (fun () n ->
         if release n then
           verify_single_release dbg warn repo auth rel n
         else
           Ok ())
       ()
       (S.elements rel.Releases.releases)
   else
     (dbg ("ignoring " ^ name) ; Ok ())) >>= fun () ->
  Ok repo

let verify_patch ?debug ?out repo newrepo (ids, auths, rels, pkgs) =
  let dbg = dbg debug
  and warn = warn out
  in
  let packages = M.fold (fun _name versions acc -> S.elements versions @ acc) pkgs [] in
  dbg (Format.sprintf "verifying a diff with %d ids %d auths %d rels %d pkgs"
         (S.cardinal ids) (S.cardinal auths) (S.cardinal rels) (List.length packages)) ;
  (* all public keys of janitors in repo are valid. *)
  load_janitors ?debug ?out repo >>= fun repo ->
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
  load_janitors ?debug ?out ~valid newrepo >>= fun newrepo ->
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
      | Error e -> maybe repo warn (str pp_m_err e) ())
    () (S.elements ids) >>= fun () ->
  foldM (fun () id ->
      match monotonicity repo newrepo `Authorisation id with
      | Ok () -> Ok ()
      | Error e -> maybe repo warn (str pp_m_err e) ())
    () (S.elements auths) >>= fun () ->
  foldM (fun () id ->
      match monotonicity repo newrepo `Releases id with
      | Ok () -> Ok ()
      | Error e -> maybe repo warn (str pp_m_err e) ())
    () (S.elements rels) >>= fun () ->
  foldM (fun () id ->
      match monotonicity repo newrepo `Checksums id with
      | Ok () -> Ok ()
      | Error e -> maybe repo warn (str pp_m_err e) ())
    () packages >>= fun () ->
  Ok newrepo

let verify_diff ?debug ?out repo data =
  let diffs = Conex_diff.to_diffs data in
  let comp = Conex_diff.diffs_to_components diffs in
  let repo' = List.fold_left Conex_diff.apply repo diffs in
  verify_patch ?debug ?out repo repo' comp
