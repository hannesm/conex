open Conex_core
open Conex_resource

open Rresult

let str_to_msg = function
  | Ok x -> Ok x
  | Error s -> Error (`Msg s)

let msg_to_cmdliner = function
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

(* WORKFLOW:
  init -- to get private key, public key entry

  status <package> (staged index, missing entries [for id + teams])

  staging operations [clear with "reset"]:
   team <id> [--remove] [-m (defaults to self)]
   package <name> [--remove] [-m (defaults to self)]
   release <name> [--remove]
   rollover (+change metadata) -- TODO

  sign
  --> show queued pieces
  --> show changes (using git diff!?), prompt (unless -y)
  --> instruct git add&commit && open PR

  TODO:
   - command listing all violations (which some might record into their index)
   - where to get the quorum from?

  ASSUMPTIONS
  - the local repo is a good one! (id is the one where we also trust queued)
  - local janitors team is good (no external TA)
  - only status does some verification, other commands ignore validity (but may warn)
   -- release actually checks whether id is authorised

  STATUS
  - load janitor team
  - load+verify own id (add resources + queued)
  - iterates over packages (either authorised (+team/--noteam) for, or given on command line)
  -- read auth, load authorised ids (as above), verify auth
  -- read releases, verify releases
  -- read checksums, verify checksum (includes computing it)

*)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  let reporter = Logs_fmt.reporter ~dst:Format.std_formatter () in
  Logs.set_reporter reporter ;
  (match style_renderer with
   | Some `None -> Conex_api.Log.set_styled false
   | Some `Ansi_tty -> ()
   | None -> match Conex_opts.terminal () with
     | `None -> Conex_api.Log.set_styled false
     | _ -> ()) ;
  match level with
  | Some Logs.Info -> Conex_api.Log.set_level `Info
  | Some Logs.Debug -> Conex_api.Log.set_level `Debug
  | _ -> Conex_api.Log.set_level `Warn

let init_repo ?quorum ?strict dry path =
  str_to_msg (Conex_provider.(if dry then fs_ro_provider path else fs_provider path)) >>= fun prov ->
  Ok (Conex_repository.repository ?quorum ?strict prov)

let help _ _ _ _ _ _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let w pp a = Logs.warn (fun m -> m "%a" pp a)
let warn_e pp = R.ignore_error ~use:(w pp)

let find_team r tid =
  let use e =
    w Conex_repository.pp_r_err e ;
    let team = Team.team tid in
    Logs.info (fun m -> m "fresh %a" Team.pp_team team) ;
    team
  in
  R.ignore_error ~use
    (Conex_repository.read_team r tid >>| fun team ->
     Logs.debug (fun m -> m "read %a" Team.pp_team team) ;
     team)

let find_auth r name =
  let use e =
    w Conex_repository.pp_r_err e ;
    let a = Authorisation.authorisation name in
    Logs.info (fun m -> m "fresh %a" Authorisation.pp_authorisation a);
    (a, true)
  in
  R.ignore_error ~use
    (Conex_repository.read_authorisation r name >>| fun a ->
     Logs.debug (fun m -> m "read %a" Authorisation.pp_authorisation a);
     (a, false))

let find_rel r name =
  let use e =
    w Conex_repository.pp_r_err e ;
    match Releases.releases name with
    | Ok r -> Logs.info (fun m -> m "fresh %a" Releases.pp_releases r) ; (r, true)
    | Error e -> invalid_arg e
  in
  R.ignore_error ~use
    (Conex_repository.read_releases r name >>| fun rel ->
     Logs.debug (fun m -> m "read %a" Releases.pp_releases rel) ;
     (rel, false))

let find_idx r name =
  let use e =
    w Conex_repository.pp_r_err e ;
    let idx = Index.index name in
    Logs.info (fun m -> m "fresh %a" Index.pp_index idx) ;
    (idx, true)
  in
  R.ignore_error ~use
    (Conex_repository.read_index r name >>| fun idx ->
     Logs.debug (fun m -> m "read %a" Index.pp_index idx) ;
     (idx, false))

let find_cs r name =
  let use e =
    w Conex_repository.pp_r_err e ;
    let c = Checksum.checksums name [] in
    Logs.info (fun m -> m "fresh %a" Checksum.pp_checksums c);
    c
  in
  R.ignore_error ~use
    (Conex_repository.read_checksum r name >>| fun cs ->
     Logs.debug (fun m -> m "read %a" Checksum.pp_checksums cs) ;
     cs)

let load_ids r ids =
  foldM Conex_api.load_id r (S.elements ids)

let self r id =
  R.error_to_msg ~pp_error:Conex_private.pp_err
    (match id with
     | None -> Conex_private.read_private_key r >>| fst
     | Some id -> Ok id) >>| fun id ->
  Logs.debug (fun m -> m "using identifier %s" id);
  id

let load_self_queued r id =
  let idx = R.ignore_error ~use:(fun _ -> Index.index id)
      (Conex_repository.read_index r id)
  in
  match Conex_api.load_id r id with
  | Ok r -> r
  | Error e ->
    Logs.warn (fun m -> m "error while loading id %s" e) ;
    let r, warns = Conex_repository.add_index r idx in
    List.iter (fun msg -> Logs.warn (fun m -> m "%s" msg)) warns ;
    match foldM (fun repo r -> Conex_repository.add_valid_resource repo id r) r idx.Index.queued with
    | Ok r -> r
    | Error e -> Logs.warn (fun m -> m "while adding %s" e) ; r

let status_all r id no_team =
  R.error_to_msg ~pp_error:Conex_repository.pp_r_err
    (Conex_repository.read_id r id) >>= function
  | `Id _ ->
    let me =
      s_of_list
        (if no_team then [id]
         else
           let teams =
             S.fold (fun id' teams ->
                 R.ignore_error
                   ~use:(fun e -> w Conex_repository.pp_r_err e ; teams)
                   (Conex_repository.read_id r id' >>| function
                     | `Team t when S.mem id t.Team.members -> Logs.info (fun m -> m "member of %a" Team.pp_team t) ; t :: teams
                     | `Team _ | `Id _ -> teams))
               (Conex_repository.ids r) []
           in
           id :: List.map (fun t -> t.Team.name) teams)
    in
    let authorised auth = not (S.is_empty (S.inter me auth)) in
    str_to_msg (foldM (fun r id -> Conex_api.verify_item ~authorised r id)
                  r (S.elements (Conex_repository.items r)))
  | `Team t ->
    Logs.info (fun m -> m "%a" Conex_resource.Team.pp_team t) ;
    let authorised auth = S.mem t.Team.name auth in
    str_to_msg (foldM (fun r id -> Conex_api.verify_item ~authorised r id)
                  r (S.elements (Conex_repository.items r)))

let status_single r name =
  Logs.info (fun m -> m "information on package %s" name) ;
  let pn, release = match Conex_opam_layout.authorisation_of_item name with
    | None -> name, (fun _ -> true)
    | Some pn -> pn, (fun nam -> name_equal nam name)
  in
  Conex_api.verify_item ~release r pn

let status _ dry path quorum strict id name no_rec =
  msg_to_cmdliner
    (init_repo ?quorum ~strict dry path >>= fun r ->
     Logs.info (fun m -> m "repository %s" (Conex_repository.provider r).Provider.name) ;
     str_to_msg (Conex_api.load_janitors r) >>= fun r ->
     self r id >>= fun id ->
     let r = load_self_queued r id in
     if name = "" then
       status_all r id no_rec >>| fun _r -> ()
     else
       let _ = status_single r name in Ok ())

let add_r idx name typ data =
  let counter = Index.next_id idx in
  let encoded = Conex_data.encode data in
  let size = Uint.of_int (String.length encoded) in
  let digest = Conex_nocrypto.digest encoded in
  let res = Index.r counter name size typ digest in
  Logs.info (fun m -> m "added %a to index" Index.pp_resource res) ;
  Index.add_resource idx res

let init _ dry path id email =
  msg_to_cmdliner
    (match id with
     | None -> Error (`Msg "please provide '--id'")
     | Some id ->
       Nocrypto_entropy_unix.initialize () ;
       init_repo dry path >>= fun r ->
       Logs.info (fun m -> m "repository %s" (Conex_repository.provider r).Provider.name) ;
       (match Conex_repository.read_id r id with
        | Ok (`Team _) -> Error (`Msg ("team " ^ id ^ " exists"))
        | Ok (`Id idx) when List.length idx.Index.keys > 0 ->
          Error (`Msg ("key " ^ id ^ " exists and includes a public key"))
        | Ok (`Id k) -> Ok k
        | Error _ -> Ok (Index.index id)) >>= fun idx ->
       (match Conex_private.read_private_key ~id r with
        | Ok (_, priv) -> Ok priv
        | Error _ ->
          let p = Conex_nocrypto.generate () in
          str_to_msg (Conex_private.write_private_key r id p) >>| fun () ->
          Logs.info (fun m -> m "generated and wrote private key %s" id) ;
          p) >>= fun priv ->
       str_to_msg (Conex_nocrypto.pub_of_priv priv) >>= fun public ->
       let accounts = `GitHub id :: List.map (fun e -> `Email e) email @ idx.Index.accounts
       and counter = idx.Index.counter
       and keys = public :: idx.Index.keys
       and signatures = idx.Index.signatures
       and queued = idx.Index.queued
       and resources = idx.Index.resources
       in
       let idx = Index.index ~accounts ~keys ~counter ~resources ~signatures ~queued id in
       let idx = add_r idx id `PublicKey (Wire.wire_pub id public) in
       str_to_msg (Conex_private.sign_index idx priv) >>= fun idx ->
       str_to_msg (Conex_repository.write_index r idx) >>= fun () ->
       Logs.info (fun m -> m "wrote index %a" Index.pp_index idx) ;
       R.error_to_msg ~pp_error:pp_verification_error
         (Conex_repository.verify_index r idx >>| fun (_, _, id) ->
          Logs.info (fun m -> m "verified %s" id)) >>| fun () ->
       Logs.app (fun m -> m "Created keypair.  Please 'git add id/%s', and submit a PR.  Join teams and claim your packages." id))

let sign _ dry path id =
  msg_to_cmdliner
    (init_repo dry path >>= fun r ->
     R.error_to_msg ~pp_error:Conex_private.pp_err
       (Conex_private.read_private_key ?id r) >>= fun (id, priv) ->
     Logs.info (fun m -> m "using private key %s" id) ;
     let idx, _ = find_idx r id in
     match idx.Index.queued with
     | [] -> Logs.app (fun m -> m "nothing changed") ; Ok ()
     | els ->
       List.iter (fun r ->
           Logs.app (fun m -> m "adding %a" Index.pp_resource r))
         els ;
       (* XXX: PROMPT HERE *)
       Nocrypto_entropy_unix.initialize () ;
       str_to_msg (Conex_private.sign_index idx priv) >>= fun idx ->
       Logs.info (fun m -> m "signed index %a" Index.pp_index idx) ;
       str_to_msg (Conex_repository.write_index r idx) >>| fun () ->
       Logs.app (fun m -> m "wrote index %s to disk" id))

let reset _ dry path id =
  msg_to_cmdliner
    (init_repo dry path >>= fun r ->
     self r id >>= fun id ->
     let idx, _ = find_idx r id in
     List.iter
       (fun r -> Logs.app (fun m -> m "dropping %a" Index.pp_resource r))
       idx.Index.queued ;
     let idx = Index.reset idx in
     str_to_msg (Conex_repository.write_index r idx) >>| fun () ->
     Logs.app (fun m -> m "wrote index %s to disk" id))

let auth _ dry path id remove members p =
  msg_to_cmdliner
    (init_repo dry path >>= fun r ->
     self r id >>= fun id ->
     let auth, _ = find_auth r p in
     let members = match members with [] -> [id] | xs -> xs in
     let f = if remove then Authorisation.remove else Authorisation.add in
     let auth' = List.fold_left f auth members in
     let idx, _ = find_idx r id in
     if not (Authorisation.equal auth auth') then begin
       let auth, overflow = Authorisation.prep auth' in
       if overflow then
         Logs.warn (fun m -> m "counter overflow in authorisation %s, needs approval" p) ;
       str_to_msg (Conex_repository.write_authorisation r auth) >>= fun () ->
       Logs.info (fun m -> m "wrote %a" Authorisation.pp_authorisation auth) ;
       let idx = add_r idx p `Authorisation (Authorisation.wire auth) in
       str_to_msg (Conex_repository.write_index r idx) >>| fun () ->
       Logs.app (fun m -> m "modified authorisation and added resource to your index.")
     end else if not (Conex_repository.contains ~queued:true idx p `Authorisation (Authorisation.wire auth)) then begin
       let idx = add_r idx p `Authorisation (Authorisation.wire auth) in
       str_to_msg (Conex_repository.write_index r idx) >>| fun () ->
       Logs.app (fun m -> m "added resource to your index.")
     end else begin
       Logs.app (fun m -> m "nothing changed.") ;
       Ok ()
     end)

let release _ dry path id remove p =
  msg_to_cmdliner
    (init_repo dry path >>= fun r ->
     self r id >>= fun id ->
     let pn, releases = match Conex_opam_layout.authorisation_of_item p with
       | None -> p, Conex_repository.subitems r p
       | Some n -> n, S.singleton p
     in
     let auth, _ = find_auth r pn in
     str_to_msg (load_ids r auth.Authorisation.authorised) >>= fun r ->
     if not (Conex_repository.authorised r auth id) then
       Logs.warn (fun m -> m "not authorised to modify package %s, PR will require approval" p) ;
     let rel, _ = find_rel r pn in
     let f m t = if remove then Releases.remove t m else Releases.add t m in
     let rel' = S.fold f releases rel in
     let idx, _ = find_idx r id in
     (if not (Releases.equal rel rel') then begin
        let rel, overflow = Releases.prep rel' in
        if overflow then
          Logs.warn (fun m -> m "counter overflow in releases %s, needs approval" pn) ;
        str_to_msg (Conex_repository.write_releases r rel) >>| fun () ->
        Logs.info (fun m -> m "wrote %a" Releases.pp_releases rel) ;
        add_r idx pn `Releases (Releases.wire rel)
       end else if not (Conex_repository.contains ~queued:true idx pn `Releases (Releases.wire rel')) then begin
        Ok (add_r idx pn `Releases (Releases.wire rel))
       end else Ok idx) >>= fun idx' ->
     let add_cs name acc =
       acc >>= fun idx ->
       let cs = find_cs r name in
       R.error_to_msg ~pp_error:Conex_repository.pp_error
         (Conex_repository.compute_checksum r name) >>= fun cs' ->
       if not (Checksum.equal cs cs') then
         let cs' = Checksum.set_counter cs' cs'.Checksum.counter in
         let cs', overflow = Checksum.prep cs' in
         if overflow then Logs.warn (fun m -> m "counter overflow in checksum %s, needs approval" name) ;
         str_to_msg (Conex_repository.write_checksum r cs') >>| fun () ->
         Logs.info (fun m -> m "wrote %a" Checksum.pp_checksums cs') ;
         add_r idx name `Checksums (Checksum.wire cs')
       else if not (Conex_repository.contains ~queued:true idx name `Checksums (Checksum.wire cs)) then
         Ok (add_r idx name `Checksums (Checksum.wire cs))
       else
         Ok idx
     in
     (if not remove then S.fold add_cs releases (Ok idx') else Ok idx') >>= fun idx' ->
     if not (Index.equal idx idx') then begin
       str_to_msg (Conex_repository.write_index r idx') >>| fun () ->
       Logs.info (fun m -> m "wrote %a" Index.pp_index idx') ;
       Logs.app (fun m -> m "released and added resources to your index.")
     end else
       Ok (Logs.app (fun m -> m "nothing happened")))

let team _ dry repo id remove members tid =
  msg_to_cmdliner
    (init_repo dry repo >>= fun r ->
     self r id >>= fun id ->
     let team = find_team r tid in
     let members = match members with [] -> [id] | xs -> xs in
     let f = if remove then Team.remove else Team.add in
     let team' = List.fold_left f team members in
     let idx, _ = find_idx r id in
     if not (Team.equal team team') then begin
       let team, overflow = Team.prep team' in
       if overflow then
         Logs.warn (fun m -> m "counter overflow in team %s, needs approval" tid) ;
       str_to_msg (Conex_repository.write_team r team) >>= fun () ->
       Logs.info (fun m -> m "wrote %a" Team.pp_team team) ;
       let idx = add_r idx tid `Team (Team.wire team) in
       str_to_msg (Conex_repository.write_index r idx) >>| fun () ->
       Logs.app (fun m -> m "modified team and added resource to your index.")
     end else if not (Conex_repository.contains ~queued:true idx tid `Team (Team.wire team)) then begin
       let idx = add_r idx tid `Team (Team.wire team) in
       str_to_msg (Conex_repository.write_index r idx) >>| fun () ->
       Logs.app (fun m -> m "added resource to your index.")
     end else begin
       Logs.app (fun m -> m "nothing changed.") ;
       Ok ()
     end)


open Cmdliner

let docs = Conex_opts.docs

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ~docs ()
        $ Logs_cli.level ~docs ())

let help_secs = [
 `S "GENERAL";
 `P "Conex is a tool suite to manage and verify cryptographically signed data repositories.";
 `S "KEYS";
 `P "Public keys contain a unique identifier.  They are distributed with the repository itself.";
 `P "Your private keys are stored PEM-encoded in ~/.conex.  You can select which key to use by passing --id to conex.";
 `S "PACKAGE";
 `P "A package is an opam package, and can either be the package name or the package name and version number.";
 `S docs;
 `P "These options are common to all commands.";
 `S "SEE ALSO";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
 `S "BUGS"; `P "Check bug reports at https://github.com/hannesm/conex.";]

let team_a =
  let doc = "Team" in
  Arg.(value & pos 0 Conex_opts.id_c "" & info [] ~docv:"TEAM" ~doc)

let status_cmd =
  let noteam =
    let doc = "Do not transitively use teams" in
    Arg.(value & flag & info ["noteam"] ~docs ~doc)
  in
  let doc = "information about yourself" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about yourself."]
  in
  Term.(ret Conex_opts.(const status $ setup_log $ dry $ repo $ quorum $ strict $ id $ package $ noteam)),
  Term.info "status" ~doc ~man

let package_cmd =
  let doc = "modify authorisation of a package" in
  let man =
    [`S "DESCRIPTION";
     `P "Modifies authorisaton of a package."]
  in
  Term.(ret Conex_opts.(const auth $ setup_log $ dry $ repo $ id $ remove $ members $ package)),
  Term.info "package" ~doc ~man

let release_cmd =
  let doc = "modify releases of a package" in
  let man =
    [`S "DESCRIPTION";
     `P "Modifies releases of a package."]
  in
  Term.(ret Conex_opts.(const release $ setup_log $ dry $ repo $ id $ remove $ package)),
  Term.info "release" ~doc ~man

let team_cmd =
  let doc = "modify members of a team" in
  let man =
    [`S "DESCRIPTION";
     `P "Modifies members of a team."]
  in
  Term.(ret Conex_opts.(const team $ setup_log $ dry $ repo $ id $ remove $ members $ team_a)),
  Term.info "team" ~doc ~man

let reset_cmd =
  let doc = "reset staged changes" in
  let man =
    [`S "DESCRIPTION";
     `P "Resets queued changes to your index."]
  in
  Term.(ret Conex_opts.(const reset $ setup_log $ dry $ repo $ id)),
  Term.info "reset" ~doc ~man

let sign_cmd =
  let doc = "sign staged changes" in
  let man =
    [`S "DESCRIPTION";
     `P "Cryptographically signs queued changes to your index."]
  in
  Term.(ret Conex_opts.(const sign $ setup_log $ dry $ repo $ id)),
  Term.info "sign" ~doc ~man

let init_cmd =
  let emails =
    let doc = "Pass set of email addresses" in
    Arg.(value & opt_all string [] & info ["email"] ~docs ~doc)
  in
  let doc = "initialise" in
  let man =
    [`S "DESCRIPTION";
     `P "Generates a private key and conex key entry."]
  in
  Term.(ret Conex_opts.(const init $ setup_log $ dry $ repo $ id $ emails)),
  Term.info "init" ~doc ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about conex_author" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about conex commands and subcommands"] @ help_secs
  in
  Term.(ret Conex_opts.(const help $ setup_log $ dry $ repo $ quorum $ strict $ id $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "cryptographically sign your released packages" in
  let man = help_secs in
  Term.(ret Conex_opts.(const help $ setup_log $ dry $ repo $ quorum $ strict $ id $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "conex_author" ~version:"0.42.0" ~sdocs:docs ~doc ~man

let cmds = [ help_cmd ; status_cmd ;
             init_cmd ; sign_cmd ; reset_cmd ;
             package_cmd ; team_cmd ; release_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
