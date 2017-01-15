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
   rollover (+change metadata)

  sign
  --> show queued pieces
  --> show changes (using git diff!?), prompt (unless -y)
  --> instruct git add&commit && open PR
*)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let help _ _copts man_format cmds = function
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let docs = Conex_opts.docs

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
    a
  in
  R.ignore_error ~use
    (Conex_repository.read_authorisation r name >>| fun a ->
     Logs.debug (fun m -> m "read %a" Authorisation.pp_authorisation a);
     a)

let find_rel r name =
  let use e =
    w Conex_repository.pp_r_err e ;
    match Releases.releases name with
    | Ok r -> Logs.info (fun m -> m "fresh %a" Releases.pp_releases r) ; r
    | Error e -> Logs.err (fun m -> m "%s" e) ; exit 1
  in
  R.ignore_error ~use
    (Conex_repository.read_releases r name >>| fun rel ->
     Logs.debug (fun m -> m "read %a" Releases.pp_releases rel) ;
     rel)

let find_idx r name =
  let use e =
    w Conex_repository.pp_r_err e ;
    let idx = Index.index name in
    Logs.info (fun m -> m "fresh %a" Index.pp_index idx) ;
    idx
  in
  R.ignore_error ~use
    (Conex_repository.read_index r name >>| fun idx ->
     Logs.debug (fun m -> m "read %a" Index.pp_index idx) ;
     idx)

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

let show_release r a rel item =
  warn_e Conex_repository.pp_r_err
    (Conex_repository.read_checksum r item >>| fun cs ->
     Logs.debug (fun m -> m "%a" Checksum.pp_checksums cs) ;
     warn_e Conex_repository.pp_error
       (Conex_repository.verify_checksum r a rel cs >>| fun ok ->
        Logs.info (fun m -> m "%a" Conex_repository.pp_ok ok)))

let show_single r showit item =
  let a = find_auth r item in
  if showit a.Authorisation.authorised then begin
    warn_e Conex_repository.pp_error
      (Conex_repository.verify_authorisation r a >>| fun ok ->
       Logs.info (fun m -> m "%a" Conex_repository.pp_ok ok)) ;
    let releases =
      let rel = find_rel r item in
      warn_e Conex_repository.pp_error
        (Conex_repository.verify_releases r a rel >>| fun ok ->
         Logs.info (fun m -> m "%a" Conex_repository.pp_ok ok)) ;
      rel
    in
    S.iter (show_release r a releases) releases.Releases.releases
  end

let self r o =
  R.error_to_msg ~pp_error:Conex_private.pp_err
    (match o.Conex_opts.id with
     | None -> Conex_private.read_private_key r >>| fst
     | Some id -> Ok id) >>| fun id ->
  Logs.debug (fun m -> m "using identifier %s" id);
  id

let status_all r o no_team =
  self r o >>= fun id ->
  R.error_to_msg ~pp_error:Conex_repository.pp_r_err
    (Conex_repository.read_id r id) >>| function
  | `Key k ->
    Logs.info (fun m -> m "%a" Publickey.pp_publickey k);
    let idx = find_idx r id in
    Logs.info (fun m -> m "%a" Index.pp_index idx);
    (* ADD queued to valid_resources! MARK key trusted! *)
    let me = s_of_list
        (if no_team then
           [id]
         else
           let teams =
             S.fold (fun id' teams ->
                 R.ignore_error
                   ~use:(fun e -> w Conex_repository.pp_r_err e ; teams)
                   (Conex_repository.read_id r id' >>| function
                     | `Team t when S.mem id t.Team.members ->
                       Logs.info (fun m -> m "member of %a" Team.pp_team t) ; t :: teams
                     | `Team _ -> teams
                     | `Key _ -> teams))
               (Conex_repository.ids r) []
           in
           id :: List.map (fun t -> t.Team.name) teams)
    in
    S.iter
      (show_single r (fun a -> not (S.is_empty (S.inter me a))))
      (Conex_repository.items r)
  | `Team t ->
    Logs.info (fun m -> m "%a" Conex_resource.Team.pp_team t);
    S.iter (show_single r (fun a -> S.mem t.Team.name a)) (Conex_repository.items r)

let status_single r _o name =
  Logs.info (fun m -> m "information on package %s" name);
  match Conex_opam_layout.authorisation_of_item name with
  | None -> show_single r (fun _ -> true) name
  | Some n ->
    let a = find_auth r n in
    (* need to load keys ++ teams *)
    warn_e Conex_repository.pp_error
      (Conex_repository.verify_authorisation r a >>| fun ok ->
       Logs.info (fun m -> m "authorisation %s %a" n Conex_repository.pp_ok ok)) ;
    let rel = find_rel r n in
    warn_e Conex_repository.pp_error
      (Conex_repository.verify_releases r a rel >>| fun ok ->
       Logs.info (fun m -> m "releases %s %a" n Conex_repository.pp_ok ok)) ;
    if not (S.mem name rel.Releases.releases) then
      Logs.warn (fun m -> m "package %s not part of releases file" name) ;
    show_release r a rel name

let status _ o name no_rec =
  let r = o.Conex_opts.repo in
  Logs.info (fun m -> m "repository %s" (Conex_repository.provider r).Provider.name) ;
  msg_to_cmdliner (if name = "" then status_all r o no_rec else Ok (status_single r o name))

let initialise r o id email =
  (match Conex_repository.read_id r id with
   | Ok (`Team _) ->
     Error (`Msg ("team " ^ id ^ " exists"))
   | Ok (`Key k) when k.Publickey.key <> None ->
     Error (`Msg ("key " ^ id ^ " exists and includes a public key"))
   | Ok (`Key k) -> Ok k
   | Error _ -> Ok (Publickey.publickey id None)) >>= fun pub ->
    let priv = Conex_nocrypto.generate () in
    if not o.Conex_opts.dry then Conex_private.write_private_key r id priv ;
    Logs.info (fun m -> m "wrote private key %s" id) ;
    str_to_msg (Conex_nocrypto.pub_of_priv priv) >>= fun public ->
    let accounts = `GitHub id :: List.map (fun e -> `Email e) email @ pub.Publickey.accounts
    and carry, counter = Uint.succ pub.Publickey.counter
    in
    if carry then
      Logs.warn (fun m -> m "counter overflow in publickey %s, needs approval" id) ;
    let pub =
      Publickey.publickey ~counter ~accounts id (Some public)
    in
    let r = Conex_repository.add_trusted_key r pub in
    if not o.Conex_opts.dry then Conex_repository.write_key r pub ;
    Logs.info (fun m -> m "wrote public key %a" Publickey.pp_publickey pub) ;
    let idx = find_idx r id in
    str_to_msg (Conex_private.sign_index idx priv) >>= fun idx ->
    if not o.Conex_opts.dry then Conex_repository.write_index r idx ;
    Logs.info (fun m -> m "wrote index %a" Index.pp_index idx) ;
    R.error_to_msg ~pp_error:pp_verification_error
       (Conex_repository.verify_index r idx >>| fun (_, _, id) ->
        Logs.info (fun m -> m "verified %s" id)) >>| fun () ->
    Logs.app (fun m -> m "Created keypair.  Please 'git add keys/%s index/%s', and submit a PR.  Join teams and claim your packages." id id)

let init _ o email =
  match o.Conex_opts.id with
  | None -> `Error (false, "please provide '--id'")
  | Some id ->
    Nocrypto_entropy_unix.initialize () ;
    let r = o.Conex_opts.repo in
    Logs.info (fun m -> m "repository %s" (Conex_repository.provider r).Provider.name) ;
    msg_to_cmdliner (initialise r o id email)

let sign _ o =
  let r = o.Conex_opts.repo in
  msg_to_cmdliner
    (R.error_to_msg ~pp_error:Conex_private.pp_err
       (Conex_private.read_private_key ?id:o.Conex_opts.id r) >>= fun (id, priv) ->
     Logs.info (fun m -> m "using private key %s" id) ;
     let idx = find_idx r id in
     match idx.Index.queued with
     | [] -> Logs.app (fun m -> m "nothing changed") ; Ok ()
     | els ->
       List.iter (fun r ->
           Logs.app (fun m -> m "adding %a" Index.pp_resource r))
         els ;
       (* XXX: PROMPT HERE *)
       str_to_msg (Conex_private.sign_index idx priv) >>| fun idx ->
       Logs.info (fun m -> m "signed index %a" Index.pp_index idx) ;
       if not o.Conex_opts.dry then Conex_repository.write_index r idx ;
       Logs.app (fun m -> m "wrote index %s to disk" id))

let reset _ o =
  let r = o.Conex_opts.repo in
  msg_to_cmdliner
    (self r o >>| fun id ->
     let idx = find_idx r id in
     List.iter (fun r ->
         Logs.app (fun m -> m "dropping %a" Index.pp_resource r))
       idx.Index.queued ;
     let idx = Index.reset idx in
     if not o.Conex_opts.dry then Conex_repository.write_index r idx ;
     Logs.app (fun m -> m "wrote index %s to disk" id))

let add_r idx name typ data =
  let counter = Index.next_id idx in
  let encoded = Conex_data.encode data in
  let size = Uint.of_int (String.length encoded) in
  let digest = Conex_nocrypto.digest encoded in
  let res = Index.r counter name size typ digest in
  Logs.info (fun m -> m "added %a to index" Index.pp_resource res) ;
  Index.add_resource idx res

let auth _ o remove members p =
  let r = o.Conex_opts.repo in
  msg_to_cmdliner
    (self r o >>| fun id ->
     let auth = find_auth r p in
     let members = match members with [] -> [id] | xs -> xs in
     let f = if remove then Authorisation.remove else Authorisation.add in
     let auth' = List.fold_left f auth members in
     if not (Authorisation.equal auth auth') then
       let auth, overflow = Authorisation.prep auth' in
       if overflow then
         Logs.warn (fun m -> m "counter overflow in authorisation %s, needs approval" p) ;
       if not o.Conex_opts.dry then Conex_repository.write_authorisation r auth ;
       Logs.info (fun m -> m "wrote %a" Authorisation.pp_authorisation auth) ;
       let idx = find_idx r id in
       let idx = add_r idx p `Authorisation (Conex_data_persistency.authorisation_to_t auth) in
       if not o.Conex_opts.dry then Conex_repository.write_index r idx ;
       Logs.app (fun m -> m "modified authorisation and added resource to your index.")
     else
       Logs.app (fun m -> m "nothing changed."))

let release _ o remove p =
  let r = o.Conex_opts.repo in
  msg_to_cmdliner
    (self r o >>= fun id ->
     let pn, pv = match Conex_opam_layout.authorisation_of_item p with
       | None -> p, `All
       | Some n -> n, `Single p
     in
     let auth = find_auth r pn in
     (* XXX this check is wrong, we need a is_authorised function taking teams into account *)
     if not (S.mem id auth.Authorisation.authorised) then
       Logs.warn (fun m -> m "not authorised to modify package %s, PR will require approval" p) ;
     let rel = find_rel r pn in
     let f m t = if remove then Releases.remove t m else Releases.add t m in
     let releases =
       match pv with
       | `All -> Conex_repository.subitems r pn
       | `Single p -> S.singleton p
     in
     let rel' = S.fold f releases rel in
     let idx = find_idx r id in
     let idx' =
       if not (Releases.equal rel rel') then
         let rel, overflow = Releases.prep rel' in
         if overflow then
           Logs.warn (fun m -> m "counter overflow in releases %s, needs approval" pn) ;
         if not (o.Conex_opts.dry) then Conex_repository.write_releases r rel ;
         Logs.info (fun m -> m "wrote %a" Releases.pp_releases rel) ;
         let idx = add_r idx pn `Releases (Conex_data_persistency.releases_to_t rel) in
         idx
       else
         idx
     in
     let add_cs name acc =
       acc >>= fun idx ->
       let cs = find_cs r name in
       R.error_to_msg ~pp_error:Conex_repository.pp_error
         (Conex_repository.compute_checksum r name) >>| fun cs' ->
       if not (Checksum.equal cs cs') then
         let cs' = Checksum.set_counter cs' cs'.Checksum.counter in
         let cs', overflow = Checksum.prep cs' in
         if overflow then Logs.warn (fun m -> m "counter overflow in checksum %s, needs approval" name) ;
         if not o.Conex_opts.dry then Conex_repository.write_checksum r cs' ;
         Logs.info (fun m -> m "wrote %a" Checksum.pp_checksums cs') ;
         add_r idx name `Checksums (Conex_data_persistency.checksums_to_t cs')
       else
         idx
     in
     (if not remove then
        S.fold add_cs releases (Ok idx')
      else
        Ok idx') >>| fun idx' ->
     if not (Index.equal idx idx') then begin
       if not o.Conex_opts.dry then Conex_repository.write_index r idx' ;
       Logs.info (fun m -> m "wrote %a" Index.pp_index idx') ;
       Logs.app (fun m -> m "released and added resources to your index.")
     end else
       Logs.app (fun m -> m "nothing happened"))

let team _ o remove members tid =
  let r = o.Conex_opts.repo in
  msg_to_cmdliner
    (self r o >>| fun id ->
     let team = find_team r tid in
     let members = match members with [] -> [id] | xs -> xs in
     let f = if remove then Team.remove else Team.add in
     let team' = List.fold_left f team members in
     if not (Team.equal team team') then
       let team, overflow = Team.prep team' in
       if overflow then
         Logs.warn (fun m -> m "counter overflow in team %s, needs approval" tid) ;
       if not o.Conex_opts.dry then Conex_repository.write_team r team ;
       Logs.info (fun m -> m "wrote %a" Team.pp_team team) ;
       let idx = find_idx r id in
       let idx = add_r idx tid `Team (Conex_data_persistency.team_to_t team) in
       if not o.Conex_opts.dry then Conex_repository.write_index r idx ;
       Logs.app (fun m -> m "modified team and added resource to your index.")
     else
       Logs.app (fun m -> m "nothing changed."))

open Cmdliner

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
  Term.(ret (const status $ setup_log $ Conex_opts.t_t $ Conex_opts.package $ noteam)),
  Term.info "status" ~doc ~man

let package_cmd =
  let doc = "modify authorisation of a package" in
  let man =
    [`S "DESCRIPTION";
     `P "Modifies authorisaton of a package."]
  in
  Term.(ret (const auth $ setup_log $ Conex_opts.t_t $ Conex_opts.remove $ Conex_opts.members $ Conex_opts.package)),
  Term.info "package" ~doc ~man

let release_cmd =
  let doc = "modify releases of a package" in
  let man =
    [`S "DESCRIPTION";
     `P "Modifies releases of a package."]
  in
  Term.(ret (const release $ setup_log $ Conex_opts.t_t $ Conex_opts.remove $ Conex_opts.package)),
  Term.info "release" ~doc ~man

let team_cmd =
  let doc = "modify members of a team" in
  let man =
    [`S "DESCRIPTION";
     `P "Modifies members of a team."]
  in
  Term.(ret (const team $ setup_log $ Conex_opts.t_t $ Conex_opts.remove $ Conex_opts.members $ team_a)),
  Term.info "team" ~doc ~man

let reset_cmd =
  let doc = "reset staged changes" in
  let man =
    [`S "DESCRIPTION";
     `P "Resets queued changes to your index."]
  in
  Term.(ret (const reset $ setup_log $ Conex_opts.t_t)),
  Term.info "reset" ~doc ~man

let sign_cmd =
  let doc = "sign staged changes" in
  let man =
    [`S "DESCRIPTION";
     `P "Cryptographically signs queued changes to your index."]
  in
  Term.(ret (const sign $ setup_log $ Conex_opts.t_t)),
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
  Term.(ret (const init $ setup_log $ Conex_opts.t_t $ emails)),
  Term.info "init" ~doc ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about conex_author" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about conex commands and other subjects..."] @ help_secs
  in
  Term.(ret
          (const help $ setup_log $ Conex_opts.t_t $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "cryptographically sign your released packages" in
  let man = help_secs in
  Term.(ret (const (fun _ _ -> `Help (`Pager, None)) $ setup_log $ Conex_opts.t_t)),
  Term.info "conex_author" ~version:"0.42.0" ~sdocs:docs ~doc ~man

let cmds = [ help_cmd ; status_cmd ;
             init_cmd ; sign_cmd ; reset_cmd ;
             package_cmd ; team_cmd ; release_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
