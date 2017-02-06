open Conex_utils
open Conex_resource

open Rresult

(* this should likely be elsewhere (conex-bells&whistles) *)
module C = Conex_api.Make(Logs)(Conex_nocrypto.V)
module CR = Conex_nocrypto.NC_R
module CS = Conex_nocrypto.NC_S

let str_to_msg = function
  | Ok x -> Ok x
  | Error s -> Error (`Msg s)

let msg_to_cmdliner = function
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

let now = match Uint.of_float (Unix.time ()) with
  | None -> invalid_arg "cannot convert now to unsigned integer"
  | Some x -> x

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
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

let init_repo ?quorum ?strict dry path =
  str_to_msg (Conex_unix_provider.(if dry then fs_ro_provider path else fs_provider path)) >>= fun prov ->
  Ok (Conex_repository.repository ?quorum ?strict (), prov)

let help _ _ _ _ _ _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let w pp a = Logs.warn (fun m -> m "%a" pp a)
let warn_e pp = R.ignore_error ~use:(w pp)

module IO = Conex_io

let self io id =
  R.error_to_msg ~pp_error:Conex_sign.pp_err
    (match id with
     | None -> Conex_sign.read_private_key io >>| fst
     | Some id -> Ok id) >>| fun id ->
  Logs.debug (fun m -> m "using identifier %s" id);
  id

let load_self_queued io r id =
  match C.load_id io r id with
  | Ok r -> r
  | Error e ->
    Logs.warn (fun m -> m "error while loading id %s" e) ;
    let idx = R.ignore_error ~use:(fun _ -> Author.t Uint.zero id) (IO.read_author io id) in
    let r, warns = Conex_repository.add_index r idx in
    List.iter (fun msg -> Logs.warn (fun m -> m "%s" msg)) warns ;
    match foldM (fun repo r -> Conex_repository.add_valid_resource repo id r) r idx.Author.queued with
    | Ok r -> r
    | Error e -> Logs.warn (fun m -> m "while adding %s" e) ; r

let status_all io r id no_team =
  R.error_to_msg ~pp_error:IO.pp_r_err (IO.read_id io id) >>= function
  | `Author _ ->
    (if no_team then
       Ok (S.singleton id)
     else begin
       str_to_msg (IO.ids io) >>= fun ids ->
       let teams =
         S.fold (fun id' teams ->
             R.ignore_error
               ~use:(fun e -> w IO.pp_r_err e ; teams)
               (IO.read_id io id' >>| function
                 | `Team t when S.mem id t.Team.members -> Logs.info (fun m -> m "member of %a" Team.pp t) ; t :: teams
                 | `Team _ | `Author _ -> teams))
           ids []
       in
       Ok (s_of_list (id :: List.map (fun t -> t.Team.name) teams))
     end) >>= fun me ->
    str_to_msg (C.load_ids ~ids:me io r) >>= fun r ->
    let authorised auth = not (S.is_empty (S.inter me auth)) in
    str_to_msg (IO.items io) >>= fun items ->
    str_to_msg (foldS (C.verify_item io ~authorised) r items)
  | `Team t ->
    Logs.info (fun m -> m "%a" Conex_resource.Team.pp t) ;
    str_to_msg (C.load_ids ~ids:t.Team.members io r) >>= fun r ->
    let authorised auth = S.mem t.Team.name auth in
    str_to_msg (IO.items io) >>= fun items ->
    str_to_msg (foldS (C.verify_item io ~authorised) r items)

let status_single io r name =
  Logs.info (fun m -> m "information on package %s" name) ;
  let pn, release = match Conex_opam_repository_layout.authorisation_of_item name with
    | None -> name, (fun _ -> true)
    | Some pn -> pn, (fun nam -> name_equal nam name)
  in
  C.verify_item ~release io r pn

let status _ dry path quorum strict id name no_rec =
  msg_to_cmdliner
    (init_repo ?quorum ~strict dry path >>= fun (r, io) ->
     Logs.info (fun m -> m "repository %s" io.Conex_provider.name) ;
     str_to_msg (C.load_janitors io r) >>= fun r ->
     self io id >>= fun id ->
     let r = load_self_queued io r id in
     if name = "" then
       status_all io r id no_rec >>| fun _r -> ()
     else
       let _ = status_single io r name in Ok ())

let add_r idx name typ data =
  let counter = Author.next_id idx in
  let encoded = Wire.to_string data in
  let size = Uint.of_int_exn (String.length encoded) in
  let digest = CR.digest encoded in
  let res = Author.r counter name size typ digest in
  Logs.info (fun m -> m "added %a to change queue" Author.pp_resource res) ;
  Author.add_resource idx res

let init _ dry path id email =
  msg_to_cmdliner
    (match id with
     | None -> Error (`Msg "please provide '--id'")
     | Some id ->
       Nocrypto_entropy_unix.initialize () ;
       init_repo ~quorum:0 dry path >>= fun (r, io) ->
       Logs.info (fun m -> m "repository %s" io.Conex_provider.name) ;
       (match IO.read_id io id with
        | Ok (`Team _) -> Error (`Msg ("team " ^ id ^ " exists"))
        | Ok (`Author idx) when List.length idx.Author.keys > 0 ->
          Error (`Msg ("key " ^ id ^ " exists and includes a public key"))
        | Ok (`Author k) ->
          Logs.info (fun m -> m "adding keys to %a" Author.pp k) ;
          Ok k
        | Error err ->
          Logs.info (fun m -> m "error reading author %s %a, creating a fresh one"
                         id Conex_io.pp_r_err err) ;
          Ok (Author.t now id)) >>= fun idx ->
       (match Conex_sign.read_private_key ~id io with
        | Ok (_, priv) ->
          let created = match priv with `Priv (_, _, c) -> c in
          Logs.info (fun m -> m "using existing private key %s (created %s)" id (Header.timestamp created)) ;
          Ok priv
        | Error _ ->
          let p = CS.generate now () in
          str_to_msg (Conex_sign.write_private_key io id p) >>| fun () ->
          Logs.info (fun m -> m "generated and wrote private key %s" id) ;
          p) >>= fun priv ->
       str_to_msg (CS.pub_of_priv priv) >>= fun public ->
       let accounts = `GitHub id :: List.map (fun e -> `Email e) email @ idx.Author.accounts
       and counter = idx.Author.counter
       and wraps = idx.Author.wraps
       and keys = public :: idx.Author.keys
       and signatures = idx.Author.signatures
       and queued = idx.Author.queued
       and resources = idx.Author.resources
       and created = idx.Author.created
       in
       let idx = Author.t ~accounts ~keys ~counter ~wraps ~resources ~signatures ~queued created id in
       let idx = add_r idx id `Key (Key.wire id public) in
       str_to_msg (CS.sign now idx priv) >>= fun idx ->
       str_to_msg (IO.write_author io idx) >>= fun () ->
       Logs.info (fun m -> m "wrote %a" Author.pp idx) ;
       R.error_to_msg ~pp_error:Conex_crypto.pp_verification_error
         (CR.verify_author r idx >>| fun (_, _, id) ->
          Logs.info (fun m -> m "verified %s" id)) >>| fun () ->
       Logs.app (fun m -> m "Created keypair.  Please 'git add id/%s', and submit a PR.  Join teams and claim your packages." id))

let find_idx io name =
  let use e =
    w IO.pp_r_err e ;
    let idx = Author.t now name in
    Logs.info (fun m -> m "fresh %a" Author.pp idx) ;
    idx
  in
  R.ignore_error ~use
    (IO.read_author io name >>| fun idx ->
     Logs.debug (fun m -> m "read %a" Author.pp idx) ;
     idx)

let sign _ dry path id =
  msg_to_cmdliner
    (init_repo dry path >>= fun (_r, io) ->
     R.error_to_msg ~pp_error:Conex_sign.pp_err
       (Conex_sign.read_private_key ?id io) >>= fun (id, priv) ->
     Logs.info (fun m -> m "using private key %s" id) ;
     let idx = find_idx io id in
     match idx.Author.queued with
     | [] -> Logs.app (fun m -> m "nothing changed") ; Ok ()
     | els ->
       List.iter (fun r ->
           Logs.app (fun m -> m "adding %a" Author.pp_resource r))
         els ;
       (* XXX: PROMPT HERE *)
       Nocrypto_entropy_unix.initialize () ;
       str_to_msg (CS.sign now idx priv) >>= fun idx ->
       Logs.info (fun m -> m "signed author %a" Author.pp idx) ;
       str_to_msg (IO.write_author io idx) >>| fun () ->
       Logs.app (fun m -> m "wrote %s to disk" id))

let reset _ dry path id =
  msg_to_cmdliner
    (init_repo dry path >>= fun (_r, io) ->
     self io id >>= fun id ->
     let idx = find_idx io id in
     List.iter
       (fun r -> Logs.app (fun m -> m "dropping %a" Author.pp_resource r))
       idx.Author.queued ;
     let idx = Author.reset idx in
     str_to_msg (IO.write_author io idx) >>| fun () ->
     Logs.app (fun m -> m "wrote %s to disk" id))

let find_auth io name =
  let use e =
    w IO.pp_r_err e ;
    let a = Authorisation.t now name in
    Logs.info (fun m -> m "fresh %a" Authorisation.pp a);
    a
  in
  R.ignore_error ~use
    (IO.read_authorisation io name >>| fun a ->
     Logs.debug (fun m -> m "read %a" Authorisation.pp a);
     a)

let auth _ dry path id remove members p =
  msg_to_cmdliner
    (init_repo dry path >>= fun (_r, io) ->
     self io id >>= fun id ->
     let auth = find_auth io p in
     let members = match members with [] -> [id] | xs -> xs in
     let f = if remove then Authorisation.remove else Authorisation.add in
     let auth' = List.fold_left f auth members in
     let idx = find_idx io id in
     if not (Authorisation.equal auth auth') then begin
       let auth, overflow = Authorisation.prep auth' in
       if overflow then
         Logs.warn (fun m -> m "counter overflow in authorisation %s, needs approval" p) ;
       str_to_msg (IO.write_authorisation io auth) >>= fun () ->
       Logs.info (fun m -> m "wrote %a" Authorisation.pp auth) ;
       let idx = add_r idx p `Authorisation (Authorisation.wire auth) in
       str_to_msg (IO.write_author io idx) >>| fun () ->
       Logs.app (fun m -> m "modified authorisation and added resource to your list.")
     end else if not (CR.contains ~queued:true idx p `Authorisation (Authorisation.wire auth)) then begin
       let idx = add_r idx p `Authorisation (Authorisation.wire auth) in
       str_to_msg (IO.write_author io idx) >>| fun () ->
       Logs.app (fun m -> m "added resource to your list.")
     end else begin
       Logs.app (fun m -> m "nothing changed.") ;
       Ok ()
     end)

let release _ dry path id remove p =
  msg_to_cmdliner
    (init_repo dry path >>= fun (r, io) ->
     self io id >>= fun id ->
     (match Conex_opam_repository_layout.authorisation_of_item p with
      | Some n -> Ok (n, S.singleton p)
      | None -> str_to_msg (IO.subitems io p) >>= fun rels -> Ok (p, rels))
     >>= fun (pn, releases) ->
     let auth = find_auth io pn in
     str_to_msg (C.load_ids ~ids:auth.Authorisation.authorised io r) >>= fun r ->
     if not (Conex_repository.authorised r auth id) then
       Logs.warn (fun m -> m "not authorised to modify package %s, PR will require approval" p) ;
     let rel =
       let use e =
         w IO.pp_r_err e ;
         Package.t now pn
       in
       R.ignore_error ~use
         (IO.read_package io pn >>| fun rel ->
          Logs.debug (fun m -> m "read %a" Package.pp rel) ;
          rel)
     in
     let f m t = if remove then Package.remove t m else Package.add t m in
     let rel' = S.fold f releases rel in
     let idx = find_idx io id in
     (if not (Package.equal rel rel') then begin
        let rel, overflow = Package.prep rel' in
        if overflow then
          Logs.warn (fun m -> m "counter overflow in releases %s, needs approval" pn) ;
        str_to_msg (IO.write_package io rel) >>| fun () ->
        Logs.info (fun m -> m "wrote %a" Package.pp rel) ;
        add_r idx pn `Package (Package.wire rel)
       end else if not (CR.contains ~queued:true idx pn `Package (Package.wire rel')) then begin
        Ok (add_r idx pn `Package (Package.wire rel))
       end else Ok idx) >>= fun idx' ->
     let add_cs name acc =
       acc >>= fun idx ->
       let cs =
         let use e =
           w IO.pp_r_err e ;
           let c = Release.t now name [] in
           Logs.info (fun m -> m "fresh %a" Release.pp c);
           c
         in
         R.ignore_error ~use
           (IO.read_release io name >>| fun cs ->
            Logs.debug (fun m -> m "read %a" Release.pp cs) ;
            cs)
       in
       R.error_to_msg ~pp_error:IO.pp_cc_err
         (IO.compute_release CR.digest io now name) >>= fun cs' ->
       if not (Release.equal cs cs') then
         let cs' = Release.set_counter cs' cs'.Release.counter in
         let cs', overflow = Release.prep cs' in
         if overflow then Logs.warn (fun m -> m "counter overflow in checksum %s, needs approval" name) ;
         str_to_msg (IO.write_release io cs') >>| fun () ->
         Logs.info (fun m -> m "wrote %a" Release.pp cs') ;
         add_r idx name `Release (Release.wire cs')
       else if not (CR.contains ~queued:true idx name `Release (Release.wire cs)) then
         Ok (add_r idx name `Release (Release.wire cs))
       else
         Ok idx
     in
     (if not remove then S.fold add_cs releases (Ok idx') else Ok idx') >>= fun idx' ->
     if not (Author.equal idx idx') then begin
       str_to_msg (IO.write_author io idx') >>| fun () ->
       Logs.info (fun m -> m "wrote %a" Author.pp idx') ;
       Logs.app (fun m -> m "released and added resources to your resource list.")
     end else
       Ok (Logs.app (fun m -> m "nothing happened")))

let team _ dry repo id remove members tid =
  msg_to_cmdliner
    (init_repo dry repo >>= fun (_r, io) ->
     self io id >>= fun id ->
     let team =
       let use e =
         w IO.pp_r_err e ;
         let team = Team.t now tid in
         Logs.info (fun m -> m "fresh %a" Team.pp team) ;
         team
       in
       R.ignore_error ~use
         (IO.read_team io tid >>| fun team ->
          Logs.debug (fun m -> m "read %a" Team.pp team) ;
          team)
     in
     let members = match members with [] -> [id] | xs -> xs in
     let f = if remove then Team.remove else Team.add in
     let team' = List.fold_left f team members in
     let idx = find_idx io id in
     if not (Team.equal team team') then begin
       let team, overflow = Team.prep team' in
       if overflow then
         Logs.warn (fun m -> m "counter overflow in team %s, needs approval" tid) ;
       str_to_msg (IO.write_team io team) >>= fun () ->
       Logs.info (fun m -> m "wrote %a" Team.pp team) ;
       let idx = add_r idx tid `Team (Team.wire team) in
       str_to_msg (IO.write_author io idx) >>| fun () ->
       Logs.app (fun m -> m "modified team and added resource to your resource list.")
     end else if not (CR.contains ~queued:true idx tid `Team (Team.wire team)) then begin
       let idx = add_r idx tid `Team (Team.wire team) in
       str_to_msg (IO.write_author io idx) >>| fun () ->
       Logs.app (fun m -> m "added resource to your resource list.")
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
     `P "Resets queued changes of your resource list."]
  in
  Term.(ret Conex_opts.(const reset $ setup_log $ dry $ repo $ id)),
  Term.info "reset" ~doc ~man

let sign_cmd =
  let doc = "sign staged changes" in
  let man =
    [`S "DESCRIPTION";
     `P "Cryptographically signs queued changes to your resource list."]
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
