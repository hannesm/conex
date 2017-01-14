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

  info [[package] <name>]

  staging operations [clear with "reset"]:
   team <id> create|join|leave (takes additional ids with -m[embers], defaults to self)
   package <name> claim|unclaim|remove|add[default]
   rollover (+change metadata)

  sign
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

let docs = Conex_opts.s

let w pp a = Logs.warn (fun m -> m "%a" pp a)
let warn_e pp = R.ignore_error ~use:(w pp)

let find_auth r name =
  let use e =
    w Conex_repository.pp_r_err e ;
    let a = Authorisation.authorisation name in
    Logs.info (fun m -> m "fresh %a" Authorisation.pp_authorisation a);
    a
  in
  R.ignore_error ~use
    (Conex_repository.read_authorisation r name >>| fun a ->
     Logs.debug (fun m -> m "%a" Authorisation.pp_authorisation a);
     a)

let find_rel r name =
  let use e =
    w Conex_repository.pp_r_err e ;
    match Releases.releases ~releases:(Conex_repository.subitems r name) name with
    | Ok r -> Logs.info (fun m -> m "fresh %a" Releases.pp_releases r) ; r
    | Error e -> Logs.err (fun m -> m "%s" e) ; exit 1
  in
  R.ignore_error ~use
    (Conex_repository.read_releases r name >>| fun rel ->
     Logs.debug (fun m -> m "%a" Releases.pp_releases rel) ;
     rel)

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
  Conex_private.(R.error_to_msg ~pp_error:pp_err
                   (match o.Conex_opts.id with
                    | None -> read_private_key r >>| fst
                    | Some id -> Ok id)) >>| fun id ->
  Logs.debug (fun m -> m "using identifier %s" id);
  id

let info_all r o =
  self r o >>= fun id ->
  Conex_repository.(R.error_to_msg ~pp_error:pp_r_err (Conex_repository.read_id r id)) >>= function
  | `Key k ->
    Logs.info (fun m -> m "%a" Publickey.pp_publickey k);
    let idx =
      R.ignore_error
        ~use:(fun e -> w Conex_repository.pp_r_err e ; Index.index id)
          (Conex_repository.read_index r id)
    in
    Logs.info (fun m -> m "%a" Index.pp_index idx);
    let me = s_of_list @@ if o.Conex_opts.team then
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
        id :: List.map (fun t -> t.Team.name) teams
      else
        [id]
    in
    S.iter
      (show_single r (fun a -> not (S.is_empty (S.inter me a))))
      (Conex_repository.items r) ;
    Ok ()
  | `Team t ->
    Logs.info (fun m -> m "%a" Conex_resource.Team.pp_team t);
    S.iter (show_single r (fun a -> S.mem t.Team.name a)) (Conex_repository.items r) ;
    Ok ()

let info_single r _o name =
  Logs.info (fun m -> m "information on package %s" name);
  match Conex_opam_layout.authorisation_of_item name with
  | None -> show_single r (fun _ -> true) name ; Ok ()
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
    show_release r a rel name ;
    Ok ()

let information _ o name =
  let r = o.Conex_opts.repo in
  Logs.info (fun m -> m "repository %s" (Conex_repository.provider r).Provider.name) ;
  msg_to_cmdliner (if name = "" then info_all r o else info_single r o name)

let initialise r o id email =
  match Conex_repository.read_id r id with
  | Ok (`Team _) ->
    Error (`Msg ("team " ^ id ^ " exists"))
  | Ok (`Key k) when k.Publickey.key <> None ->
    Error (`Msg ("key " ^ id ^ " exists and includes a public key"))
  | _ ->
    let priv = Conex_nocrypto.generate () in
    if not o.Conex_opts.dry then
      Conex_private.write_private_key r id priv ;
    Logs.info (fun m -> m "wrote private key %s" id) ;
    str_to_msg (Conex_nocrypto.pub_of_priv priv) >>= fun pub ->
    (* XXX: need to validate identifier *)
    (* XXX: need to validate all email addresses *)
    let accounts = `GitHub id :: List.map (fun e -> `Email e) email in
    let pub = Publickey.publickey ~accounts id (Some pub) in
    let r = Conex_repository.add_trusted_key r pub in
    if not o.Conex_opts.dry then
      Conex_repository.write_key r pub ;
    Logs.info (fun m -> m "wrote public key %a" Publickey.pp_publickey pub) ;
    let idx = Index.index id in
    str_to_msg (Conex_private.sign_index idx priv) >>= fun idx ->
    if not o.Conex_opts.dry then
      Conex_repository.write_index r idx ;
    Logs.info (fun m -> m "wrote index %a" Index.pp_index idx) ;
    R.error_to_msg ~pp_error:pp_verification_error
       (Conex_repository.verify_index r idx >>| fun (_, _, id) ->
        Logs.info (fun m -> m "verified %s" id)) >>| fun () ->
    Logs.info (fun m -> m "please now git add keys/%s and index/%s, and claim some packages" id id)

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
    (Conex_private.(R.error_to_msg ~pp_error:pp_err
                      (match o.Conex_opts.id with
                       | None -> read_private_key r
                       | Some id -> read_private_key ~id r)) >>| fun (id, _priv) ->
     Logs.debug (fun m -> m "using private key %s" id))
(*  Conex_
    Conex_repository.read_index r id  *)

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

let package =
  let doc = "Package." in
  Arg.(value & pos 0 string "" & info [] ~docv:"PACKAGE" ~doc)

let info_cmd =
  let doc = "information about yourself" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about yourself."]
  in
  Term.(ret (const information $ setup_log $ Conex_opts.t_t $ package)),
  Term.info "info" ~doc ~man

let sign_cmd =
  let doc = "sign a package" in
  let man =
    [`S "DESCRIPTION";
     `P "Cryptographically signs a given package with your id."]
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
  let doc = "sign your releases as author" in
  let man = help_secs in
  Term.(ret (const (fun _ _ -> `Help (`Pager, None)) $ setup_log $ Conex_opts.t_t)),
  Term.info "conex_author" ~version:"0.42.0" ~sdocs:docs ~doc ~man

let cmds = [ info_cmd ; help_cmd ; init_cmd ; sign_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
