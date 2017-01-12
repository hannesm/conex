open Conex_result
open Conex_core
open Conex_resource

open Rresult

(*
info (key, authorisations, team membership)
list/validate authorised things
generate key (+public info)
sign package X (X.version)
request authorisation
renew key
apply for/revoke team membership

logs+cmdliner+nocrypto
*)
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let help _ _copts man_format cmds = function
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

open Cmdliner

let docs = Conex_opts.s

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ~docs ()
        $ Logs_cli.level ~docs ())

let info_all r o =
  Conex_private.(R.error_to_msg ~pp_error:pp_err
                   (match o.Conex_opts.id with
                    | None -> read_private_key r >>| fst
                    | Some id -> Ok id)) >>= fun id ->
  Logs.debug (fun m -> m "using identifier %s" id);
  Repository.(R.error_to_msg ~pp_error:pp_r_err (Repository.read_id r id)) >>= function
  | `Key k ->
    Logs.info (fun m -> m "%a" Publickey.pp_publickey k);
    let warn e =
      Logs.warn (fun m -> m "%a" Repository.pp_r_err e)
    in
    let idx = Repository.(R.ignore_error ~use:(fun e -> warn e; Index.index id) (read_index r id)) in
    Logs.info (fun m -> m "%a" Index.pp_index idx);
    let me = s_of_list @@ if o.Conex_opts.team then
        let teams = S.fold (fun id' teams ->
            R.ignore_error ~use:(fun e -> warn e ; teams)
              (Repository.read_id r id' >>| function
                | `Team t when S.mem id t.Team.members ->
                  Logs.info (fun m -> m "member of %a" Team.pp_team t) ; t :: teams
                | `Team _ -> teams
                | `Key _ -> teams))
            (Repository.ids r) []
        in
        id :: List.map (fun t -> t.Team.name) teams
      else
        [id]
    in
    S.iter (fun item ->
        R.ignore_error ~use:warn
          (Repository.read_authorisation r item >>= fun a ->
           Logs.debug (fun m -> m "%a" Authorisation.pp_authorisation a);
           if not (S.is_empty (S.inter me a.Authorisation.authorised)) then begin
             Logs.info (fun m -> m "%a" Authorisation.pp_authorisation a) ;
             R.ignore_error
               ~use:(fun e -> Logs.warn (fun m -> m "%a" Repository.pp_error e))
               (Repository.verify_authorisation r a >>= fun ok ->
                Logs.info (fun m -> m "%a" Repository.pp_ok ok) ;
                Ok ()) ;

             let releases =
               let use e =
                 warn e ;
                 match Releases.releases ~releases:(Repository.subitems r item) item with
                 | Ok r -> r
                 | Error e -> invalid_arg e
               in
               R.ignore_error ~use
                 (Repository.read_releases r item >>= fun rel ->
                  Logs.info (fun m -> m "%a" Releases.pp_releases rel) ;
                  R.ignore_error ~use:(fun e -> Logs.warn (fun m -> m "%a" Repository.pp_error e))
                    (Repository.verify_releases r a rel >>| fun ok ->
                     Logs.info (fun m -> m "%a" Repository.pp_ok ok)) ;
                  Ok rel)
             in

             S.iter (fun release ->
                 R.ignore_error ~use:warn
                   (Repository.read_checksum r release >>| fun cs ->
                    R.ignore_error ~use:(fun e -> Logs.warn (fun m -> m "%a" Repository.pp_error e))
                      (Repository.verify_checksum r a releases cs >>| fun ok ->
                       Logs.info (fun m -> m "%a" Repository.pp_ok ok))))
               releases.Releases.releases ;
             Ok ()
           end else Ok ()))
      (Repository.items r) ;
    Ok ()
  | `Team t ->
    Logs.info (fun m -> m "%a" Conex_resource.Team.pp_team t); Ok ()

let info_single _r _o name =
  Logs.info (fun m -> m "information on package %s" name);
  Ok ()

let information _ o name =
  let r = o.Conex_opts.repo in
  Logs.info (fun m -> m "repository %s" (Repository.provider r).Provider.name) ;
  match if name = "" then info_all r o else info_single r o name with
  | Ok () -> `Ok ()
  | Error (`Msg m) -> `Error (false, m)

let help_secs = [
 `S "GENERAL";
 `P "Conex is a tool suite to manage and verify cryptographically signed data repositories.";
 `S "KEYS";
 `P "Public keys contain a unique identifier.  They are distributed with the repository itself.";
 `P "Your private keys are stored PEM-encoded in ~/.conex.  You can select which key to use by passing --id to conex.";
 `S docs;
 `P "These options are common to all commands.";
 `S "SEE ALSO";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
 `S "BUGS"; `P "Check bug reports at https://github.com/hannesm/conex.";]

let package =
  let doc = "Package name." in
  Arg.(value & pos 0 string "" & info [] ~docv:"PACKAGE" ~doc)

let info_cmd =
  let doc = "information about yourself" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about yourself."]
  in
  Term.(ret (const information $ setup_log $ Conex_opts.t_t $ package)),
  Term.info "info" ~doc ~man

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

let cmds = [info_cmd ; help_cmd]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
