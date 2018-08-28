open Conex_utils
open Conex_resource

open Rresult

open Conex_opts
open Conex_nc

module IO = Conex_io

let status _ repodir anchors filename  =
  msg_to_cmdliner (
    let valid = valid anchors in
    repo repodir >>= fun io ->
    match IO.read_root io filename with
    | Error r ->
      Logs.err (fun m -> m "%a" IO.pp_r_err r) ;
      Error "failed loading"
    | Ok (root, warn) ->
      List.iter (fun msg -> Logs.warn (fun m -> m "%s" msg)) warn ;
      Logs.app (fun m -> m "root file %a" Root.pp root) ;
      match C.verify_root ~valid io filename with
      | Error e -> Logs.err (fun m -> m "couldn't verify root: %s" e) ; Error "failed verification"
      | Ok _repo -> Logs.app (fun m -> m "verified successfully") ; Ok ())

let create _ dry repodir force filename =
  msg_to_cmdliner (
    repo ~rw:(not dry) repodir >>= fun io ->
    let valid = Expression.Quorum (0, Expression.KS.empty) in
    let root = Root.t ~name:filename now valid in
    let root' =
      match IO.read_root io filename with
      | Error _ -> root
      | Ok _ when force -> root
      | Ok (root', _) -> root'
    in
    Logs.app (fun m -> m "root file %a" Root.pp root') ;
    IO.write_root io root')

let to_str pp = function
  | Ok x -> Ok x
  | Error e -> Error (Fmt.to_to_string pp e)

let sign _ dry repodir id no_incr filename =
  Nocrypto_entropy_unix.initialize () ;
  msg_to_cmdliner (
    init_priv_id id >>= fun (priv, id') ->
    repo ~rw:(not dry) repodir >>= fun io ->
    to_str IO.pp_r_err (IO.read_root io filename) >>= fun (root, warn) ->
    List.iter (fun msg -> Logs.warn (fun m -> m "%s" msg)) warn ;
    (match no_incr, Uint.succ root.Root.counter with
     | false, (true, _) -> Error "couldn't increment counter"
     | true, _ -> Ok root
     | false, (false, counter) -> Ok { root with Root.counter }) >>= fun root' ->
    PRIV.sign (Root.wire_raw root') now id' `RSA_PSS_SHA256 priv >>= fun signature ->
    let root'' = Root.add_signature root' id' signature in
    IO.write_root io root'')

let help _ _ _ _ man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let docs = Keys.docs

let help_secs = [
 `S "GENERAL";
 `P "$(mname) is a tool for managing cryptographically signed community repositories.";
 `P "The signing metadata is kept in the same repository.";
 `S docs;
 `P "These options are common to all commands.";
 `S "SEE ALSO";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
 `S "BUGS"; `P "Please report bugs at https://github.com/hannesm/conex.";]

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ~docs ()
        $ Logs_cli.level ~docs ())

let sign_cmd =
  let doc = "sign root file with provided key" in
  let man =
    [`S "DESCRIPTION";
     `P "Cryptographically signs queued changes to your resource list."]
  in
  Term.(ret Conex_opts.(const sign $ setup_log $ Keys.dry $ Keys.repo $ Keys.id $ Keys.no_incr $ Keys.root)),
  Term.info "sign" ~doc ~man

let status_cmd =
  let doc = "information about provided root file" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information root file."]
  in
  Term.(ret Conex_opts.(const status $ setup_log $ Keys.repo $ Keys.anchors $ Keys.root)),
  Term.info "status" ~doc ~man

let create_cmd =
  let doc = "create an empty root file" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a fresh root file."]
  in
  Term.(ret Conex_opts.(const create $ setup_log $ Keys.dry $ Keys.repo $ Keys.force $ Keys.root)),
  Term.info "create" ~doc ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about conex_root" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about conex commands and subcommands"] @ help_secs
  in
  Term.(ret Conex_opts.(const help $ setup_log $ Keys.dry $ Keys.repo $ Keys.id $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "manage root file of a signed community repository" in
  let man = help_secs in
  Term.(ret Conex_opts.(const help $ setup_log $ Keys.dry $ Keys.repo $ Keys.id $ Term.man_format $ Term.choice_names $ Term.pure None)),
  Term.info "conex_root" ~version:"%%VERSION_NUM%%" ~sdocs:docs ~doc ~man

let cmds = [ help_cmd ; status_cmd ; sign_cmd ; create_cmd ]

let () =
  match Term.eval_choice default_cmd cmds
  with `Ok () -> exit 0 | _ -> exit 1
