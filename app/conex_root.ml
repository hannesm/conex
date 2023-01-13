open Conex_utils
open Conex_resource

open Conex_opts
open Conex_mc

module IO = Conex_io

let ( let* ) = Result.bind

let status _ repodir anchors filename =
  msg_to_cmdliner (
    let valid = valid anchors in
    let* io = repo repodir in
    Result.fold
      ~error:(fun r ->
          Logs.err (fun m -> m "%a" IO.pp_r_err r) ;
          Error "failed loading")
      ~ok:(fun (root, warn) ->
          List.iter (fun msg -> Logs.warn (fun m -> m "%s" msg)) warn ;
          Logs.app (fun m -> m "root file %a" Root.pp root) ;
          Result.fold
            ~error:(fun e ->
                Logs.err (fun m -> m "couldn't verify root: %s" e) ;
                Error "failed verification")
            ~ok:(fun _ ->
                Logs.app (fun m -> m "verified successfully") ;
                Ok ())
            (C.verify_root ~valid io filename))
      (IO.read_root io filename))

let create _ dry repodir force filename =
  msg_to_cmdliner (
    let* io = repo ~rw:(not dry) repodir in
    let valid = Expression.Quorum (0, Expression.KS.empty) in
    let root = Root.t ~name:filename now valid in
    let root' =
      Result.fold
        ~error:(fun _ -> root)
        ~ok:(fun (root', _) -> if force then root else root')
        (IO.read_root io filename)
    in
    Logs.app (fun m -> m "root file %a" Root.pp root') ;
    IO.write_root io root')

let sign _ dry repodir id no_incr filename =
  Mirage_crypto_rng_unix.initialize () ;
  msg_to_cmdliner (
    let* priv, id' = init_priv_id id in
    let* io = repo ~rw:(not dry) repodir in
    let* root, warn = to_str IO.pp_r_err (IO.read_root io filename) in
    List.iter (fun msg -> Logs.warn (fun m -> m "%s" msg)) warn ;
    let* root' =
      match no_incr, Uint.succ root.Root.counter with
      | false, (true, _) -> Error "couldn't increment counter"
      | true, _ -> Ok root
      | false, (false, counter) -> Ok { root with Root.counter }
    in
    let* signature =
      PRIV.sign (Root.wire_raw root') now id' `RSA_PSS_SHA256 priv
    in
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
     `P "Cryptographically signs the root file."]
  in
  let term =
    Term.(ret Conex_opts.(const sign $ setup_log $ Keys.dry $ Keys.repo $ Keys.id $ Keys.no_incr $ Keys.root))
  and info = Cmd.info "sign" ~doc ~man
  in
  Cmd.v info term

let status_cmd =
  let doc = "information about provided root file" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows information about the root file."]
  in
  let term =
    Term.(ret Conex_opts.(const status $ setup_log $ Keys.repo $ Keys.anchors $ Keys.root))
  and info = Cmd.info "status" ~doc ~man
  in
  Cmd.v info term

let create_cmd =
  let doc = "create an empty root file" in
  let man =
    [`S "DESCRIPTION";
     `P "Creates a fresh root file."]
  in
  let term =
    Term.(ret Conex_opts.(const create $ setup_log $ Keys.dry $ Keys.repo $ Keys.force $ Keys.root))
  and info = Cmd.info "create" ~doc ~man
  in
  Cmd.v info term

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  Term.(ret Conex_opts.(const help $ setup_log $ Keys.dry $ Keys.repo $ Keys.id $ Arg.man_format $ Term.choice_names $ topic))

let cmds = [ status_cmd ; sign_cmd ; create_cmd ]

let () =
  let doc = "Manage root file of a signed community repository" in
  let man = help_secs in
  let info = Cmd.info "conex_root" ~version:"%%VERSION_NUM%%" ~sdocs:docs ~doc ~man in
  let group = Cmd.group ~default:help_cmd info cmds in
  exit (Cmd.eval group)
