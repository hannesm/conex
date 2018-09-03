open Conex_verify_app

open Conex_opts

module V = VERIFY(Logs)(Conex_nocrypto.NC_V)

let jump _ repo quorum anchors inc dir patch nostrict root no_opam =
  msg_to_cmdliner (V.verify_it repo quorum anchors inc dir patch nostrict root (not no_opam))

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let docs = Keys.docs

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ~docs ()
        $ Logs_cli.level ~docs ())

let cmd =
  Term.(ret (const jump $ setup_log $ Keys.repo $ Keys.quorum $ Keys.anchors $ Keys.incremental $ Keys.dir $ Keys.patch $ Keys.ignore_missing $ Keys.root $ Keys.no_opam)),
  Term.info "conex_verify_nocrypto" ~version:"%%VERSION_NUM%%"
    ~doc:Conex_verify_app.doc ~man:Conex_verify_app.man

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
