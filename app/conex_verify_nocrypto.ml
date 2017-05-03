open Conex_verify_app

module V = VERIFY(Logs)(Conex_nocrypto.NC_V)

let jump _ repo quorum anchors inc dir patch nostrict =
  err_to_cmdliner (V.verify_it repo quorum anchors inc dir patch nostrict)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Conex_opts
open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ~docs ()
        $ Logs_cli.level ~docs ())

let cmd =
  Term.(ret (const jump $ setup_log $ repo $ quorum $ anchors $ incremental $ dir $ patch $ no_strict)),
  Term.info "conex_verify_nocrypto" ~version:"%%VERSION_NUM%%"
    ~doc:Conex_verify_app.doc ~man:Conex_verify_app.man

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
