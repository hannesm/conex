open Conex_verify_app

open Conex_opts

module V = VERIFY(Logs)(Conex_mirage_crypto.NC_V)

let jump _ repo quorum anchors inc dir patch nostrict root no_opam timestamp_expiry =
  let now = Int64.of_float (Ptime.to_float_s Conex_mc.now) in
  msg_to_cmdliner (V.verify_it repo quorum anchors inc dir patch nostrict root (not no_opam) ~timestamp_expiry ~now)

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
  let term =
    Term.(ret (const jump $ setup_log $ Keys.repo $ Keys.quorum $ Keys.anchors $ Keys.incremental $ Keys.dir $ Keys.patch $ Keys.ignore_missing $ Keys.root $ Keys.no_opam $ Keys.timestamp_expiry))
  and info = Cmd.info "conex_verify_mirage_crypto" ~version:"%%VERSION_NUM%%"
      ~doc:Conex_verify_app.doc ~man:Conex_verify_app.man
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
