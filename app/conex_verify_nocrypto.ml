open Conex_utils

module V = Conex_verify.VERIFY (Logs) (Conex_nocrypto.NC_V)

let jump _ = V.verify_it

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
  Term.(ret (const jump $ setup_log $ repo $ quorum $ anchors $ incremental $ dir $ patch $ strict)),
  Term.info "conex_verify_nocrypto" ~version:"%%VERSION_NUM%%"
    ~doc:Conex_verify.doc ~man:Conex_verify.man

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
