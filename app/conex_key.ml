open Conex_utils
open Conex_resource

open Conex_opts
open Conex_mc

let jump _ id force pub =
  Mirage_crypto_rng_unix.initialize () ;
  msg_to_cmdliner
    (match id with
     | None -> Error "need an id"
     | Some id ->
       let fp k =
         let public = PRIV.pub_of_priv k in
         Logs.app (fun m -> m "key %s created %s keyid %s"
                      (PRIV.id k) (PRIV.created k)
                      (Digest.to_string (Key.keyid V.raw_digest public))) ;
         if pub then
           Logs.app (fun m -> m "public key: %a@.%s"
                        Conex_resource.Key.pp public
                        (Conex_opam_encoding.encode (Key.wire public))) ;
         Ok ()
       in
       let gen_or_err () =
         PRIV.generate to_ts `RSA id () >>= fun t ->
         Logs.app (fun m -> m "generated fresh key") ;
         fp t
     in
     if force
     then gen_or_err ()
     else match PRIV.read to_ts id with
       | Ok key -> fp key
       | Error `None -> gen_or_err ()
       | Error e -> Error (Fmt.to_to_string PRIV.pp_r_err e))

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

let pub =
  let doc = "Display full public key." in
  Arg.(value & flag & info ["pub"] ~docs ~doc)

let cmd =
  let doc = "key management" in
  Term.(ret (const jump $ setup_log $ Keys.id $ Keys.force $ pub)),
  Term.info "conex_key" ~version:"%%VERSION_NUM%%" ~doc

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
