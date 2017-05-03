open Conex_utils
open Conex_resource
open Rresult

module V = Conex_nocrypto.NC_V
module C = Conex.Make(Logs)(V)
module PRIV = Conex_private.Make(Conex_nocrypto.C(Conex_unix_private_key))

let queue_r idx name auth =
  let counter = Author.next_id idx in
  let data = Author.wire auth in
  let digest = V.digest data in
  let res = Author.r counter name `Author digest in
  Logs.info (fun m -> m "added %a to queue" Author.pp_r res) ;
  let idx = Author.queue idx res in
  Author.approve idx res

let now = match Uint.of_float (Unix.time ()) with
  | None -> invalid_arg "cannot convert now to unsigned integer"
  | Some x -> x

let err_to_cmdliner = function
  | Ok _ -> `Ok ()
  | Error m -> `Error (false, m)

let doit _ repo quorum patchfile ignore_missing id =
  err_to_cmdliner (
    (match patchfile with
     | None -> Error "you have to provide --patch"
     | Some p -> Ok p) >>= fun patchfile ->
    Nocrypto_entropy_unix.initialize () ;
    (match id with
     | None -> Error "you have to provide --id"
     | Some id -> PRIV.read id >>= fun p -> (Ok (id, p))) >>= fun (id, priv) ->
    Conex_unix_provider.fs_provider repo >>= fun io ->
    let repo = Conex_repository.repository ?quorum V.digest () in
    Conex_unix_persistency.read_file patchfile >>= fun patch ->
    C.verify_diff ~ignore_missing io repo patch >>= fun _ ->
    let ws = Logs.warn_count () in
    Printf.printf "verification successfull with %d warnings\n" ws ;
    let diffs = Conex_diff.to_diffs patch in
    let newio = List.fold_left Conex_diff_provider.apply io diffs in
    (* now that we have a sane repository, read ourselves, increment counter,
       sign, and save... we expect patch to not include us *)
    let idx = match Conex_io.read_author io id with
      | Ok idx -> idx (* should empty all the resources - no need for keeping old stuff *)
      | Error _ -> Author.t Uint.zero id
    in
    (* sign _all_ the (new)authors (apart from ourselves/other snaps and potential
       timestamps -- does it hurt?) *)
    Conex_io.ids newio >>= fun ids ->
    (* we ignore errors in this fold: verify above already complained, and we're
       not signing off any teams! *)
    let idx = S.fold (fun id idx ->
        match Conex_io.read_author newio id with
        | Ok data -> queue_r idx id data
        | _ -> idx)
        ids idx
    in
    PRIV.sign now idx `RSA_PSS_SHA256 priv >>= fun idx ->
    Logs.info (fun m -> m "signed author %a" Author.pp idx) ;
    Conex_io.write_author io idx >>| fun () ->
    Logs.app (fun m -> m "wrote %s to disk" id))

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

let repo =
  let doc = "Repository base directory" in
  Arg.(value & pos 0 dir "" & info [] ~docs ~doc)

let man = [
  `S "DESCRIPTION" ;
  `P "$(tname) snapshots a given repository"
]

let cmd =
  Term.(ret (const doit $ setup_log $ repo $ quorum $ patch $ no_strict $ id)),
  Term.info "conex_snapshot" ~version:"%%VERSION_NUM%%"
    ~doc:"Snapshot a given repository" ~man


let () =
  match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
