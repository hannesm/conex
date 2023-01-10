open Conex_utils

module type EXTLOGS = sig
  include LOGS

  type level = [ `Debug | `Info | `Warn ]
  val set_level : level -> unit
  val set_styled : bool -> unit
end

module Log : EXTLOGS = struct
  module Tag = struct
    type set
  end

  type ('a, 'b) msgf =
    (?header:string -> ?tags:Tag.set ->
     ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
  type 'a log = ('a, unit) msgf -> unit

  type src

  type level = [ `Debug | `Info | `Warn ]
  let curr_level = ref `Warn
  let set_level lvl = curr_level := lvl
  let level_to_string = function
    | `Debug -> "DEBUG"
    | `Info -> "INFO"
    | `Warn -> "WARN"

  let curr_styled = ref true
  let set_styled b = curr_styled := b
  let style level txt =
    if !curr_styled then
      let rst = "\027[m" in
      match level with
      | `Debug -> "\027[32m" ^ txt ^ rst
      | `Info -> "\027[34m" ^ txt ^ rst
      | `Warn -> "\027[33m" ^ txt ^ rst
    else
      txt

  let report level k msgf =
    let k _ = k () in
    msgf @@ fun ?header ?tags:_ fmt ->
    let hdr = match header with None -> "" | Some s -> s ^ " " in
    Format.kfprintf k Format.std_formatter ("%s[%s] @[" ^^ fmt ^^ "@]@.") hdr (style level (level_to_string level))

  let wcount = ref 0

  let warn_count () = !wcount

  let kunit _ = ()
  let kmsg : type a b. (unit -> b) -> level -> (a, b) msgf -> b =
    fun k level msgf ->
      let doit =
        match level, !curr_level with
        | `Warn, _ -> true
        | `Info, `Debug | `Info, `Info -> true
        | `Debug, `Debug -> true
        | _ -> false
      in
      if doit then report level k msgf else k ()

  let debug ?src:_ msgf = kmsg kunit `Debug msgf
  let info ?src:_ msgf = kmsg kunit `Info msgf
  let warn ?src:_ msgf = incr wcount ; kmsg kunit `Warn msgf
end

open Conex_verify_app
open Conex_opts
module V = VERIFY(Log)(Conex_openssl.O_V)

let terminal () =
  let dumb = try Sys.getenv "TERM" = "dumb" with
    | Not_found -> true
  in
  let isatty = try Unix.(isatty (descr_of_out_channel Stdlib.stdout)) with
    | Unix.Unix_error _ -> false
  in
  if not dumb && isatty then `Ansi_tty else `None

let setup repo quorum anchors incremental dir patch verbose quiet strict no_c root no_opam timestamp_expiry =
  let level =
    if quiet then `Warn
    else if verbose then `Debug
    else `Info
  in
  Log.set_level level ;
  let styled = if no_c then false else match terminal () with `Ansi_tty -> true | `None -> false
  in
  Log.set_styled styled ;
  let now = Int64.of_float (Unix.time ()) in
  msg_to_cmdliner (
    Conex_openssl.V.check_version () >>= fun () ->
    V.verify_it repo quorum anchors incremental dir patch strict root (not no_opam) ~timestamp_expiry ~now)

open Conex_opts
open Cmdliner

let quiet =
    let doc = "Be quiet.  Takes over $(b,--verbose)" in
    Arg.(value & flag & info [ "q" ; "quiet" ] ~doc)

let verbose =
    let doc = "Be more verbose." in
    Arg.(value & flag & info [ "v" ; "verbose" ] ~doc)

let no_color =
    let doc = "Don't colourise the output.  Default is to colourise unless the output is not a terminal (or a dumb one)." in
    Arg.(value & flag & info [ "no-color" ] ~doc)

let cmd =
  let term =
    Term.(ret (const setup $ Keys.repo $ Keys.quorum $ Keys.anchors $ Keys.incremental $ Keys.dir $ Keys.patch $ verbose $ quiet $ Keys.ignore_missing $ no_color $ Keys.root $ Keys.no_opam $ Keys.timestamp_expiry))
  and info = Cmd.info "conex_verify_openssl" ~version:"%%VERSION_NUM%%"
      ~doc:Conex_verify_app.doc ~man:Conex_verify_app.man
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
