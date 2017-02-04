
open Cmdliner

let docs = "COMMON OPTIONS"

let id_c =
  let parse s =
    if Conex_opam_repository_layout.valid_id s then
      `Ok s
    else
      `Error "invalid identifier (valid: A-Za-z0-9)"
  in
  (parse, fun ppf s -> Format.pp_print_string ppf s)

let name_c =
  let parse s =
    let pn = match Conex_opam_repository_layout.authorisation_of_item s with
      | None -> s
      | Some x -> x
    in
    if Conex_opam_repository_layout.valid_name pn then
      `Ok s
    else
      `Error "invalid package name (valid: A-Za-z0-9-_)"
  in
  (parse, fun ppf s -> Format.pp_print_string ppf s)

let strict =
    let doc = "Strict verification" in
    Arg.(value & flag & info [ "strict" ] ~doc)

let quorum =
  let doc = "Pass the quorum for the repository" in
  Arg.(value & opt (some int) None & info [ "quorum" ] ~doc)

let repo =
    let doc = "Repository base directory" in
    Arg.(value & opt dir "/tmp/conex" & info [ "repository" ] ~doc)

let anchors =
  let doc = "Trust anchors" in
  Arg.(value & opt_all string [] & info [ "trust-anchors" ] ~doc)

let dry =
  let doc = "Try run. Do not write anything." in
  Arg.(value & flag & info ["dry-run"] ~docs ~doc)

let id =
  let doc = "Use specified identity" in
  Arg.(value & opt (some id_c) None & info ["id"] ~docs ~doc)

let remove =
  let doc = "Remove" in
  Arg.(value & flag & info ["remove"] ~docs ~doc)

let members =
  let doc = "Members" in
  Arg.(value & opt_all id_c [] & info ["members"; "m"] ~docs ~doc)

let package =
  let doc = "Package" in
  Arg.(value & pos 0 name_c "" & info [] ~docv:"PACKAGE" ~doc)

let terminal () =
  let dumb = try Sys.getenv "TERM" = "dumb" with
    | Not_found -> true
  in
  let isatty = try Unix.(isatty (descr_of_out_channel Pervasives.stdout)) with
    | Unix.Unix_error _ -> false
  in
  if not dumb && isatty then `Ansi_tty else `None
