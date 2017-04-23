
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
    let pn = match Conex_opam_repository_layout.authorisation_of_package s with
      | None -> s
      | Some x -> x
    in
    if Conex_opam_repository_layout.valid_name pn then
      `Ok s
    else
      `Error "invalid package name (valid: A-Za-z0-9-_)"
  in
  (parse, fun ppf s -> Format.pp_print_string ppf s)

let no_strict =
  let doc = "Non-strict verification mode.  Packages without authorisation or package index are ignored." in
  Arg.(value & flag & info [ "nostrict" ] ~doc)

let quorum =
  let doc = "The quorum of janitors used for verification of the repository" in
  Arg.(value & opt (some int) None & info [ "quorum" ] ~doc)

let repo =
  let doc = "Repository base directory (defaults to cwd)" in
  Arg.(value & opt (some dir) None & info [ "r" ; "repository" ] ~docs ~doc)

let anchors =
  let doc = "Trust anchors (Base64 encoded SHA256 hashes, seperated by ',').  Can be repeated." in
  Arg.(value & opt_all string [] & info [ "t" ; "trust-anchors" ] ~doc)

let convert_anchors a = Conex_utils.s_of_list
    (List.flatten (List.map (Conex_utils.String.cuts ',') a))

let dry =
  let doc = "Dry run. Do not write anything to persistent storage." in
  Arg.(value & flag & info ["dry-run"] ~docs ~doc)

let id =
  let doc = "Use a specific identity (not needed unless you have more than one identity)." in
  Arg.(value & opt (some id_c) None & info ["id"] ~docs ~doc)

let remove =
  let doc = "Remove members from resource" in
  Arg.(value & flag & info ["remove"] ~doc)

let members =
  let doc = "Members to include.  May be repeated." in
  Arg.(value & opt_all id_c [] & info ["members"; "m"] ~doc)

let package =
  let doc = "Package name" in
  Arg.(value & pos 0 name_c "" & info [] ~doc)

let incremental =
  let doc = "Incremental verification mode" in
  Arg.(value & flag & info [ "incremental" ] ~doc)

let dir =
  let doc = "Directory which is verified." in
  Arg.(value & opt (some dir) None & info [ "dir" ] ~doc)

let patch =
  let doc = "Patch file which is verified." in
  Arg.(value & opt (some file) None & info [ "patch" ] ~doc)
