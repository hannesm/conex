type t = {
  dry : bool ;
  repo : Conex_repository.t ;
  id : string option ;
  trust_anchors : string list ;
}

open Cmdliner

let docs = "COMMON OPTIONS"

let t dry repo id trust_anchors quorum =
  let repo =
    let provider = if dry then Conex_provider.fs_ro_provider repo else Conex_provider.fs_provider repo in
    Conex_repository.repository ?quorum provider
  in
  { dry ; repo ; id ; trust_anchors }

let id_c =
  let parse s =
    if Conex_opam_layout.valid_id s then
      `Ok s
    else
      `Error "invalid identifier (valid: A-Za-z0-9)"
  in
  (parse, fun ppf s -> Format.pp_print_string ppf s)

let name_c =
  let parse s =
    let pn = match Conex_opam_layout.authorisation_of_item s with
      | None -> s
      | Some x -> x
    in
    if Conex_opam_layout.valid_name pn then
      `Ok s
    else
      `Error "invalid package name (valid: A-Za-z0-9-_)"
  in
  (parse, fun ppf s -> Format.pp_print_string ppf s)

let t_t =
  let dry =
    let doc = "Try run. Do not write anything." in
    Arg.(value & flag & info ["dry-run"] ~docs ~doc)
  and repo =
    let doc = "Repository base directory" in
    Arg.(value & opt dir "/tmp/conex" & info [ "r" ; "repository" ] ~docs ~docv:"DIR" ~doc)
  and id =
    let doc = "Use specified identity" in
    Arg.(value & opt (some id_c) None & info ["id"] ~docs ~docv:"IDENTITY" ~doc)
  and tas =
    let doc = "Pass set of trust anchors" in
    Arg.(value & opt_all string [] & info ["trust-anchors"; "t"] ~docs ~doc)
  and quorum =
    let doc = "Pass the quorum for the repository" in
    Arg.(value & opt (some int) None & info ["quorum"] ~docs ~doc)
  in
  Term.(const t $ dry $ repo $ id $ tas $ quorum)

let remove =
  let doc = "Remove" in
  Arg.(value & flag & info ["remove"] ~docs ~doc)

let members =
  let doc = "Members" in
  Arg.(value & opt_all id_c [] & info ["members"; "m"] ~docs ~doc)

let package =
  let doc = "Package" in
  Arg.(value & pos 0 name_c "" & info [] ~docv:"PACKAGE" ~doc)
