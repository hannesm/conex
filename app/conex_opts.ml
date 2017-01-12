type t = {
  dry : bool ;
  repo : Repository.t ;
  id : string option ;
  trust_anchors : string list ;
  team : bool
}

open Cmdliner

let s = "COMMON OPTIONS"

let t dry repo id trust_anchors quorum team =
  let repo =
    let provider = if dry then Conex_provider.fs_ro_provider repo else Conex_provider.fs_provider repo in
    Repository.repository ?quorum provider
  in
  { dry ; repo ; id ; trust_anchors ; team }

let t_t =
  let docs = s in
  let dry =
    let doc = "Try run. Do not write anything." in
    Arg.(value & flag & info ["dry-run"] ~docs ~doc)
  and repo =
    let doc = "Repository base directory" in
    Arg.(value & opt dir "/tmp/conex" & info [ "r" ; "repository" ] ~docs ~docv:"DIR" ~doc)
  and id =
    let doc = "Use specified identity" in
    Arg.(value & opt (some string) None & info ["id"] ~docs ~docv:"IDENTITY" ~doc)
  and tas =
    let doc = "Pass set of trust anchors" in
    Arg.(value & opt_all string [] & info ["trust-anchors"; "t"] ~docs ~docv:"DIR" ~doc)
  and quorum =
    let doc = "Pass the quorum for the repository" in
    Arg.(value & opt (some int) None & info ["quorum"] ~docs ~doc)
  and team =
    let doc = "Transitively use teams" in
    Arg.(value & flag & info ["team"] ~docs ~doc)
  in
  Term.(const t $ dry $ repo $ id $ tas $ quorum $ team)
