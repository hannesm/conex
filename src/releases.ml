open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  releases : name list ;
}

let releases ?(counter = 0L) ?(version = 0L) ?(releases = []) name =
  (* XXX: should verify that all r.Releases.releases are good regarding r.Releases.name *)
  { counter ; version ; name ; releases }

(*BISECT-IGNORE-BEGIN*)
let pp_releases ppf r =
  Format.fprintf ppf "name: %a@ counter %Lu@ releases %a@."
    pp_name r.name r.counter
    (pp_list pp_name) r.releases
(*BISECT-IGNORE-END*)
