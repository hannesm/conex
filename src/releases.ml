open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  releases : S.t ;
}

let releases ?(counter = 0L) ?(version = 0L) ?(releases = S.empty) name =
  (* XXX: should verify that all r.Releases.releases are good regarding r.Releases.name *)
  { counter ; version ; name ; releases }

(*BISECT-IGNORE-BEGIN*)
let pp_releases ppf r =
  Format.fprintf ppf "name: %a@ counter %Lu@ releases %a@."
    pp_name r.name r.counter
    (pp_list pp_name) (S.elements r.releases)
(*BISECT-IGNORE-END*)
