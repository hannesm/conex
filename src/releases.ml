open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  releases : S.t ;
}

let releases ?(counter = 0L) ?(version = 0L) ?(releases = S.empty) name =
  if S.for_all (is_release name) releases then
    Ok { counter ; version ; name ; releases }
  else
    Error "all releases must have the same package name"

(*BISECT-IGNORE-BEGIN*)
let pp_releases ppf r =
  Format.fprintf ppf "name: %a@ counter %Lu@ releases %a@."
    pp_name r.name r.counter
    (pp_list pp_name) (S.elements r.releases)
(*BISECT-IGNORE-END*)
