open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  releases : name list ;
}

let releases ?(counter = 0L) ?(version = 0L) ?(releases = []) name =
  { counter ; version ; name ; releases }

let pp_releases ppf r =
  Format.fprintf ppf "name: %a@ counter %Lu@ releases %a@."
    pp_name r.name r.counter
    (pp_list pp_name) r.releases
