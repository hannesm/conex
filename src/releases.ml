open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  releases : name list ;
  signatures : Signature.t list ;
}

let pp_releases ppf r =
  Format.fprintf ppf "name: %a@ counter %Lu@ releases %a@ %a@."
    pp_name r.name r.counter
    (pp_list pp_name) r.releases
    Signature.pp_signatures r.signatures
