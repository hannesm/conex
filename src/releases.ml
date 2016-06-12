open Core

type t = {
  name : name ;
  counter : int64 ;
  releases : name list ;
  signatures : Signature.t list ;
}

let pp_releases ppf r =
  Format.fprintf ppf "name: %a@ counter %Lu@ releases %a@ %a@."
    pp_name r.name r.counter
    (pp_list pp_name) r.releases
    Signature.pp_signatures r.signatures
