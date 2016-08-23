open Core

type t = {
  counter : int64 ;
  version : int64 ;
  identifier : identifier ;
  resources : (name * resource * digest) list ;
  signatures : Signature.t list ;
}

let index ?(counter = 0L) ?(version = 0L) ?(resources = []) ?(signatures = []) identifier =
  { counter ; version ; identifier ; resources ; signatures }

let pp_resource ppf (n, r, digest) =
  Format.fprintf ppf "name: %a@ resource: %a@ digest: %a@."
    pp_name n pp_resource r pp_digest digest

let pp_index ppf i =
  Format.fprintf ppf "identifier: %a@ counter: %Lu@ resources:@ %a@ %a@."
    pp_id i.identifier
    i.counter
    (pp_list pp_resource) i.resources
    Signature.pp_signatures i.signatures

let add_sig i s = { i with signatures = s :: i.signatures }
