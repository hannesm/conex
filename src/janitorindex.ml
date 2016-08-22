open Core

type t = {
  counter : int64 ;
  version : int64 ;
  identifier : identifier ;
  resources : (name * resource * digest) list ;
  signatures : Signature.t list ;
}

let janitorindex ?(counter = 0L) ?(version = 0L) ?(resources = []) ?(signatures = []) identifier =
  { counter ; version ; identifier ; resources ; signatures }

let pp_resource ppf (n, r, digest) =
  Format.fprintf ppf "name: %a@ resource: %a@ digest: %a@."
    pp_name n pp_resource r pp_digest digest

let pp_janitorindex ppf ji =
  Format.fprintf ppf "identifier: %a@ counter: %Lu@ resources:@ %a@ %a@."
    pp_id ji.identifier
    ji.counter
    (pp_list pp_resource) ji.resources
    Signature.pp_signatures ji.signatures

let add_sig ji s = { ji with signatures = s :: ji.signatures }
