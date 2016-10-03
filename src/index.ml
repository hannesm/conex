open Core

type t = {
  counter : int64 ;
  version : int64 ;
  identifier : identifier ;
  resources : (name * resource * digest) list ; (* should be a Set? should include file size for better error reporting? *)
  signatures : Signature.t list ;
}

let index ?(counter = 0L) ?(version = 0L) ?(resources = []) ?(signatures = []) identifier =
  { counter ; version ; identifier ; resources ; signatures }

let add_resource t r =
  { t with resources = r :: t.resources ; counter = Int64.succ t.counter }

let add_resources t rs =
  { t with resources = rs @ t.resources ; counter = Int64.succ t.counter }

(*BISECT-IGNORE-BEGIN*)
let pp_resource ppf (n, r, digest) =
  Format.fprintf ppf "name: %a@ resource: %a@ digest: %a@."
    pp_name n pp_resource r pp_digest digest

let pp_index ppf i =
  Format.fprintf ppf "identifier: %a@ counter: %Lu@ resources:@ %a@ %a@."
    pp_id i.identifier
    i.counter
    (pp_list pp_resource) i.resources
    (pp_list Signature.pp_signature) i.signatures
(*BISECT-IGNORE-END*)

let add_sig i s = { i with signatures = s :: i.signatures }

let replace_sig i s = { i with signatures = [ s ] }
