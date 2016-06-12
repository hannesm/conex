open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  authorised : identifier list ;
  signatures : Signature.t list ;
}

let authorisation ?(counter = 0L) ?(version = 0L) ?(authorised = []) ?(signatures = []) name =
  { counter ; version ; name ; authorised ; signatures }

let add_sig t s = { t with signatures = s :: t.signatures }

let pp_authorised ppf x = pp_list pp_id ppf (List.sort String.compare x)

let pp_authorisation ppf d =
  Format.fprintf ppf "authorisation name: %a@ counter: %Lu@ authorised:@ %a@ %a@."
    pp_name d.name d.counter pp_authorised d.authorised
    Signature.pp_signatures d.signatures
