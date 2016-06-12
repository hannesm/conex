open Core

type t = {
  name : name ;
  counter : int64 ;
  authorised : identifier list ;
  signatures : Signature.t list ;
}

let pp_authorised ppf x = pp_list pp_id ppf (List.sort String.compare x)

let pp_authorisation ppf d =
  Format.fprintf ppf "name: %a@ counter: %Lu@ authorised:@ %a@ %a@."
    pp_name d.name d.counter pp_authorised d.authorised
    Signature.pp_signatures d.signatures
