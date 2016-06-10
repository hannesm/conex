open Core

type t = {
  name : string ;
  counter : int64 ;
  authorised : identifier list ;
  signatures : Signature.t list ;
}

let pp_owners ppf x =
  Format.pp_print_string ppf "[" ;
  let rec p1 = function
    | [] -> Format.pp_print_string ppf "]"
    | [x] -> Format.fprintf ppf "%s]" x
    | x::xs -> Format.fprintf ppf "%s," x ; p1 xs
  in
  p1 (List.sort String.compare x)

let pp_authorisation ppf d =
  Format.fprintf ppf "name: %s@ counter: %Lu@ owners:@ %a@ %a@."
                 d.name d.counter pp_owners d.authorised Signature.pp_signatures d.signatures
