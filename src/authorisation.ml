open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  authorised : S.t ;
}

let authorisation ?(counter = 0L) ?(version = 0L) ?(authorised = S.empty) name =
  { counter ; version ; name ; authorised }

let add t id =
  if S.mem id t.authorised then
    t
  else
    { t with counter = Int64.succ t.counter ; authorised = S.add id t.authorised }

let remove t id =
  if S.mem id t.authorised then
    { t with counter = Int64.succ t.counter ; authorised = S.remove id t.authorised }
  else
    t

(*BISECT-IGNORE-BEGIN*)
let pp_authorised ppf x = pp_list pp_id ppf (List.sort String.compare (S.elements x))

let pp_authorisation ppf d =
  Format.fprintf ppf "authorisation name: %a@ counter: %Lu@ authorised:@ %a@."
    pp_name d.name d.counter pp_authorised d.authorised
(*BISECT-IGNORE-END*)
