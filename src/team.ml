open Core

type t = {
  counter : int64 ;
  version : int64 ;
  name : identifier ;
  members : S.t
}

let team ?(counter = 0L) ?(version = 0L) ?(members = S.empty) name =
  { counter ; version ; members ; name }

let add t id =
  { t with counter = Int64.succ t.counter ; members = S.add id t.members }

let remove t id =
  { t with counter = Int64.succ t.counter ; members = S.remove id t.members }

(*BISECT-IGNORE-BEGIN*)
let pp_mems ppf x = pp_list pp_id ppf (List.sort String.compare (S.elements x))

let pp_team ppf x =
  Format.fprintf ppf "team name: %a@ counter: %Lu@ members:@ %a@."
    pp_id x.name x.counter pp_mems x.members
(*BISECT-IGNORE-END*)
