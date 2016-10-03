open Core

type t = identifier * string

let extend_data data id =
  String.concat " " [ data ; id ]

(*BISECT-IGNORE-BEGIN*)
let pp_signature ppf (id, s) = Format.fprintf ppf "%a sig:@ %s" pp_id id s
(*BISECT-IGNORE-END*)
