open Core

type t = identifier * int64 * string

let extend_data data id ts =
  String.concat " " [ data ; id ; Int64.to_string ts ]

(*BISECT-IGNORE-BEGIN*)
let pp_signature ppf (id, ts, s) =
  Format.fprintf ppf "%a created at %Lu@ sig:@ %s" pp_id id ts s
(*BISECT-IGNORE-END*)
