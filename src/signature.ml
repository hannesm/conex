open Core

type t = identifier * string

let extend_data data id =
  String.concat " " [ data ; id ]

(*BISECT-IGNORE-BEGIN*)
let pp_signature ppf = function
  | None -> Format.pp_print_string ppf "no sig"
  | Some (id, sv) -> Format.fprintf ppf "%a sig:@ %s" pp_id id sv
(*BISECT-IGNORE-END*)
