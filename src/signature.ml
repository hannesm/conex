open Core

type t = identifier * string

let extend_data data id resource =
  String.concat " " [ data ; id ; resource_to_string resource ]

let pp_signature ppf (id, sv) =
  Format.fprintf ppf "%a sig:@ %s" pp_id id sv

let pp_signatures ppf sigs =
  Format.fprintf ppf "%d signatures:@.%a" (List.length sigs)
    (pp_list pp_signature)
    (List.sort (fun (ida, _) (idb, _) -> String.compare ida idb) sigs)
