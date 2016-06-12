open Core

type t = identifier * string

let extend_data data id kind =
  String.concat " " [ data ; id ; kind_to_string kind ]

let pp_signature ppf (id, sv) =
  Format.fprintf ppf "%a sig:@ %s" pp_id id sv

let pp_signatures ppf sigs =
  Format.fprintf ppf "%d signatures:@.%a" (List.length sigs)
    (pp_list pp_signature)
    (List.sort (fun (ida, _) (idb, _) -> String.compare ida idb) sigs)
