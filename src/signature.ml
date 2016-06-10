open Core

type t = identifier * string

let extend_data data id kind =
  String.concat " " [ data ; id ; kind_to_string kind ]

let pp_signature ppf (id, sv) =
  Format.fprintf ppf "%s sig:@ %s" id sv

let pp_signatures ppf sigs =
  Format.fprintf ppf "%d signatures:@." (List.length sigs) ;
  List.iter (fun s -> pp_signature ppf s ; Format.pp_print_newline ppf ())
            (List.sort (fun (ida, _) (idb, _) -> String.compare ida idb) sigs)
