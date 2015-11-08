open Core

type t = identifier * algorithm * string

let extend_data data alg id =
  data ^ ",algorithm:" ^ (algorithm_to_string alg) ^ ",id:" ^ id

let pp_signature ppf (id, alg, sv) =
  Format.fprintf ppf "%s used %s, sig:@ %s"
                 id (algorithm_to_string alg) sv

let pp_signatures ppf sigs =
  Format.fprintf ppf "%d signatures:@." (List.length sigs) ;
  List.iter (fun s -> pp_signature ppf s ; Format.pp_print_newline ppf ())
            (List.sort (fun (ida, _, _) (idb, _, _) -> String.compare ida idb) sigs)
