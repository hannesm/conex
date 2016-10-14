

let handle_file filename =
  let content = Persistency.read_file filename in
  let diffs = Diff.to_diffs content in
  let comps = Patch.diffs_to_components diffs in
  List.iteri (fun i d -> Format.fprintf Format.std_formatter "%d: %a" i Patch.pp_component d) comps

let () =
  match Sys.argv with
  | [| _ ; diff |] -> handle_file diff
  | _ -> invalid_arg "expecting exactly one argument"
