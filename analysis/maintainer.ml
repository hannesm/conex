open Conex_core

let find_m s = match Astring.String.cut ~sep:"<" s with
  | None -> s
  | Some (_, r) -> match Astring.String.cut ~sep:">" r with
    | Some (l, _) -> l
    | None -> invalid_arg ("cannot parse maintainer " ^ s)

let replacements = [
  " [at] ", "@" ;
  "[at]", "@" ;
  " at ", "@" ;
  " (at) ", "@" ;
  "buenzl i", "buenzli"
]

let sanitize_mail str =
  let escape1 sep replacement data =
    let pieces = Astring.String.cuts ~sep data in
    String.concat replacement pieces
  in
  List.fold_left
    (fun s (c, r) -> escape1 c r s)
    str replacements

let maintainers provider p r =
  (* contact@ocamlpro.com -- good if tag org:ocamlpro is around (opam lint does this)
    opam-devel@lists.ocaml.org *)
  match provider.Provider.read ["packages" ; p ; r ; "opam" ] with
    | Ok data ->
      let opam = OpamFile.OPAM.read_from_string data in
      let ms = S.of_list (List.map sanitize_mail (List.map find_m (OpamFile.OPAM.maintainer opam))) in
      let ms = S.remove "opam-devel@lists.ocaml.org" ms in
      if S.mem "contact@ocamlpro.com" ms then
        if List.mem "org:ocamlpro" (OpamFile.OPAM.tags opam) then
          ms
        else
          S.remove "contact@ocamlpro.com" ms
      else
        ms
    | Error _ -> invalid_arg ("couldn't read opam file: " ^ p ^ "/" ^ r)

let mainloop repo =
  let provider = Conex_provider.fs_provider repo in
  let repo = Repository.repository provider in
  let packages = Repository.items repo in
  let empty =
    List.fold_left (fun acc p ->
        let releases = Conex_opam_layout.subitems provider p in
        let maintainers =
          List.fold_left
            (fun s r -> S.union (maintainers provider p r) s)
            S.empty releases
        in
        match S.elements maintainers with
        | [] -> S.add p acc
        | xs ->
          let auth = Conex_resource.Authorisation.authorisation ~authorised:(s_of_list xs) p in
          Repository.write_authorisation repo auth ; acc)
      S.empty
      (S.elements packages)
  in
  let empties = List.sort String.compare (S.elements empty) in
  let fd = Unix.openfile "unmaintained" [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let str = String.concat "\n" empties in
  ignore(Unix.write fd (Bytes.of_string str) 0 (String.length str)) ;
  Unix.close fd ;
  Printf.printf "%d packages without maintainer\n" (S.cardinal empty)

let () = match Sys.argv with
  | [| _ ; repo |] -> mainloop repo
  | _ -> invalid_arg "expecting one argument"
