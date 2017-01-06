open Conex_core
open Astring



module OpamMaintainer = struct
  let src = Logs.Src.create "opam-maintainer" ~doc:"Opam Maintainer module"
  module Log = (val Logs.src_log src : Logs.LOG)

  let find_m s = match String.cut ~sep:"<" s with
    | None -> s
    | Some (_, r) -> match String.cut ~sep:">" r with
      | Some (l, _) -> l
      | None -> Log.err (fun m -> m "cannot parse maintainer %s" s) ; s

  let replacements = [
    " [at] ", "@" ;
    "[at]", "@" ;
    " at ", "@" ;
    " (at) ", "@" ;
    "buenzl i", "buenzli"
  ]

  let sanitize_mail str =
    let escape1 sep replacement data =
      let pieces = String.cuts ~sep data in
      String.concat ~sep:replacement pieces
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
      if S.mem "opam-devel@lists.ocaml.org" ms then
        Log.info (fun m -> m "removing opam-devel@lists.ocaml.org from %s" p) ;
      let ms = S.remove "opam-devel@lists.ocaml.org" ms in
      if S.mem "contact@ocamlpro.com" ms then
        if List.mem "org:ocamlpro" (OpamFile.OPAM.tags opam) then
          ms
        else begin
          Log.info (fun m -> m "removed contact@ocamlpro.com from %s" p) ;
          S.remove "contact@ocamlpro.com" ms
        end
      else
        ms
    | Error _ -> Log.err (fun m -> m "couldn't read opam file: %s/%s" p r) ; S.empty

  let infer repo =
    let packages = Repository.items repo in
    List.fold_left (fun (es, map) p ->
        let releases = Conex_opam_layout.subitems (Repository.provider repo) p in
        let maintainers =
          List.fold_left
            (fun s r -> S.union (maintainers (Repository.provider repo) p r) s)
            S.empty releases
        in
        match S.elements maintainers with
        | [] ->
          Log.warn (fun m -> m "empty maintainer for %s" p) ;
          (S.add p es, map)
        | xs ->
          Log.info (fun m -> m "maintainers for %s are %s" p (String.concat ~sep:" " xs)) ;
          (es, M.add p xs map))
      (S.empty, M.empty)
      (S.elements packages)
end

module PR = struct
  let src = Logs.Src.create "pr" ~doc:"Pull request module"
  module Log = (val Logs.src_log src : Logs.LOG)

  (* mostly organisations *)
  let ignore_github = S.of_list [
      "mirage" ; "bactrian" ; "OCamlPro" ; "ocaml" ; "janestreet" ;
      "jane-street" ; "BinaryAnalysisPlatform" ; "ocaml-core-dev" ;
      "camlunity" ; "backtracking" ; "Beluga-lang" ; "Incubaid" ;
      "rundat" ; "planar" ; "ahrefs"
    ]

  let ignore_pr = S.of_list [ "3118" ; "6939" ; "3900" ; "8170" ; "1825" ]

  (* somehow the shell scripts put him everywhere *)
  let ignore_mail = S.of_list [ "yallop@gmail.com" ]

  let bad_mail m =
    match String.cut ~sep:"@" m with
    | None -> true
    | Some (_, r) -> r = "local" || r = "???.???" || r = "counterfeit-monkey.(none)"

  let equal_ids = [
      S.of_list [ "def-lkb" ; "let-def" ] ;
      S.of_list [ "timbertson" ; "gfxmonk" ] ;
      S.of_list [ "protz" ; "msprotz" ] ;
      S.of_list [ "cakeplus" ; "mmouratov" ] ;
      S.of_list [ "iguer" ; "OCamlPro-Iguernlala" ] ;
      S.of_list [ "olgu" ; "o-gu" ] ;
      S.of_list [ "tr61" ; "tomjridge" ] ;
      S.of_list [ "hnrgrgr" ; "OCamlPro-Henry" ] ;
      S.of_list [ "fccm" ; "blue-prawn" ]
    ]

  (* Maps github id -> mail address list *)
  let github_mail _dir commit pr github mail (g_m, m_g) =
    if bad_mail mail then
      (Log.info (fun m -> m "ignoring bad mail %s (%s) in %s (%s)" mail github pr commit) ; (g_m, m_g))
    else if S.mem github ignore_github then
      (Logs.info (fun m -> m "ignoring github id %s (%s) in %s (%s)" github mail pr commit) ; (g_m, m_g))
    else if S.mem mail ignore_mail then
      (Logs.info (fun m -> m "ignoring mail %s (%s) in %s (%s)" mail github pr commit) ; (g_m, m_g))
    else if S.mem pr ignore_pr then
      (Logs.info (fun m -> m "ignoring pr (%s -> %s) %s (%s)" github mail pr commit) ; (g_m, m_g))
    else
    let mails = try M.find github g_m with Not_found -> S.empty in
    let ids =
      try
        let ids = M.find mail m_g in
        if not (S.mem github ids) then begin
          let eq = try
              let set = List.find (fun e -> S.mem github e) equal_ids in
              S.subset ids set
            with Not_found -> false
          in
          if not eq then begin
            Log.err (fun m -> m "%s (commit %s) mail address %s used for multiple accounts (here %s): %s\n"
                         pr commit mail github (String.concat ~sep:" " (S.elements ids)))
          end
        end ;
        ids
      with Not_found ->
        Log.info (fun m -> m "new mapping %s -> %s in %s (%s)" github mail pr commit) ;
        S.empty
    in
    (M.add github (S.add mail mails) g_m, M.add mail (S.add github ids) m_g)

  let handle_prs dir f acc =
    let base = Filename.concat dir "prs" in
    let prs = Conex_persistency.collect_dir base in
    List.fold_left
      (fun acc pr ->
         let data = Conex_persistency.read_file (Filename.concat base pr) in
         let eles = Astring.String.cuts ~sep:" " data in
         let cid = List.nth eles 0
         and pr = List.nth eles 1
         and gid = List.nth eles 2
         and mail = String.trim (List.nth eles 3)
         in
         f dir cid pr gid mail acc)
      acc prs
end

let pp_map pp t =
  M.iter (fun k v ->
      Format.fprintf pp "%s -> %s@." k (String.concat ~sep:" " (S.elements v)))
    t

let infer_maintainers base lvl =
  Logs.set_level lvl ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  let provider = Conex_provider.fs_provider base in
  let _repo = Repository.repository provider in
  let github_mail, _mail_github = PR.handle_prs base PR.github_mail (M.empty, M.empty) in
  pp_map Format.std_formatter github_mail
  (* this is a set of package name without maintainer,
     and a map pkgname -> email address (or name) list *)
(*  let empty, mapping = OpamMaintainer.infer repo in
    () *)

open Cmdliner

let repo =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"repository"
         ~doc:"The full path to the repository")

let cmd =
  let doc = "Opam maintainer inference" in
  let man = [
    `S "DESCRIPTION" ;
    `P "$(tname) infers actual maintainers of opam packages"
  ] in
  Term.(pure infer_maintainers $ repo $ Logs_cli.level ()),
  Term.info "maintainer" ~version:"%%VERSION_NUM%%" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
