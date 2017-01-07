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

  let equalise_ids = function
    | "def-lkb" -> "let-def"
    | "gfxmonk" -> "timbertson"
    | "msprotz" -> "protz"
    | "mmouratov" -> "cakeplus"
    | "iguer" -> "OCamlPro-Iguernlala"
    | "o-gu" -> "olgu"
    | "tr61" -> "tomjridge"
    | "OCamlPro-Henry" -> "hnrgrgr"
    | "blue-prawn" -> "fccm"
    | x -> x

  (* Maps github id -> mail address list *)
  let github_mail _dir commit pr github mail (g_m, m_g) =
    let github = equalise_ids github in
    if bad_mail mail then
      (Log.debug (fun m -> m "ignoring bad mail %s (%s) in %s (%s)" mail github pr commit) ; (g_m, m_g))
    else if S.mem github ignore_github then
      (Log.debug (fun m -> m "ignoring github id %s (%s) in %s (%s)" github mail pr commit) ; (g_m, m_g))
    else if S.mem mail ignore_mail then
      (Log.debug (fun m -> m "ignoring mail %s (%s) in %s (%s)" mail github pr commit) ; (g_m, m_g))
    else if S.mem pr ignore_pr then
      (Log.debug (fun m -> m "ignoring pr (%s -> %s) %s (%s)" github mail pr commit) ; (g_m, m_g))
    else
    let mails = try M.find github g_m with Not_found -> S.empty in
    let ids =
      try
        let ids = M.find mail m_g in
        if not (S.mem github ids) then
          Log.err (fun m -> m "%s (commit %s) mail address %s used for multiple accounts (here %s): %s\n"
                      pr commit mail github (String.concat ~sep:" " (S.elements ids))) ;
        ids
      with Not_found ->
        Log.debug (fun m -> m "new mapping %s -> %s in %s (%s)" github mail pr commit) ;
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

open Conex_resource

let pp_map pp t =
  M.iter (fun k v ->
      Format.fprintf pp "%s -> %s@." k (String.concat ~sep:" " (S.elements v)))
    t

let known_ids =
  [ `Key (Publickey.publickey ~accounts:[ `GitHub "yallop" ; `Email "yallop@gmail.com" ] "yallop" None) ;
    (* them all should be inferred further down *)
    `Key (Publickey.publickey ~accounts:[ `GitHub "seliopou" ; `Email "spiros@inhabitedtype.com" ; `Email "seliopou@gmail.com" ] "seliopou" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "lefessan" ; `Email "fabrice.le_fessant@inria.fr" ; `Email "fabrice@ocamlpro.com" ; `Email "fabrice.le_fessant@ocamlpro.com" ; `Email "fabrice@lefessant.net" ] "lefessan" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "c-cube" ; `Email "simon.cruanes.2007@m4x.org" ; `Email "simon.cruanes@inria.fr" ] "c-cube" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "agarwal" ; `Email "ashish@solvuu.com" ; `Email "agarwal1975@gmail.com" ] "agarwal" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "lpw25" ; `Email "lpw25@cl.cam.ac.uk" ; `Email "leo@lpw25.net" ] "lpw25" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "314eter" ; `Email "3.14.e.ter@gmail.com" ] "314eter" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "johnelse" ; `Email "john.else@gmail.com" ; `Email "john.else@eu.citrix.com" ] "johnelse" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "juergenhoetzel" ; `Email "juergen@archlinux.org" ; `Email "juergen@hoetzel.info" ] "juergenhoetzel" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "alainfrisch" ; `Email "alain.frisch@lexifi.com" ; `Email "alain@frisch.fr" ] "alainfrisch" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "johnwhitington" ; `Email "contact@coherentgraphics.co.uk" ; `Email "john@coherentgraphics.co.uk" ] "johnwhitington" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "darioteixeira" ; `Email "dario.teixeira@nleyten.com" ; `Email "darioteixeira@yahoo.com" ] "darioteixeira" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "rdicosmo" ; `Email "roberto@dicosmo.org" ; `Email "github@dicosmo.org" ] "rdicosmo" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "mzp" ; `Email "mzpppp@gmail.com" ; `Email "mzp.ppp@gmail.com" ] "mzp" None) ;
    `Key (Publickey.publickey ~accounts:[ `GitHub "andrewray" ; `Email "evilkidder@gmail.com" ; `Email "andy.ray@ujamjar.com" ] "andrewray" None) ;
    `Team ("opensource@janestreet.com", "janestreet") ;
    `Team ("xen-api@lists.xen.org", "xapi-project") ;
    `Team ("contact@ocamlpro.com", "OCamlPro") ;
    `Team ("dev@ocsigen.org", "ocsigen") ;
    `Team ("mirageos-devel@lists.openmirage.org", "MirageOS") ;
    `Team ("mirageos-devel", "MirageOS")
  ]

let infer_maintainers base lvl =
  Logs.set_level lvl ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  let provider = Conex_provider.fs_provider base in
  let repo = Repository.repository provider in
  let github_mail, _mail_github = PR.handle_prs base PR.github_mail (M.empty, M.empty) in
  Logs.info (fun m -> m "github-email %a" pp_map github_mail) ;
  let repo = M.fold (fun k v repo ->
      let accounts = `GitHub k :: List.map (fun e -> `Email e) (S.elements v) in
      let key = Publickey.publickey ~accounts k None in
      Repository.write_key repo key ;
      Repository.add_trusted_key repo key)
      github_mail repo
  in
  let repo = List.fold_left (fun r ->
      function
      | `Team (mail, id) ->
        let t = Team.team id in
        Repository.write_team r t ;
        let r = Repository.add_trusted_key r (Publickey.publickey ~accounts:[`GitHub id; `Email mail] id None) in
        Repository.add_team r t
      | `Key k -> Repository.write_key repo k ;Repository.add_trusted_key r k)
      repo known_ids
  in
  (* this is a set of package name without maintainer,
     and a map pkgname -> email address (or name) set *)
  let _empty, mapping = OpamMaintainer.infer repo in
  let _authorisations = M.fold (fun k v acc ->
      let ids = s_of_list v in
      let authorised = S.fold (fun s acc ->
          match Repository.find_id repo s with
          | None -> acc
          | Some x -> S.add x acc)
          ids S.empty
      in
      let authorisation = Authorisation.authorisation ~authorised k in
      Logs.info (fun m -> m "%a" Authorisation.pp_authorisation authorisation) ;
      Repository.write_authorisation repo authorisation ;
      authorisation :: acc)
      mapping []
  in
  (* NOW! we can go through all the diffs of the PRs and look around a bit:
     - filter out maintainers for commits (seeded by samoht, yallop, altgr, avsm, damiendoligez, dsheets?, camlspotter?, diml?, zoggy?)
     - add maintainer mail address to keys/ subdir if sensible *)
  ()


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
