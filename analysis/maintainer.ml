open Conex_core
open Astring

module OpamMaintainer = struct
  let src = Logs.Src.create "opam-maintainer" ~doc:"Opam Maintainer module"
  module Log = (val Logs.src_log src : Logs.LOG)

  let find_m acc s = match String.cut ~sep:"<" s with
    | None -> begin match String.cut ~sep:"@" s with
        | None -> acc
        | Some (l, post) -> match String.cut ~sep:" " l with
          | None -> s :: acc
          | Some (_, pre) -> (pre ^ "@" ^ post) :: acc
      end
    | Some (_, r) -> match String.cut ~sep:">" r with
      | Some (l, _) -> l :: acc
      | None -> Log.err (fun m -> m "cannot parse maintainer %s" s) ; acc

  let replacements = [
    " [at] ", "@" ;
    "[at]", "@" ;
    " at ", "@" ;
    " (at) ", "@" ;
    "buenzl i", "buenzli" ;
    "Ben Greenman", "types@ccs.neu.edu" ;
  ]

  let sanitize_mail str =
    let escape1 sep replacement data =
      let pieces = String.cuts ~sep data in
      String.concat ~sep:replacement pieces
    in
    List.fold_left
      (fun s (c, r) -> escape1 c r s)
      str replacements

  let mirage_packages = s_of_list [ "io-page" ; "cstruct" ; "dns" ; "tcpip" ; "vmnet" ; "channel" ; "fat-filesystem" ; "pcap-format" ; "functoria" ; "mbr-format" ; "protocol-9p" ; "qcow-format" ; "vchan" ; "gmp-freestanding" ; "gmp-xen" ; "zarith-xen" ; "zarith-freestanding" ; "conduit" ; "cow"]
  let xapi_packages = s_of_list [ "vhdlib" ; "vhd-format" ; "nbd" ; "cdrom"]

  let maintainers provider p r =
    (* contact@ocamlpro.com -- good if tag org:ocamlpro is around (opam lint does this)
       opam-devel@lists.ocaml.org *)
    if String.is_prefix ~affix:"mirage" p then S.singleton "mirage" else
    if S.mem p mirage_packages then S.singleton "mirage" else
    if String.is_prefix ~affix:"xapi" p then S.singleton "xapi-project" else
    if String.is_prefix ~affix:"xen" p then S.singleton "xapi-project" else
    if S.mem p xapi_packages then S.singleton "xapi-project" else
    if String.is_prefix ~affix:"ocp" p then S.singleton "OCamlPro" else
    match provider.Provider.read ["packages" ; p ; r ; "opam" ] with
    | Ok data ->
      let opam = OpamFile.OPAM.read_from_string data in
      let ms = S.of_list
          (List.map sanitize_mail
             (List.fold_left find_m [] (OpamFile.OPAM.maintainer opam)))
      in
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
    let packages = Conex_repository.items repo in
    List.fold_left (fun map p ->
        let releases = Conex_opam_layout.subitems (Conex_repository.provider repo) p in
        let releases = List.rev (List.sort OpamVersionCompare.compare releases) in
        let maintainers =
          List.fold_left
            (fun s r ->
               if S.is_empty s then
                 maintainers (Conex_repository.provider repo) p r
               else
                 s)
            S.empty releases
        in
        if S.is_empty maintainers then
          Log.warn (fun m -> m "empty maintainer for %s" p)
        else
          Log.info (fun m -> m "maintainers for %s are %s"
                       p (String.concat ~sep:" " (S.elements maintainers))) ;
        M.add p maintainers map)
      M.empty
      (S.elements packages)
end

open Conex_resource

module PR = struct
  let src = Logs.Src.create "pr" ~doc:"Pull request module"
  module Log = (val Logs.src_log src : Logs.LOG)

  (* mostly organisations *)
  let ignore_github = S.of_list [
      "mirage" ; "bactrian" ; "OCamlPro" ; "ocaml" ; "janestreet" ;
      "jane-street" ; "BinaryAnalysisPlatform" ; "ocaml-core-dev" ;
      "camlunity" ; "Beluga-lang" ; "Incubaid" ;
      "rundat" ; "planar" ; "ahrefs"
    ]

  let ignore_pr = S.of_list [ "3118" ; "6939" ; "3900" ; "8170" ; "1825" ; "651" ; "636" ]

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
          Log.err (fun m -> m "%s (commit %s) mail address %s used for multiple accounts (here %s): %s"
                      pr commit mail github (String.concat ~sep:" " (S.elements ids))) ;
        ids
      with Not_found ->
        Log.debug (fun m -> m "new mapping %s -> %s in %s (%s)" github mail pr commit) ;
        S.empty
    in
    (M.add github (S.add mail mails) g_m, M.add mail (S.add github ids) m_g)

  let janitors = s_of_list [
      "avsm" ; "mirage" ; "samoht" ; "gasche" ;
      "damiendoligez" ; "AltGr" ; "yallop" ; "dsheets" ;

      "jane-street" ; "ocaml-core-dev" ; "vbmithr" ; "tuong" ; "lefessan" ;
      "whitequark" ; "fccm" ; "planar" ; "Chris00" ; "chambart" ;
      "ocaml" ; "OCamlPro" ; "timbertson" ; "Drup" ; "janestreet" ;
      "camlunity" ; "mmottl" ; "BinaryAnalysisPlatform"]

  let mteams = s_of_list [ "mirage" ; "janestreet" ; "ocsigen" ; "xapi-project" ; "alt-ergo" ]

  let check authorised base commit pr github _mail acc =
    if S.mem github janitors then (Log.info (fun m -> m "%s %s janitor" pr github) ; acc) else
    let content = Conex_persistency.read_file
        (Filename.concat base (Filename.concat "diffs" (commit ^ ".diff")))
    in
    let diffs = Conex_diff.to_diffs content in
    let _ids, _auths, _rels, packages = Conex_diff.diffs_to_components diffs in

    let add_it p map =
      M.add p (S.add github (try M.find p map with Not_found -> S.empty)) map
    in
    M.fold (fun pn _pvs (empty, violation, teams) ->
        if Conex_persistency.exists
            (Filename.concat base (String.concat ~sep:"/" (Conex_opam_layout.authorisation_path pn)))
        then
          let auth = M.find pn authorised in
          let authorised = auth.Authorisation.authorised in
          if S.mem github authorised then begin
            Log.debug (fun m -> m "%s %s %s valid access" pr pn github) ;
            (empty, violation, teams)
          end else if S.cardinal authorised = 1 && S.mem (S.choose authorised) mteams then begin
            Log.warn (fun m -> m "%s %s %s team owned, add!? (%s)" pr pn github (S.choose authorised)) ;
            (empty, violation, add_it (S.choose authorised) teams)
          end else if S.is_empty authorised then begin
              Log.info (fun m -> m "%s %s %s (empty maintainer)" pr pn github) ;
              (add_it pn empty, violation, teams)
          end else begin
            Log.warn (fun m -> m "%s %s %s not maintainer (%s)" pr pn github (String.concat ~sep:" " (S.elements authorised))) ;
            (empty, add_it pn violation, teams)
          end
        else begin
          Log.debug (fun m -> m "%s %s ignoring deleted package" pr pn) ;
          (empty, violation, teams)
        end)
      packages acc

  let handle_prs dir f acc =
    let base = Filename.concat dir "prs" in
    let prs = Conex_persistency.collect_dir base in
    List.fold_left
      (fun acc pr ->
         let data = Conex_persistency.read_file (Filename.concat base pr) in
         let eles = Astring.String.cuts ~sep:" " data in
         let cid = List.nth eles 0
         and pr = List.nth eles 1
         and gid = equalise_ids (List.nth eles 2)
         and mail = String.trim (List.nth eles 3)
         in
         f dir cid pr gid mail acc)
      acc prs
end

let pp_map pp t =
  M.iter (fun k v ->
      let s =
        if S.is_empty v then "EMPTY "
        else if S.cardinal v = 1 then "SINGLE "
        else ""
      in
      Format.fprintf pp "%s%s -> %s@." s k (String.concat ~sep:" " (S.elements v)))
    t

let known_ids =
  [ `Key (Index.index ~accounts:[ `GitHub "yallop" ; `Email "yallop@gmail.com" ] "yallop") ;
    (* them all should be inferred further down *)
    `Key (Index.index ~accounts:[ `GitHub "seliopou" ; `Email "spiros@inhabitedtype.com" ; `Email "seliopou@gmail.com" ] "seliopou") ;
    `Key (Index.index ~accounts:[ `GitHub "lefessan" ; `Email "fabrice.le_fessant@inria.fr" ; `Email "fabrice@ocamlpro.com" ; `Email "fabrice.le_fessant@ocamlpro.com" ; `Email "fabrice@lefessant.net" ] "lefessan") ;
    `Key (Index.index ~accounts:[ `GitHub "c-cube" ; `Email "simon.cruanes.2007@m4x.org" ; `Email "simon.cruanes@inria.fr" ] "c-cube") ;
    `Key (Index.index ~accounts:[ `GitHub "agarwal" ; `Email "ashish@solvuu.com" ; `Email "agarwal1975@gmail.com" ] "agarwal") ;
    `Key (Index.index ~accounts:[ `GitHub "lpw25" ; `Email "lpw25@cl.cam.ac.uk" ; `Email "leo@lpw25.net" ] "lpw25") ;
    `Key (Index.index ~accounts:[ `GitHub "314eter" ; `Email "3.14.e.ter@gmail.com" ] "314eter") ;
    `Key (Index.index ~accounts:[ `GitHub "johnelse" ; `Email "john.else@gmail.com" ; `Email "john.else@eu.citrix.com" ; `Email "john.else@citrix.com" ] "johnelse") ;
    `Key (Index.index ~accounts:[ `GitHub "juergenhoetzel" ; `Email "juergen@archlinux.org" ; `Email "juergen@hoetzel.info" ] "juergenhoetzel") ;
    `Key (Index.index ~accounts:[ `GitHub "alainfrisch" ; `Email "alain.frisch@lexifi.com" ; `Email "alain@frisch.fr" ] "alainfrisch") ;
    `Key (Index.index ~accounts:[ `GitHub "johnwhitington" ; `Email "contact@coherentgraphics.co.uk" ; `Email "john@coherentgraphics.co.uk" ] "johnwhitington") ;
    `Key (Index.index ~accounts:[ `GitHub "darioteixeira" ; `Email "dario.teixeira@nleyten.com" ; `Email "darioteixeira@yahoo.com" ] "darioteixeira") ;
    `Key (Index.index ~accounts:[ `GitHub "rdicosmo" ; `Email "roberto@dicosmo.org" ; `Email "github@dicosmo.org" ] "rdicosmo") ;
    `Key (Index.index ~accounts:[ `GitHub "mzp" ; `Email "mzpppp@gmail.com" ; `Email "mzp.ppp@gmail.com" ] "mzp") ;
    `Key (Index.index ~accounts:[ `GitHub "andrewray" ; `Email "evilkidder@gmail.com" ; `Email "andy.ray@ujamjar.com" ] "andrewray") ;
    `Key (Index.index ~accounts:[ `GitHub "codinuum" ; `Email "codinuum@users.noreply.github.com" ; `Email "codinuum@me.com" ] "codinuum") ;
    `Key (Index.index ~accounts:[ `GitHub "raphael-proust" ; `Email "raphael.proust@cl.cam.ac.uk" ; `Email "raphlalou@gmail.com" ] "raphael-proust") ;
    `Key (Index.index ~accounts:[ `GitHub "madroach" ; `Email "madroach@gmerlin.de" ; `Email "christopher@gmerlin.de" ] "madroach") ;
    `Key (Index.index ~accounts:[ `GitHub "testcocoon" ; `Email "sebastien.fricker@gmail.com" ; `Email "fricker@froglogic.com" ] "testcocoon") ;
    `Key (Index.index ~accounts:[ `GitHub "protz" ; `Email "protz@microsoft.com" ; `Email "jonathan.protzenko@inria.fr" ; `Email "jonathan.protzenko@gmail.com" ] "protz") ;
    `Key (Index.index ~accounts:[ `GitHub "AngryLawyer" ; `Email "tony@angry-lawyer.com" ; `Email "tony@dabapps.com" ] "AngryLawyer") ;
    `Key (Index.index ~accounts:[ `GitHub "rixed" ; `Email "rixed-opam@happyleptic.org" ; `Email "rixed@happyleptic.org" ] "rixed") ;
    `Key (Index.index ~accounts:[ `GitHub "backtracking" ; `Email "filliatr@lri.fr" ; `Email "Jean-Christophe.Filliatre@lri.fr" ] "backtracking") ;
    `Key (Index.index ~accounts:[ `GitHub "andrenth" ; `Email "andre@digirati.com.br" ; `Email "andrenth@gmail.com" ] "andrenth") ;
    `Key (Index.index ~accounts:[ `GitHub "mathiasbourgoin" ; `Email "mathias.bourgoin@gmail.com" ; `Email "mathias.bourgoin@lip6.fr" ] "mathiasbourgoin") ;
    `Key (Index.index ~accounts:[ `GitHub "pmundkur" ; `Email "prashanth.mundkur@gmail.com" ; `Email "pmundkur.ocaml@gmail.com" ] "pmundkur") ;
    `Key (Index.index ~accounts:[ `GitHub "pmeunier" ; `Email "pe@dhcp-135-214.caltech.edu" ; `Email "pe@patoline.org" ] "pmeunier") ;
    `Key (Index.index ~accounts:[ `GitHub "eatonphil" ; `Email "philip@Philips-MacBook-Pro.local" ; `Email "me@eatonphil.com" ] "eatonphil") ;
    `Team ([ "opensource@janestreet.com" ], "janestreet") ;
    `Team ([ "xen-api@lists.xen.org" ], "xapi-project") ;
    `Team ([ "contact@ocamlpro.com" ], "OCamlPro") ;
    `Team ([ "dev@ocsigen.org" ], "ocsigen") ;
    `Team ([ "alt-ergo@ocamlpro.com" ], "alt-ergo") ;
    `Team ([ "mirageos-devel@lists.openmirage.org" ; "mirageos-devel@lists.xenproject.org" ; "mirageos-devel" ], "mirage")
  ]

let infer_maintainers base lvl =
  Logs.set_level lvl ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  let provider = Conex_provider.fs_provider base in
  let repo = Conex_repository.repository provider in
  let github_mail, _mail_github = PR.handle_prs base PR.github_mail (M.empty, M.empty) in
  Logs.info (fun m -> m "github-email %a" pp_map github_mail) ;
  let idxs = M.fold (fun k v acc ->
      let accounts = `GitHub k :: List.map (fun e -> `Email e) (S.elements v) in
      let idx = Index.index ~accounts k in
      Conex_repository.write_index repo idx ;
      idx :: acc)
      github_mail []
  in
  let more_idxs = List.fold_left (fun acc ->
      function
      | `Team (mails, id) ->
        let t = Team.team id in
        Conex_repository.write_team repo t ;
        let idx =
          let accounts = `GitHub id :: List.map (fun e -> `Email e) mails in
          Index.index ~accounts id
        in
        idx :: acc
      | `Key k ->
        Conex_repository.write_index repo k ;
        k :: acc)
      [] known_ids
  in

  let find_by_mail email =
    List.fold_left (fun r k ->
        match r with
        | None ->
          let contains =
            let f = function `Email e when e = email -> true | _ -> false in
            List.exists f k.Index.accounts
          in
          if contains then Some k.Index.name else None
        | Some x -> Some x)
      None (idxs @ more_idxs)
  in
  (* this is a set of package name without maintainer,
     and a map pkgname -> email address (or name) set *)
  let mapping = OpamMaintainer.infer repo in
  let authorisations = M.fold (fun k ids acc ->
      let authorised = S.fold (fun s acc ->
          if String.is_infix ~affix:"@" s then
            match find_by_mail s with
            | None -> acc
            | Some x -> S.add x acc
          else
            S.add s acc)
          ids S.empty
      in
      let authorisation = Authorisation.authorisation ~authorised k in
      Logs.info (fun m -> m "%a" Authorisation.pp_authorisation authorisation) ;
      Conex_repository.write_authorisation repo authorisation ;
      M.add k authorisation acc)
      mapping M.empty
  in
  (* authorisation contains pkgname -> github id
     mapping contains pkgname -> mail address
     if a team is authorised, maybe add committer to team?
  *)
  (* NOW! we can go through all the diffs of the PRs and look around a bit:
     - filter out maintainers for commits
     - add maintainer mail address to id/ subdir if sensible *)
  Logs.app (fun m -> m "MARK") ;
  let empty, _violation, team = PR.handle_prs base (PR.check authorisations) (M.empty, M.empty, M.empty) in
  Logs.app (fun m -> m "potentially\n%a" pp_map empty) ;
  Logs.app (fun m -> m "teams\n%a" pp_map team) ;
  M.iter (fun t members ->
      let t = Team.team ~members t in
      Conex_repository.write_team repo t)
    team ;
  M.iter (fun p authorised -> if S.cardinal authorised = 1 then
             let a = Authorisation.authorisation ~authorised p in
             Conex_repository.write_authorisation repo a)
    empty

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
