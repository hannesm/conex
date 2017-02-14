open Conex_utils
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

  let maintainers io p r =
    (* contact@ocamlpro.com -- good if tag org:ocamlpro is around (opam lint does this)
       opam-devel@lists.ocaml.org *)
    if String.is_prefix ~affix:"mirage" p then S.singleton "mirage" else
    if S.mem p mirage_packages then S.singleton "mirage" else
    if String.is_prefix ~affix:"xapi" p then S.singleton "xapi-project" else
    if String.is_prefix ~affix:"xen" p then S.singleton "xapi-project" else
    if S.mem p xapi_packages then S.singleton "xapi-project" else
    if String.is_prefix ~affix:"ocp" p then S.singleton "OCamlPro" else
    match io.Conex_io.read ["packages" ; p ; r ; "opam" ] with
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

  let infer io =
    let packages = match Conex_io.packages io with Error e -> invalid_arg e | Ok xs -> xs in
    List.fold_left (fun map p ->
        let rels = match Conex_io.releases io p with
          | Error _ -> []
          | Ok s -> S.elements s
        in
        let releases = List.rev (List.sort OpamVersionCompare.compare rels) in
        let maintainers =
          List.fold_left
            (fun s r ->
               if S.is_empty s then
                 maintainers io p r
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
      (Log.debug (fun m -> m "ignoring bad mail %s (%s) in %s (%s)" mail github pr commit) ; Ok (g_m, m_g))
    else if S.mem github ignore_github then
      (Log.debug (fun m -> m "ignoring github id %s (%s) in %s (%s)" github mail pr commit) ; Ok (g_m, m_g))
    else if S.mem mail ignore_mail then
      (Log.debug (fun m -> m "ignoring mail %s (%s) in %s (%s)" mail github pr commit) ; Ok (g_m, m_g))
    else if S.mem pr ignore_pr then
      (Log.debug (fun m -> m "ignoring pr (%s -> %s) %s (%s)" github mail pr commit) ; Ok (g_m, m_g))
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
    Ok (M.add github (S.add mail mails) g_m, M.add mail (S.add github ids) m_g)

  let janitors = s_of_list [
      "avsm" ; "mirage" ; "samoht" ; "gasche" ;
      "damiendoligez" ; "AltGr" ; "yallop" ; "dsheets" ;

      "jane-street" ; "ocaml-core-dev" ; "vbmithr" ; "tuong" ; "lefessan" ;
      "whitequark" ; "fccm" ; "planar" ; "Chris00" ; "chambart" ;
      "ocaml" ; "OCamlPro" ; "timbertson" ; "Drup" ; "janestreet" ;
      "camlunity" ; "mmottl" ; "BinaryAnalysisPlatform"]

  let mteams = s_of_list [ "mirage" ; "janestreet" ; "ocsigen" ; "xapi-project" ; "alt-ergo" ]

  let check authorised base commit pr github _mail acc =
    if S.mem github janitors then (Log.info (fun m -> m "%s %s janitor" pr github) ; Ok acc) else
    Conex_unix_persistency.read_file
      (Filename.concat base (Filename.concat "diffs" (commit ^ ".diff"))) >>= fun content ->
    let diffs = Conex_diff.to_diffs content in
    let _ids, _auths, _rels, packages = Conex_diff.diffs_to_components diffs in

    let add_it p map =
      M.add p (S.add github (try M.find p map with Not_found -> S.empty)) map
    in
    let stuff =
      M.fold (fun pn _pvs (empty, violation, teams) ->
          if Conex_unix_persistency.exists
              (Filename.concat base (String.concat ~sep:"/" (Conex_opam_repository_layout.authorisation_path pn)))
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
    in
    Ok stuff

  let handle_prs dir f acc =
    let base = Filename.concat dir "prs" in
    Conex_unix_persistency.collect_dir base >>= fun prs ->
    foldM
      (fun acc pr ->
         Conex_unix_persistency.read_file (Filename.concat base pr) >>= fun data ->
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
  [ `Key (Author.t ~accounts:[ `GitHub "yallop" ; `Email "yallop@gmail.com" ] Uint.zero "yallop") ;
    (* them all should be inferred further down *)
    `Key (Author.t ~accounts:[ `GitHub "seliopou" ; `Email "spiros@inhabitedtype.com" ; `Email "seliopou@gmail.com" ] Uint.zero "seliopou") ;
    `Key (Author.t ~accounts:[ `GitHub "lefessan" ; `Email "fabrice.le_fessant@inria.fr" ; `Email "fabrice@ocamlpro.com" ; `Email "fabrice.le_fessant@ocamlpro.com" ; `Email "fabrice@lefessant.net" ] Uint.zero "lefessan") ;
    `Key (Author.t ~accounts:[ `GitHub "c-cube" ; `Email "simon.cruanes.2007@m4x.org" ; `Email "simon.cruanes@inria.fr" ] Uint.zero "c-cube") ;
    `Key (Author.t ~accounts:[ `GitHub "agarwal" ; `Email "ashish@solvuu.com" ; `Email "agarwal1975@gmail.com" ] Uint.zero "agarwal") ;
    `Key (Author.t ~accounts:[ `GitHub "lpw25" ; `Email "lpw25@cl.cam.ac.uk" ; `Email "leo@lpw25.net" ] Uint.zero "lpw25") ;
    `Key (Author.t ~accounts:[ `GitHub "314eter" ; `Email "3.14.e.ter@gmail.com" ] Uint.zero "314eter") ;
    `Key (Author.t ~accounts:[ `GitHub "johnelse" ; `Email "john.else@gmail.com" ; `Email "john.else@eu.citrix.com" ; `Email "john.else@citrix.com" ] Uint.zero "johnelse") ;
    `Key (Author.t ~accounts:[ `GitHub "juergenhoetzel" ; `Email "juergen@archlinux.org" ; `Email "juergen@hoetzel.info" ] Uint.zero "juergenhoetzel") ;
    `Key (Author.t ~accounts:[ `GitHub "alainfrisch" ; `Email "alain.frisch@lexifi.com" ; `Email "alain@frisch.fr" ] Uint.zero "alainfrisch") ;
    `Key (Author.t ~accounts:[ `GitHub "johnwhitington" ; `Email "contact@coherentgraphics.co.uk" ; `Email "john@coherentgraphics.co.uk" ] Uint.zero "johnwhitington") ;
    `Key (Author.t ~accounts:[ `GitHub "darioteixeira" ; `Email "dario.teixeira@nleyten.com" ; `Email "darioteixeira@yahoo.com" ] Uint.zero "darioteixeira") ;
    `Key (Author.t ~accounts:[ `GitHub "rdicosmo" ; `Email "roberto@dicosmo.org" ; `Email "github@dicosmo.org" ] Uint.zero "rdicosmo") ;
    `Key (Author.t ~accounts:[ `GitHub "mzp" ; `Email "mzpppp@gmail.com" ; `Email "mzp.ppp@gmail.com" ] Uint.zero "mzp") ;
    `Key (Author.t ~accounts:[ `GitHub "andrewray" ; `Email "evilkidder@gmail.com" ; `Email "andy.ray@ujamjar.com" ] Uint.zero "andrewray") ;
    `Key (Author.t ~accounts:[ `GitHub "codinuum" ; `Email "codinuum@users.noreply.github.com" ; `Email "codinuum@me.com" ] Uint.zero "codinuum") ;
    `Key (Author.t ~accounts:[ `GitHub "raphael-proust" ; `Email "raphael.proust@cl.cam.ac.uk" ; `Email "raphlalou@gmail.com" ] Uint.zero "raphael-proust") ;
    `Key (Author.t ~accounts:[ `GitHub "madroach" ; `Email "madroach@gmerlin.de" ; `Email "christopher@gmerlin.de" ] Uint.zero "madroach") ;
    `Key (Author.t ~accounts:[ `GitHub "testcocoon" ; `Email "sebastien.fricker@gmail.com" ; `Email "fricker@froglogic.com" ] Uint.zero "testcocoon") ;
    `Key (Author.t ~accounts:[ `GitHub "protz" ; `Email "protz@microsoft.com" ; `Email "jonathan.protzenko@inria.fr" ; `Email "jonathan.protzenko@gmail.com" ] Uint.zero "protz") ;
    `Key (Author.t ~accounts:[ `GitHub "AngryLawyer" ; `Email "tony@angry-lawyer.com" ; `Email "tony@dabapps.com" ] Uint.zero "AngryLawyer") ;
    `Key (Author.t ~accounts:[ `GitHub "rixed" ; `Email "rixed-opam@happyleptic.org" ; `Email "rixed@happyleptic.org" ] Uint.zero "rixed") ;
    `Key (Author.t ~accounts:[ `GitHub "backtracking" ; `Email "filliatr@lri.fr" ; `Email "Jean-Christophe.Filliatre@lri.fr" ] Uint.zero "backtracking") ;
    `Key (Author.t ~accounts:[ `GitHub "andrenth" ; `Email "andre@digirati.com.br" ; `Email "andrenth@gmail.com" ] Uint.zero "andrenth") ;
    `Key (Author.t ~accounts:[ `GitHub "mathiasbourgoin" ; `Email "mathias.bourgoin@gmail.com" ; `Email "mathias.bourgoin@lip6.fr" ] Uint.zero "mathiasbourgoin") ;
    `Key (Author.t ~accounts:[ `GitHub "pmundkur" ; `Email "prashanth.mundkur@gmail.com" ; `Email "pmundkur.ocaml@gmail.com" ] Uint.zero "pmundkur") ;
    `Key (Author.t ~accounts:[ `GitHub "pmeunier" ; `Email "pe@dhcp-135-214.caltech.edu" ; `Email "pe@patoline.org" ] Uint.zero "pmeunier") ;
    `Key (Author.t ~accounts:[ `GitHub "eatonphil" ; `Email "philip@Philips-MacBook-Pro.local" ; `Email "me@eatonphil.com" ] Uint.zero "eatonphil") ;
    `Team ([ "opensource@janestreet.com" ], "janestreet") ;
    `Team ([ "xen-api@lists.xen.org" ], "xapi-project") ;
    `Team ([ "contact@ocamlpro.com" ], "OCamlPro") ;
    `Team ([ "dev@ocsigen.org" ], "ocsigen") ;
    `Team ([ "alt-ergo@ocamlpro.com" ], "alt-ergo") ;
    `Team ([ "mirageos-devel@lists.openmirage.org" ; "mirageos-devel@lists.xenproject.org" ; "mirageos-devel" ], "mirage")
  ]

let to_cmd = function
  | Ok () -> `Ok ()
  | Error e -> `Error (false, e)

let infer_maintainers base lvl =
  to_cmd
    (Logs.set_level lvl ;
     Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ()) ;
     Conex_unix_provider.fs_provider base >>= fun io ->
     PR.handle_prs base PR.github_mail (M.empty, M.empty) >>= fun (github_mail, _mail_github) ->
     Logs.info (fun m -> m "github-email %a" pp_map github_mail) ;
     M.fold (fun k v acc ->
         acc >>= fun acc ->
         let accounts = `GitHub k :: List.map (fun e -> `Email e) (S.elements v) in
         let idx = Author.t ~accounts Uint.zero k in
         Conex_io.write_author io idx >>= fun () ->
         Ok (idx :: acc))
       github_mail (Ok []) >>= fun idxs ->
     (List.fold_left (fun acc id ->
          acc >>= fun acc ->
          match id with
          | `Team (mails, id) ->
            let t = Team.t Uint.zero id in
            Conex_io.write_team io t >>= fun () ->
            let idx =
              let accounts = `GitHub id :: List.map (fun e -> `Email e) mails in
              Author.t ~accounts Uint.zero id
            in
            Ok (idx :: acc)
          | `Key k ->
            Conex_io.write_author io k >>= fun () ->
            Ok (k :: acc))
         (Ok []) known_ids) >>= fun more_idxs ->

     let find_by_mail email =
       List.fold_left (fun r k ->
           match r with
           | None ->
             let contains =
               let f = function `Email e when e = email -> true | _ -> false in
               List.exists f k.Author.accounts
             in
             if contains then Some k.Author.name else None
           | Some x -> Some x)
         None (idxs @ more_idxs)
     in
     (* this is a set of package name without maintainer,
        and a map pkgname -> email address (or name) set *)
     let mapping = OpamMaintainer.infer io in
     (M.fold (fun k ids acc ->
          acc >>= fun acc ->
          let authorised = S.fold (fun s acc ->
              if String.is_infix ~affix:"@" s then
                match find_by_mail s with
                | None -> acc
                | Some x -> S.add x acc
              else
                S.add s acc)
              ids S.empty
          in
          let authorisation = Authorisation.t ~authorised Uint.zero k in
          Logs.info (fun m -> m "%a" Authorisation.pp authorisation) ;
          Conex_io.write_authorisation io authorisation >>= fun () ->
          Ok (M.add k authorisation acc))
         mapping (Ok M.empty)) >>= fun authorisations ->
     (* authorisation contains pkgname -> github id
        mapping contains pkgname -> mail address
        if a team is authorised, maybe add committer to team?
     *)
     (* NOW! we can go through all the diffs of the PRs and look around a bit:
        - filter out maintainers for commits
        - add maintainer mail address to id/ subdir if sensible *)
     Logs.app (fun m -> m "MARK") ;
     PR.handle_prs base (PR.check authorisations) (M.empty, M.empty, M.empty) >>= fun (empty, _violation, team) ->
     Logs.app (fun m -> m "potentially %a" pp_map empty) ;
     Logs.app (fun m -> m "teams %a" pp_map team) ;
     M.iter (fun name members ->
         let t = Team.t ~members Uint.zero name in
         match Conex_io.write_team io t with
         | Ok () -> ()
         | Error e -> Logs.err (fun m -> m "error %s while writing team %s" e name))
       team ;
     M.iter (fun p authorised ->
         if S.cardinal authorised = 1 then
           let a = Authorisation.t ~authorised Uint.zero p in
           match Conex_io.write_authorisation io a with
           | Ok () -> ()
           | Error e -> Logs.err (fun m -> m "error %s while writing authorisation for %s" e p))
       empty ;
     Ok ())

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
  Term.(ret (const infer_maintainers $ repo $ Logs_cli.level ())),
  Term.info "maintainer" ~version:"%%VERSION_NUM%%" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
