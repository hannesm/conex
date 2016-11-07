(* XXX: revise debug: errors should be reported for failed command, but not too many errors *)
(* XXX: packages as a useful show/verify/sign target *)
(* XXX: commands to dynamically add/remove members to authorisation and teams *)
(* XXX: key rollover and key revocation *)

open Conex_core
open Conex_resource

module Color = struct
  let endc = "\027[m"
  let red = "\027[31m"
  let green = "\027[32m"

  let res_c v = match v with | Ok _ -> green | Error _ -> red
end

type copts = {
  debug : bool ;
  dry : bool ;
  repo : Repository.t ;
  out : Format.formatter ;
  name : string option ;
  signed_by : string option ;
  owner : string option ;
  private_key : string option ;
  trust_anchors : string option ;
  verify : bool ;
}

let help _copts man_format cmds = function
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some _ -> List.iter print_endline cmds; `Ok ()

let kinds =
  [ ("privatekeys", `Privates) ;
    ("keys", `Keys) ;
    ("ids", `Ids) ;
    ("teams", `Teams) ;
    ("authorisations", `Authorisations) ;
    ("releases", `Releases) ;
    ("checksums", `Checksums) ;
    ("repository", `Repository) ]

let find_private_keys copts =
  let keys = List.sort String.compare (Private.all_private_keys copts.repo) in
  let keys = Utils.option keys (fun _ -> []) copts.signed_by in
  Utils.option keys (fun s -> List.filter (id_equal s) keys) copts.owner

let load_tas copts =
  match copts.trust_anchors with
  | None ->
    Format.fprintf copts.out "WARNING: maybe you forgot --trust-anchors?@." ;
    copts
  | Some s ->
    let repo = Conex_common.load_anchors_janitors copts.repo copts.out copts.debug (fun () -> ()) s in
    { copts with repo ; verify = true }

let find_keys copts =
  let keys =
    Utils.filter_map
      ~f:(fun f ->
          match Repository.read_key copts.repo f with
          | Error e -> if copts.debug then Format.fprintf copts.out "%skey %a%s@." Color.red Repository.pp_r_err e Color.endc ; None
          | Ok x -> Some x)
      (List.sort String.compare (S.elements (Repository.all_ids copts.repo)))
  in
  match copts.owner, copts.signed_by with
  | Some o, _ -> List.filter (fun k -> id_equal k.Publickey.keyid o) keys
  | None, Some s ->
    let is k =
      match Repository.valid copts.repo (Conex_nocrypto.digest (Data.publickey_to_string k)) with
      | None -> false
      | Some (_, _, sigs) -> S.mem s sigs
    in
    List.filter is keys
  | _ -> keys

let find_teams copts =
  let teams =
    Utils.filter_map
      ~f:(fun f ->
          match Repository.read_team copts.repo f with
          | Error e -> if copts.debug then Format.fprintf copts.out "%steam %a%s@." Color.red Repository.pp_r_err e Color.endc ; None
          | Ok x -> Some x)
      (List.sort String.compare (S.elements (Repository.all_ids copts.repo)))
  in
  match copts.owner, copts.signed_by with
  | Some o, _ -> List.filter (fun t -> S.mem o t.Team.members) teams
  | None, Some s ->
    let is k =
      match Repository.valid copts.repo (Conex_nocrypto.digest (Data.team_to_string k)) with
      | None -> false
      | Some (_, _, sigs) -> S.mem s sigs
    in
    List.filter is teams
  | _ -> teams

let find_ids copts =
  let ids =
    Utils.filter_map
      ~f:(fun f ->
          match Repository.read_id copts.repo f with
          | Error e -> Format.fprintf copts.out "%sid %a%s@." Color.red Repository.pp_r_err e Color.endc ; None
          | Ok x -> Some x)
      (List.sort String.compare (S.elements (Repository.all_ids copts.repo)))
  in
  match copts.owner, copts.signed_by with
  | Some o, _ ->
    List.filter (function
        | `Key k -> id_equal k.Publickey.keyid o
        | `Team t -> S.mem o t.Team.members)
      ids
  | None, Some s ->
    let is i =
      let raw = match i with
        | `Key k -> Data.publickey_to_string k
        | `Team t -> Data.team_to_string t
      in
      match Repository.valid copts.repo (Conex_nocrypto.digest raw) with
      | None -> false
      | Some (_, _, sigs) -> S.mem s sigs
    in
    List.filter is ids
  | _ -> ids

let find_authorisations copts =
  let auths =
    Utils.filter_map
      ~f:(fun f ->
          match Repository.read_authorisation copts.repo f with
          | Error e -> Format.fprintf copts.out "%sauthorisation %a%s@." Color.red Repository.pp_r_err e Color.endc ; None
          | Ok x -> Some x)
      (List.sort String.compare (S.elements (Repository.all_authorisations copts.repo)))
  in
  match copts.owner, copts.signed_by with
  | Some o, _ -> List.filter (fun d -> S.mem o d.Authorisation.authorised) auths
  | None, Some s ->
    let is a = match Repository.valid copts.repo (Conex_nocrypto.digest (Data.authorisation_to_string a)) with
      | None -> false
      | Some (_, _, sigs) -> S.mem s sigs
    in
    List.filter is auths
  | None, None -> auths

let find_releases copts =
  (* won't capture those dirs with releases but no authorisation!!! *)
  let auths =
    let auths = find_authorisations { copts with signed_by = None } in
    Utils.option
      auths
      (fun o -> List.filter (fun a -> a.Authorisation.name = o) auths)
      copts.name
  in
  let releases =
    Utils.filter_map
      ~f:(fun a ->
          match Repository.read_releases copts.repo a.Authorisation.name with
          | Error e -> Format.fprintf copts.out "%sreleases %a%s@." Color.red Repository.pp_r_err e Color.endc ; None
          | Ok x -> Some (a, x))
      auths
  in
  match copts.owner, copts.signed_by with
  | Some o, _ -> List.filter (fun (a, _) -> S.mem o a.Authorisation.authorised) releases
  | None, Some s ->
    let is (_, r) = match Repository.valid copts.repo (Conex_nocrypto.digest (Data.releases_to_string r)) with
      | None -> false
      | Some (_, _, sigs) -> S.mem s sigs
    in
    List.filter is releases
  | None, None -> releases

let find_checksums copts =
  let rel_auths =
    let rel_auths = find_releases { copts with signed_by = None } in
    Utils.option
      rel_auths
      (fun o -> List.filter (fun (a, _) -> a.Authorisation.name = o) rel_auths)
      copts.name
  in
  List.fold_left
    (fun acc (a, r) ->
     let cs =
       let items = Layout.items (Repository.provider copts.repo) a.Authorisation.name in
       let all = List.sort String.compare items in
       let all_cs =
         Utils.filter_map
           ~f:(fun f ->
               match Repository.read_checksum copts.repo f with
               | Error e -> Format.fprintf copts.out "%schecksum %a%s@." Color.red Repository.pp_r_err e Color.endc ; None
               | Ok x -> Some x)
           all
       in
       Utils.option
         all_cs
         (fun s ->
            let is c = match Repository.valid copts.repo (Conex_nocrypto.digest (Data.checksums_to_string c)) with
              | None -> false
              | Some (_, _, sigs) -> S.mem s sigs
            in
            List.filter is all_cs)
         copts.signed_by
     in
     match cs with
     | [] -> acc
     | xs -> (a, r, xs) :: acc)
    []
    rel_auths

let load_index id r =
  match Repository.read_index r id with
  | Error _ -> r
  | Ok i -> Repository.add_index r i

let list copts kind =
  let repo = S.fold load_index (Repository.all_ids copts.repo) copts.repo in
  let copts = { copts with repo } in
  let rec exec kind =
    let out items =
      List.iter (fun s -> Format.fprintf copts.out "%s@." s) items
    in
    let _, kinds_pp = Cmdliner.Arg.enum kinds in
    Format.fprintf copts.out "listing %a" kinds_pp kind ;
    match kind with
    | `Privates ->
       let keys = find_private_keys copts in
       Format.fprintf copts.out " (%d)@." (List.length keys) ;
       out keys
    | `Keys ->
       let keys = find_keys copts in
       Format.fprintf copts.out " (%d)@." (List.length keys) ;
       out (List.map (fun k -> k.Publickey.keyid) keys)
    | `Teams ->
       let teams = find_teams copts in
       Format.fprintf copts.out " (%d)@." (List.length teams) ;
       out (List.map (fun t -> t.Team.name) teams)
    | `Ids ->
       let ids = find_ids copts in
       Format.fprintf copts.out " (%d)@." (List.length ids) ;
       out (List.map (function `Key k -> k.Publickey.keyid | `Team t -> t.Team.name) ids)
    | `Authorisations ->
       let auths = find_authorisations copts in
       Format.fprintf copts.out " (%d)@." (List.length auths) ;
       out (List.map (fun a -> a.Authorisation.name) auths)
    | `Releases ->
       let rels = find_releases copts in
       Format.fprintf copts.out " (%d)@." (List.length rels) ;
       out (List.map (fun (_, r) -> r.Releases.name) rels)
    | `Checksums ->
       let d_cs = find_checksums copts in
       Format.fprintf copts.out " (%d)@." (List.length d_cs) ;
       List.iter
         (fun (a, _, cs) ->
          Format.fprintf copts.out "%s (%d authorised: %a)@."
            a.Authorisation.name (List.length cs) Authorisation.pp_authorised a.Authorisation.authorised ;
          out (List.map (fun c -> c.Checksum.name) cs))
         d_cs
    | `Repository ->
       let provider = Repository.provider copts.repo in
       Format.pp_print_newline copts.out () ;
       Provider.pp_provider copts.out provider ;
       List.iter exec (List.filter (function `Repository -> false | `Ids -> false | _ -> true) (snd (List.split kinds)))
  in
  exec kind ; `Ok ()

let verify copts kind =
  let copts = load_tas copts in
  let repo = S.fold
      (Conex_common.load_id copts.out copts.debug (fun () -> ()))
      (S.diff (Repository.all_ids copts.repo) (Repository.team copts.repo "janitors"))
      copts.repo
  in
  let copts = { copts with repo } in
  let rec exec repo kind =
    let out items verified =
      List.iter2
        (fun s v ->
           let c = Color.res_c v in
           match v with
           | Ok ok -> Format.fprintf copts.out "%s%s %a%s@." c s Repository.pp_ok ok Color.endc
           | Error err -> Format.fprintf copts.out "%s%s %a%s@." c s Repository.pp_error err Color.endc)
        items verified
    in
    let _, kinds_pp = Cmdliner.Arg.enum kinds in
    Format.fprintf copts.out "verifying %a" kinds_pp kind ;
    match kind with
    | `Privates ->
       let keys = find_private_keys copts in
       Format.fprintf copts.out " (%d)@." (List.length keys) ;
       List.iter (Format.fprintf copts.out "%s@.") keys ;
       repo
    | `Keys ->
       let keys = find_keys copts in
       let verified = List.map (Repository.verify_key repo) keys in
       Format.fprintf copts.out " (%d)@." (List.length verified) ;
       out (List.map (fun k -> k.Publickey.keyid) keys) verified ;
       repo
    | `Teams ->
       let teams = find_teams copts in
       let verified = List.map (Repository.verify_team repo) teams in
       Format.fprintf copts.out " (%d)@." (List.length verified) ;
       out (List.map (fun t -> t.Team.name) teams) verified ;
       repo
    | `Ids ->
       let keys = find_keys copts in
       let kverified = List.map (Repository.verify_key repo) keys in
       let teams = find_teams copts in
       let tverified = List.map (Repository.verify_team repo) teams in
       Format.fprintf copts.out " (%d keys, %d teams)@." (List.length kverified) (List.length tverified) ;
       out (List.map (fun k -> k.Publickey.keyid) keys) kverified ;
       out (List.map (fun t -> t.Team.name) teams) tverified ;
       repo
    | `Authorisations ->
       let auths = find_authorisations copts in
       let verified = List.map (Repository.verify_authorisation repo) auths in
       Format.fprintf copts.out " (%d)@." (List.length auths) ;
       out (List.map (fun d -> d.Authorisation.name) auths) verified ;
       repo
    | `Releases ->
       let rels = find_releases copts in
       let verified = List.map (fun (a, r) -> Repository.verify_releases repo a r) rels in
       Format.fprintf copts.out " (%d)@." (List.length rels) ;
       out (List.map (fun (_, r) -> r.Releases.name) rels) verified ;
       repo
    | `Checksums ->
       let d_cs = find_checksums copts in
       Format.fprintf copts.out " (%d)@." (List.length d_cs) ;
       List.iter
         (fun (a, r, cs) ->
            let a_verified = Repository.verify_authorisation repo a in
            let c = Color.res_c a_verified in
            (match a_verified with
             | Ok ok ->
               Format.fprintf copts.out "%s%s (%d authorised: %a) (%a)%s@."
                 c a.Authorisation.name (List.length cs)
                 Authorisation.pp_authorised a.Authorisation.authorised
                 Repository.pp_ok ok Color.endc
             | Error err ->
               Format.fprintf copts.out "%s%s (%d authorised: %a) (%a)%s@."
                 c a.Authorisation.name (List.length cs)
                 Authorisation.pp_authorised a.Authorisation.authorised
                 Repository.pp_error err Color.endc) ;
            let verified = List.map (Repository.verify_checksum repo a r) cs in
            List.iter2
              (fun s v ->
                 let col = Color.res_c v in
                 match v with
                 | Ok ok ->
                   Format.fprintf copts.out "%s%s, %a%s@."
                     col s.Checksum.name Repository.pp_ok ok Color.endc
                 | Error err ->
                   Format.fprintf copts.out "%s%s, %a%s@."
                     col s.Checksum.name Repository.pp_error err Color.endc)
              cs verified)
         d_cs ;
       repo
    | `Repository ->
       let provider = Repository.provider copts.repo in
       Format.pp_print_newline copts.out () ;
       Provider.pp_provider copts.out provider ;
       List.fold_left exec repo (List.filter (function `Repository -> false | `Ids -> false | _ -> true) (snd (List.split kinds)))
  in
  let _ = exec copts.repo kind in
  `Ok ()

(* XXX id, index *)
let items = [
  ("key", `Key) ;
  ("team", `Team) ;
  ("private", `Private) ;
  ("authorisation", `Authorisation) ;
  ("releases", `Releases) ;
  ("checksum", `Checksum) ;
  ("diff", `Diff)
]

let verified copts b =
  let c = Color.res_c b in
  match b with
  | Ok ok -> Format.fprintf copts.out "%s%a%s@." c Repository.pp_ok ok Color.endc
  | Error err -> Format.fprintf copts.out "%s%a%s@." c Repository.pp_error err Color.endc

let show_key copts key =
  Publickey.pp_publickey copts.out key ;
  verified copts (Repository.verify_key copts.repo key) ;
  `Ok ()

let show_team copts team =
  Team.pp_team copts.out team ;
  verified copts (Repository.verify_team copts.repo team) ;
  `Ok ()

let show_private copts (id, priv) =
  let raw = match priv with `Priv p -> p in
  Format.fprintf copts.out "keyid: %s,@ %s@." id raw ;
  `Ok ()

let show_authorisation copts a =
  Authorisation.pp_authorisation copts.out a ;
  verified copts (Repository.verify_authorisation copts.repo a) ;
  `Ok ()

let show_releases copts r =
  Releases.pp_releases copts.out r ;
  (match Repository.read_authorisation copts.repo r.Releases.name with
   | Error e -> Format.fprintf copts.out "%sauthorisation %a%s@ " Color.red Repository.pp_r_err e Color.endc
   | Ok a ->
     verified copts (Repository.verify_releases copts.repo a r)) ;
  `Ok ()

let show_checksum copts c =
  Checksum.pp_checksums copts.out c ;
  (match Layout.authorisation_of_item c.Checksum.name with
   | Some name ->
     (match Repository.read_authorisation copts.repo name with
      | Error e -> Format.fprintf copts.out "%sauthorisation %a%s@ " Color.red Repository.pp_r_err e Color.endc
      | Ok a ->
        match Repository.read_releases copts.repo name with
        | Error e -> Format.fprintf copts.out "%sreleases %a%s@ " Color.red Repository.pp_r_err e Color.endc
        | Ok r ->
          Format.fprintf copts.out "authorised: %a@ " Authorisation.pp_authorised a.Authorisation.authorised ;
          verified copts (Repository.verify_checksum copts.repo a r c)) ;
     (match Repository.compute_checksum copts.repo c.Checksum.name with
      | Error e -> Format.fprintf copts.out "%schecksum %a%s@ " Color.red Repository.pp_error e Color.endc
      | Ok computed ->
        match Checksum.compare_checksums computed c with
        | Ok () ->
          Format.fprintf copts.out "%schecksum %s%s@."
            Color.green "valid" Color.endc
        | Error e ->
          Format.fprintf copts.out "%schecksum %s%s@."
            Color.red "invalid" Color.endc ;
          Repository.pp_error copts.out e)
   | None -> Format.fprintf copts.out "couldn't figure out package name for %s@." c.Checksum.name) ;
  `Ok ()

let show copts item value =
  let copts = load_tas copts in
  let repo = S.fold
      (Conex_common.load_id copts.out copts.debug (fun () -> ()))
      (S.diff (Repository.all_ids copts.repo) (Repository.team copts.repo "janitors"))
      copts.repo
  in
  let copts = { copts with repo } in
  match item with
  | `Key -> (match Repository.read_key copts.repo value with
      | Ok k -> show_key copts k
      | Error e ->
        Format.fprintf copts.out "%skey %a%s@." Color.red Repository.pp_r_err e Color.endc ;
        `Error (false, "error"))
  | `Team -> (match Repository.read_team copts.repo value with
      | Ok t -> show_team copts t
      | Error e ->
        Format.fprintf copts.out "%steam %a%s@." Color.red Repository.pp_r_err e Color.endc ;
        `Error (false, "error"))
  | `Private -> (match Private.read_private_key ~id:value copts.repo with
      | Ok k -> show_private copts k
      | Error e ->
        Format.fprintf copts.out "%s%a%s@." Color.red Private.pp_err e Color.endc ;
        `Error (false, "error"))
  | `Authorisation -> (match Repository.read_authorisation copts.repo value with
      | Ok k -> show_authorisation copts k
      | Error e ->
        Format.fprintf copts.out "%sauthorisation %a%s" Color.red Repository.pp_r_err e Color.endc ;
        `Error (false, "error"))
  | `Releases -> (match Repository.read_releases copts.repo value with
      | Ok k -> show_releases copts k
      | Error e ->
        Format.fprintf copts.out "%sreleases %a%s" Color.red Repository.pp_r_err e Color.endc ;
        `Error (false, "error"))
  | `Checksum -> (match Repository.read_checksum copts.repo value with
      | Ok k -> show_checksum copts k
      | Error e ->
        Format.fprintf copts.out "%schecksum %a%s" Color.red Repository.pp_r_err e Color.endc ;
        `Error (false, "error"))
  | `Diff -> if Persistency.exists value then
               let data = Persistency.read_file value in
               let provider = Repository.provider copts.repo in
               Format.fprintf copts.out "verifying %s (%a)@.%s@."
                              value Provider.pp_provider provider data ;
(*               (match Patch.verify_diff copts.repo data with
                | `Ok _ -> Format.fprintf copts.out "Diff %s successfully verified@." value
                 | `Error s -> Format.fprintf copts.out "Failed to verify diff %s: %s@." value s) ; *)
               `Ok ()
             else
               `Error (false, "diff " ^ value ^ " not found")

let generate copts item name ids =
  match item with
  | `Private ->
     Nocrypto_entropy_unix.initialize () ;
     let priv = Conex_nocrypto.generate () in
     if copts.dry then
       Format.fprintf copts.out "dry run, nothing written.@."
     else
       Private.write_private_key copts.repo name priv ;
     Format.fprintf copts.out "generated new private key for %s@." name ;
     show_private copts (name, priv)
  | `Key ->
    let writeout p =
      if copts.dry then
        Format.fprintf copts.out "dry run, nothing written.@."
      else
        Repository.write_key copts.repo p ;
      Format.fprintf copts.out "generated public key for %s@." name ;
      show_key copts p
    in
    (match Repository.read_key copts.repo name with
     | Ok p ->
       let pub_opt =
         match Private.read_private_key ~id:name copts.repo with
         | Error _ -> None
         | Ok (_, priv) -> match Conex_nocrypto.pub_of_priv priv with
           | Ok x -> Some x
           | Error _ -> None
       in
       writeout
         (Publickey.publickey
            ~counter:(Int64.succ p.Publickey.counter)
            name pub_opt)
     | Error _ ->
       (* XXX need: if name = "" then missing argument! *)
       match Private.read_private_key ~id:name copts.repo with
       | Error _ -> Format.fprintf copts.out "%seither need a public or a private key for %s%s@."
                      Color.red name Color.endc ;
         `Error (false, "no private key and public does not exist")
       | Ok (_, k) -> match Conex_nocrypto.pub_of_priv k with
         | Error e ->
           Format.fprintf copts.out "%scouldn't decode private key for %s: %s%s@."
             Color.red name e Color.endc ;
           `Error (false, "decoding of private key failed")
         | Ok p -> writeout (Publickey.publickey name (Some p)))
  | `Team ->
    let counter = match Repository.read_team copts.repo name with
      | Error _ -> None
      | Ok t' -> Some (Int64.succ t'.Team.counter)
    in
    let t = Team.team ~members:(S.of_list ids) ?counter name in
    if copts.dry then
      Format.fprintf copts.out "dry run, nothing written.@."
    else
      Repository.write_team copts.repo t ;
    Format.fprintf copts.out "updated team %s@." name ;
    show_team copts t
  | `Authorisation ->
    let counter = match Repository.read_authorisation copts.repo name with
      | Error _ -> None
      | Ok a' -> Some (Int64.succ a'.Authorisation.counter)
    in
    let a = Authorisation.authorisation ~authorised:(S.of_list ids) ?counter name in
    if copts.dry then
      Format.fprintf copts.out "dry run, nothing written.@."
    else
      Repository.write_authorisation copts.repo a ;
    Format.fprintf copts.out "updated authorisation %s@." name ;
    show_authorisation copts a
  | `Releases ->
    let counter = match Repository.read_releases copts.repo name with
      | Error _ -> None
      | Ok r' -> Some (Int64.succ r'.Releases.counter)
    in
    (match Releases.releases ~releases:(S.of_list ids) ?counter name with
     | Ok r ->
       if copts.dry then
         Format.fprintf copts.out "dry run, nothing written.@."
       else
         Repository.write_releases copts.repo r ;
       Format.fprintf copts.out "updated releases %s@." name ;
       show_releases copts r
     | Error r ->
       Format.fprintf copts.out "%sfailed to create releases: %s%s.@." Color.red r Color.endc ;
       `Error (false, "while creating releases"))
  | `Checksum ->
    (match Repository.compute_checksum copts.repo name with
     | Error e ->
       Format.fprintf copts.out "%scomputing checksum %a%s@."
         Color.red Repository.pp_error e Color.endc ;
       `Error (false, "while computing checksum")
     | Ok cs ->
       let cs = match Repository.read_checksum copts.repo name with
         | Error _ -> cs
         | Ok o -> Checksum.set_counter cs (Int64.succ o.Checksum.counter)
       in
       if copts.dry then
         Format.fprintf copts.out "dry run, nothing written.@."
       else
         Repository.write_checksum copts.repo cs ;
       Format.fprintf copts.out "updated checksum %s@." name ;
       show_checksum copts cs)
  | _ -> `Error (false, "dunno what to generate")

let sign copts item name =
  Nocrypto_entropy_unix.initialize () ;
  match Private.read_private_key ?id:copts.private_key copts.repo with
  | Error e ->
    Format.fprintf copts.out "%s%a%s@." Color.red Private.pp_err e Color.endc ;
    `Error (false, "error")
  | Ok (id, p) ->
    Format.fprintf copts.out "using private key %s@." id ;
    let idx = match Repository.read_index copts.repo id with
      | Error e -> Repository.pp_r_err copts.out e ; Index.index id
      | Ok idx -> idx
    in
    let add_r r = Private.sign_index (Index.add_resource idx r) p in
    match item with
    | `Key -> (match Repository.read_key copts.repo name with
        | Error e ->
          Format.fprintf copts.out "%skey %a%s@." Color.red Repository.pp_r_err e Color.endc ;
          `Error (false, "error")
        | Ok k ->
          match add_r (name, `PublicKey, Conex_nocrypto.digest (Data.publickey_to_string k)) with
          | Ok idx ->
            if copts.dry then
              Format.fprintf copts.out "dry run, nothing written.@."
            else
              Repository.write_index copts.repo idx ;
            Format.fprintf copts.out "signed key %s@." name ;
            let copts = { copts with repo = Repository.add_index copts.repo idx } in
            show_key copts k
          | Error e ->
            Format.fprintf copts.out "%s%s%s@." Color.red e Color.endc ;
            `Error (false, "error"))

    | `Team -> (match Repository.read_team copts.repo name with
        | Error e ->
          Format.fprintf copts.out "%steam %a%s@." Color.red Repository.pp_r_err e Color.endc ;
          `Error (false, "error")
        | Ok t ->
          match add_r (name, `Team, Conex_nocrypto.digest (Data.team_to_string t)) with
          | Ok idx ->
            if copts.dry then
              Format.fprintf copts.out "dry run, nothing written.@."
            else
              Repository.write_index copts.repo idx ;
            Format.fprintf copts.out "signed team %s@." name ;
            let copts = { copts with repo = Repository.add_index copts.repo idx } in
            show_team copts t
          | Error e ->
            Format.fprintf copts.out "%s%s%s@." Color.red e Color.endc ;
            `Error (false, "error"))

    | `Authorisation -> (match Repository.read_authorisation copts.repo name with
        | Error e ->
          Format.fprintf copts.out "%sauthorisation %a%s@." Color.red Repository.pp_r_err e Color.endc ;
          `Error (false, "error")
        | Ok a ->
          match add_r (name, `Authorisation, Conex_nocrypto.digest (Data.authorisation_to_string a)) with
          | Ok idx ->
            if copts.dry then
              Format.fprintf copts.out "dry run, nothing written.@."
            else
              Repository.write_index copts.repo idx ;
            Format.fprintf copts.out "signed authorisation %s@." name ;
            let copts = { copts with repo = Repository.add_index copts.repo idx } in
            show_authorisation copts a
          | Error e ->
            Format.fprintf copts.out "%s%s%s@." Color.red e Color.endc ;
            `Error (false, "error"))

    | `Releases -> (match Repository.read_releases copts.repo name with
        | Error e ->
          Format.fprintf copts.out "%sreleases %a%s@." Color.red Repository.pp_r_err e Color.endc ;
          `Error (false, "error")
        | Ok r ->
          match add_r (name, `Releases, Conex_nocrypto.digest (Data.releases_to_string r)) with
          | Ok idx ->
            if copts.dry then
              Format.fprintf copts.out "dry run, nothing written.@."
            else
              Repository.write_index copts.repo idx ;
            Format.fprintf copts.out "signed releases %s@." name ;
            let copts = { copts with repo = Repository.add_index copts.repo idx } in
            show_releases copts r
          | Error e ->
            Format.fprintf copts.out "%s%s%s@." Color.red e Color.endc ;
            `Error (false, "error"))

    | `Checksum -> (match Repository.read_checksum copts.repo name with
        | Error e ->
          Format.fprintf copts.out "%schecksum %a%s@." Color.red Repository.pp_r_err e Color.endc ;
          `Error (false, "error")
        | Ok c ->
          match add_r (name, `Checksum, Conex_nocrypto.digest (Data.checksums_to_string c)) with
          | Ok idx ->
            if copts.dry then
              Format.fprintf copts.out "dry run, nothing written.@."
            else
              Repository.write_index copts.repo idx ;
            Format.fprintf copts.out "signed checksum %s@." name ;
            let copts = { copts with repo = Repository.add_index copts.repo idx } in
            show_checksum copts c
          | Error e ->
            Format.fprintf copts.out "%s%s%s@." Color.red e Color.endc ;
            `Error (false, "error"))

    | _ -> `Error (false, "dunno what to sign")


open Cmdliner

let copts_sect = "COMMON OPTIONS"
let help_secs = [
 `S "GENERAL";
 `P "Conex provides a signed repository, containing data as well as public keys and ownership information.  The main idea is that each directory is owned by a set of keys (which can contract and expand dynamically).  An initial set of public keys, so called trust anchors, should be distributed out of band.  A set of maintainers can do janitor work, cleaning up and adjusting all directories, if needed.  These need to coordinate, there need to be either a signature by the owner, or a quorum of maintainer signatures on ownership and data information.";
 `P "If starting with a fresh repository, you likely want to generate a private and public key, then setup some authorisations and distribute some data.";
 `S "RESOURCE";
 `P "Conex distinguishes six different resources, which you can identify by name: private key, public key, authorisation, checksum, index, and diff.  Private keys are stored in ~/.conex, all others are stored in the repository.  A diff is just a file containing a unified diff, which can be verified (if it applies cleanly and has valid signatures).";
 `S "KEYS";
 `P "Public keys contain a unique identifier.  They are distributed with the repository itself.  You can filter items by the owner (a key identifier) and by the key identifier which signed items.";
 `P "Your private keys are stored PEM-encoded in ~/.conex.  You can select which key to use for a signing operation by passing -k to conex.";
 `S copts_sect;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `S "BUGS"; `P "Check bug reports at https://github.com/hannesm/conex.";]

let copts debug dry repo name signed_by owner private_key trust_anchors quorum =
  let repo =
    let provider = if dry then Provider.fs_ro_provider repo else Provider.fs_provider repo in
    Repository.repository ?quorum provider
  in
  { debug ; dry ; repo ; out = Format.std_formatter ; name ; signed_by ; owner ; private_key ; trust_anchors ; verify = false }

let copts_t =
  let docs = copts_sect in
  let dry =
    let doc = "Try run. Do not write anything." in
    Arg.(value & flag & info ["dry-run"] ~docs ~doc)
  and debug =
    let doc = "Print debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  and repo =
    let doc = "Repository base directory" in
    Arg.(value & opt dir "/tmp/conex" & info [ "r" ; "repository" ] ~docs ~docv:"DIR" ~doc)
  and nam =
    let doc = "filter on the name of an resource, depending on context." in
    Arg.(value & opt (some string) None & info ["name";"n"] ~docs ~docv:"RESOURCE" ~doc)
  and signed_by =
    let doc = "filter for a specific key identifier which signed the resource." in
    Arg.(value & opt (some string) None & info ["signed-by";"by";"s"] ~docs ~docv:"KEYS" ~doc)
  and owner =
    let doc = "filter for a key identifier which owns the resource." in
    Arg.(value & opt (some string) None & info ["owner";"o"] ~docs ~docv:"KEYS" ~doc)
  and private_key =
    let doc = "use specific private key for signing." in
    Arg.(value & opt (some string) None & info ["private-key"; "k"] ~docs ~docv:"KEYS" ~doc)
  and tas =
    let doc = "use directory as initial trust anchor set (influences behaviour of show and verify)." in
    Arg.(value & opt (some string) None & info ["trust-anchors"; "t"] ~docs ~docv:"DIR" ~doc)
  and quorum =
    let doc = "use the specific value as quorum, otherwise the library default is used" in
    Arg.(value & opt (some int) None & info ["quorum"] ~docs ~docv:"NUMBER" ~doc)
  in
  Term.(const copts $ debug $ dry $ repo $ nam $ signed_by $ owner $ private_key $ tas $ quorum)

let kind =
  let conv = Arg.enum kinds in
  let doc = "The kind of resources."in
  Arg.(value & pos 0 conv `Repository & info [] ~docv:"RESOURCETYPE" ~doc)

let item =
  let conv = Arg.enum items in
  let doc = "A single resource type." in
  Arg.(value & pos 0 conv `Key & info [] ~docv:"RESOURCETYPE" ~doc)

let value =
  let doc = "name of the resource." in
  Arg.(value & pos 1 string "" & info [] ~docv:"RESOURCE" ~doc)

let show_cmd =
  let doc = "shows a single repository resource" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows a single resource in the conex repository."]
  in
  Term.(ret (const show $ copts_t $ item $ value)),
  Term.info "show" ~doc ~man

let valid =
  let doc = "Owners of this authorisation, defaults to own keyid." in
  Arg.(value & opt_all string [] & info ["id"; "i"] ~doc ~docv:"KEYS")

let generate_cmd =
  let doc = "generates a resource for the repository" in
  let man =
    [`S "DESCRIPTION";
     `P "Generates a single resource in the conex repository"]
  in
  Term.(ret (const generate $ copts_t $ item $ value $ valid)),
  Term.info "generate" ~doc ~man

(* modify_cmd = [key: team change ; authorisation: add/remove id] *)

let sign_cmd =
  let doc = "signs a resource in the repository" in
  let man =
    [`S "DESCRIPTION";
     `P "Sings a single resource in the conex repository"]
  in
  Term.(ret (const sign $ copts_t $ item $ value)),
  Term.info "sign" ~doc ~man

let list_cmd =
  let doc = "lists repository resources" in
  let man =
    [`S "DESCRIPTION";
     `P "Lists resources in the conex repository."]
  in
  Term.(ret (const list $ copts_t $ kind)),
  Term.info "list" ~doc ~man

let verify_cmd =
  let doc = "verifies repository resources" in
  let man =
    [`S "DESCRIPTION";
     `P "Verifies resources in the conex repository."]
  in
  Term.(ret (const verify $ copts_t $ kind)),
  Term.info "verify" ~doc ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about conex and conex commands" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about conex commands and other subjects..."] @ help_secs
  in
  Term.(ret
          (const help $ copts_t $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "a repository signing tool" in
  let man = help_secs in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "conex" ~version:"0.1.0" ~sdocs:copts_sect ~doc ~man

let cmds = [sign_cmd ; generate_cmd ; verify_cmd ; show_cmd ; list_cmd ; help_cmd]

let () =
  match Term.eval_choice default_cmd cmds
  with `Error _ -> exit 1 | _ -> exit 0
