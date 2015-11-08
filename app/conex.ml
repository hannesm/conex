open Core

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
    ("delegates", `Delegates) ;
    ("checksums", `Checksums) ;
    ("repository", `Repository) ]

let find_private_keys copts =
  let keys = List.sort String.compare (Private.all_private_keys copts.repo) in
  let keys = Utils.option keys (fun _ -> []) copts.signed_by in
  Utils.option keys (fun s -> List.filter ((=) s) keys) copts.owner

let has_signature_of sigs id = List.exists (fun (id', _, _) -> id' = id) sigs

let id_of_sig (id, _, _) = id

let load copts sigs =
  Repository.load_keys copts.repo ~verify:copts.verify (List.map id_of_sig sigs)

let load_tas copts =
  match copts.trust_anchors with
  | None ->
     Format.fprintf copts.out "WARNING: treating repository as trusted, maybe you forgot --trust-anchors?@." ; copts
  | Some s ->
     let keys = List.map (Filename.concat s) (Persistency.collect_dir s) in
     let keys = List.filter (Strhelper.is_suffix ~suffix:".public") keys in
     let keys =
       List.map
         (fun f -> let content = Persistency.read_file (Filename.concat s f) in
                   let data = Data.parse content in
                   Data.data_to_publickey data)
         keys
     in
     let repo = List.fold_left Repository.add_key copts.repo keys in
     let pkey k = Printf.sprintf "%s as %s" k.Publickey.keyid (role_to_string k.Publickey.role) in
     Format.fprintf copts.out "Loaded %d trust anchors [%s] from %s@."
                    (List.length keys)
                    (String.concat ", " (List.map pkey keys))
                    s ;
     { copts with repo ; verify = true }

let find_keys copts =
  let keys =
    Utils.filter_map
      ~f:(Repository.read_key copts.repo)
      (List.sort String.compare (Repository.all_keyids copts.repo))
  in
  match copts.owner, copts.signed_by with
  | Some o, _ -> List.filter (fun k -> k.Publickey.keyid = o) keys
  | None, Some s -> List.filter (fun k -> has_signature_of k.Publickey.signatures s) keys
  | _ -> keys

let find_delegates copts =
  let dels =
    Utils.filter_map
      ~f:(Repository.read_delegate copts.repo)
      (List.sort String.compare (Repository.all_delegates copts.repo))
  in
  match copts.owner, copts.signed_by with
  | Some o, _ -> List.filter (fun d -> List.mem o d.Delegate.validids) dels
  | None, Some s -> List.filter (fun d -> has_signature_of d.Delegate.signatures s) dels
  | None, None -> dels

let find_checksums copts =
  let dels =
    let ds = find_delegates { copts with signed_by = None } in
    Utils.option
      ds
      (fun o -> List.filter (fun d -> d.Delegate.name = o) ds)
      copts.name
  in
  List.fold_left
    (fun acc d ->
     let cs =
       let items = Layout.items (Repository.provider copts.repo) d.Delegate.name in
       let all = List.sort String.compare items in
       let all_cs =
         Utils.filter_map
           ~f:(Repository.read_checksum copts.repo)
           all
       in
       Utils.option
         all_cs
         (fun s -> List.filter (fun c -> has_signature_of c.Checksum.signatures s) all_cs)
         copts.signed_by
     in
     match cs with
     | [] -> acc
     | xs -> (d, xs) :: acc)
    []
    dels

let list copts kind =
  let rec exec kind =
    let out items =
      List.iter
        (fun s -> Format.pp_print_string copts.out s ; Format.pp_print_newline copts.out ())
        items
    in
    let _, kinds_pp = Cmdliner.Arg.enum kinds in
    Format.fprintf copts.out "listing %a" kinds_pp kind ;
    match kind with
    | `Privates ->
       let keys = find_private_keys copts in
       Format.fprintf copts.out " (%d)@." (List.length keys) ; out keys
    | `Keys ->
       let keys = find_keys copts in
       Format.fprintf copts.out " (%d)@." (List.length keys) ; out (List.map (fun k -> k.Publickey.keyid) keys)
    | `Delegates ->
       let dels = find_delegates copts in
       Format.fprintf copts.out " (%d)@." (List.length dels) ; out (List.map (fun d -> d.Delegate.name) dels)
    | `Checksums ->
       let d_cs = find_checksums copts in
       Format.fprintf copts.out " (%d)@." (List.length d_cs) ;
       List.iter
         (fun (d, cs) ->
          Format.fprintf copts.out "%s (%d owners: %a)@."
                         d.Delegate.name (List.length cs) Delegate.pp_owners d.Delegate.validids ;
          out (List.map (fun c -> c.Checksum.name) cs))
         d_cs
    | `Repository ->
       let provider = Repository.provider copts.repo in
       Format.pp_print_newline copts.out () ;
       Provider.pp_provider copts.out provider ;
       List.iter exec (List.filter ((<>) `Repository) (snd (List.split kinds)))
  in
  exec kind ; `Ok ()

let verify copts kind =
  let copts = load_tas copts in
  let load_k repo sigs = Repository.load_keys repo ~verify:copts.verify (List.map id_of_sig sigs) in
  let rec exec repo kind =
    let out items verified =
      List.iter2
        (fun s v ->
           let c = Color.res_c v in
           Format.fprintf copts.out "%s%s %a%s@." c s Repository.pp_res v Color.endc)
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
       let repo = Repository.load_keys repo ~verify:copts.verify (List.map (fun k -> k.Publickey.keyid) keys) in
       let verified = List.map (Repository.verify_key repo) keys in
       Format.fprintf copts.out " (%d)@." (List.length verified) ;
       out (List.map (fun k -> k.Publickey.keyid) keys) verified ;
       repo
    | `Delegates ->
       let dels = find_delegates copts in
       let repo = List.fold_left load_k repo (List.map (fun d -> d.Delegate.signatures) dels) in
       let verified = List.map (Repository.verify_delegate repo) dels in
       Format.fprintf copts.out " (%d)@." (List.length dels) ;
       out (List.map (fun d -> d.Delegate.name) dels) verified ;
       repo
    | `Checksums ->
       let d_cs = find_checksums copts in
       let repo = List.fold_left (fun repo (d, cs) -> List.fold_left load_k (load_k repo d.Delegate.signatures) (List.map (fun c -> c.Checksum.signatures) cs)) repo d_cs in
       Format.fprintf copts.out " (%d)@." (List.length d_cs) ;
       List.iter
         (fun (d, cs) ->
            let d_verified = Repository.verify_delegate repo d in
            let c = Color.res_c d_verified in
            Format.fprintf
              copts.out "%s%s (%d owners: %a) (%a)%s@."
              c d.Delegate.name (List.length cs) Delegate.pp_owners d.Delegate.validids
              Repository.pp_res d_verified Color.endc ;
            let verified =
              List.map
                (fun c ->
                   let computed = Repository.compute_checksum repo c.Checksum.name in
                   let valid = Checksum.checksums_equal computed c
                   and verified = Repository.verify_checksum repo d c
                   in
                   (verified, valid)) cs
            in
            List.iter2
              (fun s (v, c) ->
                 let col = Color.res_c v in
                 Format.fprintf
                   copts.out "%s%s, %s, %a%s@."
                   col s.Checksum.name
                   (if c then "valid checksum" else "invalid checksum")
                   Repository.pp_res v
                   Color.endc)
              cs verified)
         d_cs ;
       repo
    | `Repository ->
       let provider = Repository.provider copts.repo in
       Format.pp_print_newline copts.out () ;
       Provider.pp_provider copts.out provider ;
       List.fold_left exec repo (List.filter ((<>) `Repository) (snd (List.split kinds)))
  in
  let _ = exec copts.repo kind in
  `Ok ()

let items = [ ("key", `Key) ; ("private", `Private) ; ("delegate", `Delegate) ; ("checksum", `Checksum) ; ("diff", `Diff) ]

let verified copts b =
  let c = Color.res_c b in
  Format.fprintf copts.out "%s%a%s@." c Repository.pp_res b Color.endc

let show_key copts key =
  Publickey.pp_publickey copts.out key ;
  let repo = load copts key.Publickey.signatures in
  verified copts (Repository.verify_key repo key) ;
  `Ok ()

let show_private copts (id, priv) =
  Format.fprintf copts.out "keyid: %s,@ %a@." id Private.pp_priv priv ;
  `Ok ()

let show_delegate copts d =
  Delegate.pp_delegate copts.out d ;
  let repo = load copts d.Delegate.signatures in
  verified copts (Repository.verify_delegate repo d) ;
  `Ok ()

let show_checksum copts c =
  let repo = load copts c.Checksum.signatures in
  let valid =
    let computed = Repository.compute_checksum copts.repo c.Checksum.name in
    Checksum.checksums_equal computed c
  in
  Checksum.pp_checksums copts.out c ;
  (match Repository.read_delegate repo (Layout.delegate_of_item c.Checksum.name) with
   | None -> Format.pp_print_string copts.out "no delegate@ "
   | Some d ->
     Format.fprintf copts.out "owners: %a@ " Delegate.pp_owners d.Delegate.validids ;
     verified copts (Repository.verify_checksum repo d c)) ;
  Format.fprintf copts.out "%s@." (if valid then "checksums valid" else "checksums invalid") ;
  `Ok ()

let show copts item value =
  let copts = load_tas copts in
  match item with
  | `Key -> Utils.option
              (`Error (false, ("key " ^ value ^ " not found")))
              (show_key copts)
              (Repository.read_key copts.repo value)
  | `Private -> Utils.option
                  (`Error (false, ("private key " ^ value ^ " not found")))
                  (show_private copts)
                  (Private.read_private_key ~id:value copts.repo)
  | `Delegate -> Utils.option
                   (`Error (false, "delegate " ^ value ^ " not found"))
                   (show_delegate copts)
                   (Repository.read_delegate copts.repo value)
  | `Checksum -> Utils.option
                   (`Error (false, "checksum " ^ value ^ " not found"))
                   (show_checksum copts)
                   (Repository.read_checksum copts.repo value)
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

let generate copts item name role ids =
  match item with
  | `Private ->
     Nocrypto_entropy_unix.initialize () ;
     let priv = Private.generate () in
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
       Format.fprintf copts.out "generated new key for %s@." name ;
       show_key copts p
     in
     Utils.option
       (Utils.option
          (`Error (false, "no private key and public does not exist"))
          (fun (_, k) ->
           let pub = Publickey.publickey ~role name (Some (Private.pub_of_priv k)) in
           writeout pub)
          (Private.read_private_key ~id:name copts.repo))
       (fun p ->
        let pub = { p with Publickey.counter = Int64.succ p.Publickey.counter ; role ; signatures = [] } in
        let pub =
          Utils.option
            pub
            (fun (_, priv) -> { pub with Publickey.key = Some (Private.pub_of_priv priv) })
            (Private.read_private_key ~id:name copts.repo)
        in
        writeout pub)
       (Repository.read_key copts.repo name)
  | `Delegate ->
     let d = { Delegate.name = name ; counter = 0L ; validids = ids ; signatures = [] } in
     let d =
       Utils.option
         d
         (fun d' -> { d with Delegate.counter = Int64.succ d'.Delegate.counter })
         (Repository.read_delegate copts.repo name)
     in
     if copts.dry then
       Format.fprintf copts.out "dry run, nothing written.@."
     else
       Repository.write_delegate copts.repo d ;
     Format.fprintf copts.out "updated delegate %s@." name ;
     show_delegate copts d
  | `Checksum ->
     let cs = Repository.compute_checksum copts.repo name in
     let cs =
       Utils.option
         cs
         (fun o -> { cs with Checksum.counter = Int64.succ o.Checksum.counter })
         (Repository.read_checksum copts.repo name)
     in
     if copts.dry then
       Format.fprintf copts.out "dry run, nothing written.@."
     else
       Repository.write_checksum copts.repo cs ;
     Format.fprintf copts.out "updated checksum %s@." name ;
     show_checksum copts cs
  | _ -> `Error (false, "dunno what to generate")

let sign copts item name =
  Nocrypto_entropy_unix.initialize () ;
  let sign raw =
    match Private.read_private_key ?id:copts.private_key copts.repo with
    | Some (id, p) -> Private.sign id p raw
    | None -> invalid_arg "no private_key"
  in
  match item with
  | `Key -> Utils.option
              (`Error (false, ("no such key " ^ name)))
              (fun k ->
               let sigv = sign (Data.publickey_raw k) in
               let key = { k with Publickey.signatures = sigv :: k.Publickey.signatures } in
               if copts.dry then
                 Format.fprintf copts.out "dry run, nothing written.@."
               else
                 Repository.write_key copts.repo key ;
               Format.fprintf copts.out "signed key %s@." name ;
               show_key copts key)
              (Repository.read_key copts.repo name)
  | `Delegate -> Utils.option
                   (`Error (false, ("no delegate " ^ name)))
                   (fun d ->
                    let sigv = sign (Data.delegate_raw d) in
                    let d = { d with Delegate.signatures = sigv :: d.Delegate.signatures } in
                    if copts.dry then
                      Format.fprintf copts.out "dry run, nothing written.@."
                    else
                      Repository.write_delegate copts.repo d ;
                    Format.fprintf copts.out "signed delegate %s@." name ;
                    show_delegate copts d)
                   (Repository.read_delegate copts.repo name)
  | `Checksum -> Utils.option
                   (`Error (false, ("no checksum " ^ name)))
                   (fun c ->
                    let sigv = sign (Data.checksums_raw c) in
                    let c = { c with Checksum.signatures = sigv :: c.Checksum.signatures } in
                    if copts.dry then
                      Format.fprintf copts.out "dry run, nothing written.@."
                    else
                      Repository.write_checksum copts.repo c ;
                    Format.fprintf copts.out "signed checksum %s@." name ;
                    show_checksum copts c)
                   (Repository.read_checksum copts.repo name)
  | _ -> `Error (false, "dunno what to sign")


open Cmdliner

let copts_sect = "COMMON OPTIONS"
let help_secs = [
 `S "GENERAL";
 `P "Conex provides a signed repository, containing data as well as public keys and ownership information.  The main idea is that each directory is owned by a set of keys (which can contract and expand dynamically).  An initial set of public keys, so called trust anchors, should be distributed out of band.  A set of maintainers can do janitor work, cleaning up and adjusting all directories, if needed.  These need to coordinate, there need to be either a signature by the owner, or a quorum of maintainer signatures on ownership and data information.";
 `P "If starting with a fresh repository, you likely want to generate a private and public key, then setup some delegations and distribute some data.";
 `S "ITEM";
 `P "Conex distinguishes five different items, which you can identify by name: private, key, delegate, checksum, and diff.  Private keys are stored in ~/.conex, key, delegate, and checksum are stored in the repository.  A diff is just a file containing a unified diff, which can be verified (if it applies cleanly and has valid signatures).";
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
    let doc = "filter on the name of an item, depending on context." in
    Arg.(value & opt (some string) None & info ["name";"n"] ~docs ~docv:"ITEM" ~doc)
  and signed_by =
    let doc = "filter for a specific key identifier which signed the item." in
    Arg.(value & opt (some string) None & info ["signed-by";"by";"s"] ~docs ~docv:"KEYS" ~doc)
  and owner =
    let doc = "filter for a key identifier which owns the item." in
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
  let doc = "The kind of items."in
  Arg.(value & pos 0 conv `Repository & info [] ~docv:"ITEMTYPE" ~doc)

let item =
  let conv = Arg.enum items in
  let doc = "A single item type." in
  Arg.(value & pos 0 conv `Key & info [] ~docv:"ITEMTYPE" ~doc)

let value =
  let doc = "name of the item." in
  Arg.(value & pos 1 string "" & info [] ~docv:"ITEM" ~doc)

let show_cmd =
  let doc = "shows a single repository item" in
  let man =
    [`S "DESCRIPTION";
     `P "Shows a single item in the conex repository."]
  in
  Term.(ret (const show $ copts_t $ item $ value)),
  Term.info "show" ~doc ~man

let role =
  let doc = "Role of the key" in
  let conv =
    ((fun s -> try `Ok (string_to_role s) with _ -> `Error "not a role"),
     pp_role)
  in
  Arg.(value & opt conv `Developer & info ["role"; "r"] ~doc ~docv:"KEYS")

let valid =
  let doc = "Owners of this delegation, defaults to own keyid." in
  Arg.(value & opt_all string [] & info ["id"; "i"] ~doc ~docv:"KEYS")

let generate_cmd =
  let doc = "generates an item for the repository" in
  let man =
    [`S "DESCRIPTION";
     `P "Generates a single item in the conex repository"]
  in
  Term.(ret (const generate $ copts_t $ item $ value $ role $ valid)),
  Term.info "generate" ~doc ~man

(* modify_cmd = [key: role change ; delegate: add/remove id] *)

let sign_cmd =
  let doc = "signs an item in the repository" in
  let man =
    [`S "DESCRIPTION";
     `P "Sings a single item in the conex repository"]
  in
  Term.(ret (const sign $ copts_t $ item $ value)),
  Term.info "sign" ~doc ~man

let list_cmd =
  let doc = "lists repository items" in
  let man =
    [`S "DESCRIPTION";
     `P "Lists items in the conex repository."]
  in
  Term.(ret (const list $ copts_t $ kind)),
  Term.info "list" ~doc ~man

let verify_cmd =
  let doc = "verifies repository items" in
  let man =
    [`S "DESCRIPTION";
     `P "Verifies items in the conex repository."]
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
