open Core

let out = Format.std_formatter

let debug = true

let strict = false

let maybe_exit () = if strict then exit 1

let load_trust_anchors repo dir =
  let keys = List.map (Filename.concat dir) (Persistency.collect_dir dir) in
  let keys =
    List.fold_left (fun acc f ->
        try
          let content = Persistency.read_file (Filename.concat dir f) in
          let data = Data.parse content in
          let key = Data.data_to_publickey data in
          key :: acc
        with
        | Invalid_argument x ->
          Format.fprintf out "error while loading %s: %s\n" f x ; maybe_exit () ; acc)
      [] keys
  in
  let repo = List.fold_left Repository.add_trusted_key repo keys in
  Format.fprintf out "Loaded %s, found %d trust anchors: %a@."
    dir (List.length keys) (pp_list Publickey.pp_publickey) keys ;
  repo

let load_id id r =
  match Repository.read_key r id with
  | Error e -> Repository.pp_r_err out e ; maybe_exit () ; r
  | Ok k -> match Repository.read_index r id with
    | Error e -> Repository.pp_r_err out e ; maybe_exit () ; r
    | Ok i ->
      let r' = Repository.add_trusted_key r k in
      match Repository.verify_index r' i with
      | Error e -> pp_verification_error out e ; maybe_exit () ; r
      | Ok id ->
        if debug then Format.fprintf out "loaded index for %a@." pp_id id ;
        let r' = Repository.add_index r' i in
        match Repository.verify_key r' k with
        | Error e -> pp_error out e ; maybe_exit () ; r
        | Ok ok -> if debug then Repository.pp_ok out ok ; r'

let verify_complete_repository directory trust =
  if not (Persistency.exists directory) then begin
    Format.fprintf out "directory %s does not exist@." directory ;
    exit (-1)
  end ;
  if not (Persistency.exists trust) then begin
    Format.fprintf out "trust anchor directory %s does not exist@." trust ;
    exit (-1)
  end ;
  let p = Provider.fs_ro_provider directory in
  let r = Repository.repository p in
  (* a) load trust anchors *)
  let r = load_trust_anchors r trust in
  (* b) load janitors *)
  let r = S.fold load_id (Repository.all_janitors r) r in
  (* c) load all other indexes *)
  let r = S.fold load_id (Repository.all_authors r) r in
  (* d) for each package: read & verify authorisation, releases, checksums *)
  S.iter (fun name ->
      match Repository.read_authorisation r name with
      | Error e -> Repository.pp_r_err out e ; maybe_exit ()
      | Ok auth -> match Repository.verify_authorisation r auth with
        | Error e -> pp_error out e ; maybe_exit ()
        | Ok ok -> if debug then Repository.pp_ok out ok ;
          match Repository.read_releases r name with
          | Error e -> Repository.pp_r_err out e ; maybe_exit ()
          | Ok rel -> match Repository.verify_releases r auth rel with
            | Error e -> pp_error out e ; maybe_exit ()
            | Ok ok -> if debug then Repository.pp_ok out ok ;
              let good = ref true in
              S.iter (fun rname ->
                  match Repository.read_checksum r rname with
                  | Error e -> Repository.pp_r_err out e ; good := false ; maybe_exit ()
                  | Ok cs -> match Repository.verify_checksum r auth rel cs with
                    | Error e -> pp_error out e ; good := false ; maybe_exit ()
                    | Ok ok -> if debug then Repository.pp_ok out ok)
                rel.Releases.releases ;
              if !good then Format.fprintf out "verified %a@." pp_name name)
    (Repository.all_authorisations r) ;
  exit 0

let () =
  match Sys.argv with
  | [| _ ; directory ; trust |] -> verify_complete_repository directory trust
  | _ -> Printf.eprintf "expecting two arguments: <directory> <trust anchors>"
