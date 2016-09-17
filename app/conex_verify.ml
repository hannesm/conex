open Core

let out = Format.std_formatter

let debug = true

let strict = false

let maybe_exit () = if strict then exit 1

let load_trust_anchors dir =
  let keys = List.map (Filename.concat dir) (Persistency.collect_dir dir) in
  let keys =
    List.fold_left (fun acc f ->
        try
          let content = Persistency.read_file f in
          match Data.string_to_publickey content with
          | Ok key -> key :: acc
          | Error e -> Format.fprintf out "error while constructing key %a: %s@." pp_id f e ; maybe_exit () ; acc
        with
        | Invalid_argument x ->
          Format.fprintf out "error while loading %s: %s@." f x ; maybe_exit () ; acc)
      [] keys
  in
  Format.fprintf out "Loaded %s, found %d trust anchors: %a@."
    dir (List.length keys) (pp_list Publickey.pp_publickey) keys ;
  keys

let load_verify_idx r k id =
  match Repository.read_index r id with
  | Error e -> Repository.pp_r_err out e ; maybe_exit () ; r
  | Ok i ->
    let r' = Repository.add_trusted_key r k in
    match Repository.verify_index r' i with
    | Error e -> pp_verification_error out e ; maybe_exit () ; r
    | Ok id ->
      if debug then Format.fprintf out "loaded index for %a@." pp_id id ;
      let r' = Repository.add_index r' i in
      match Repository.verify_key r' k with
      | Error e -> Repository.pp_error out e ; maybe_exit () ; r
      | Ok ok -> if debug then Repository.pp_ok out ok ; r'

let load_key_idx id r =
  match Repository.read_key r id with
  | Error e -> Repository.pp_r_err out e ; maybe_exit () ; r
  | Ok k -> load_verify_idx r k id

let load_id id r =
  match Repository.read_id r id with
  | Error e -> Repository.pp_r_err out e ; maybe_exit () ; r
  | Ok (`Team t) ->
    begin match Repository.verify_team r t with
      | Ok _ -> Repository.add_team r t
      | Error e -> Repository.pp_error out e ; maybe_exit () ; r
    end
  | Ok (`Key k) -> load_verify_idx r k id

let load_index id r =
  match Repository.read_index r id with
  | Error _ -> r
  | Ok i -> match Repository.verify_index r i with
    | Error _ -> r
    | Ok id ->
      if debug then Format.fprintf out "loaded trust anchor index for %a@." pp_id id ;
      Repository.add_index r i

let verify_complete_repository directory trust =
  if not (Persistency.exists directory) then begin
    Format.fprintf out "directory %s does not exist@." directory ;
    exit (-1)
  end ;
  if not (Persistency.exists trust) then begin
    Format.fprintf out "trust anchor directory %s does not exist@." trust ;
    exit (-1)
  end ;
  (* a) load trust anchors *)
  let r =
    let anchors = load_trust_anchors trust in
    let p = Provider.fs_ro_provider directory in
    let r = Repository.repository p in
    let r = List.fold_left Repository.add_trusted_key r anchors in
    let members = S.of_list (List.map (fun k -> k.Publickey.keyid) anchors) in
    let r = Repository.add_team r (Team.team ~members "janitors") in
    (* we only read the indexes and verify those *)
    S.fold load_index members r
  in
  (* b) load janitors *)
  let js = match Repository.read_team r "janitors" with
    | Error e -> Repository.pp_r_err out e ; maybe_exit () ; S.empty
    | Ok team -> match Repository.verify_team r team with
      | Error e -> Repository.pp_error out e ; maybe_exit () ; S.empty
      | Ok _ -> team.Team.members
  in
  let r = S.fold load_key_idx js r in
  let r = Repository.add_team r (Team.team ~members:js "janitors") in
  (* c) load all other identities [can also be teams!] *)
  let r = S.fold load_id (S.diff (Repository.all_ids r) js) r in
  (* d) for each package: read & verify authorisation, releases, checksums *)
  S.iter (fun name ->
      match Repository.read_authorisation r name with
      | Error e -> Repository.pp_r_err out e ; maybe_exit ()
      | Ok auth -> match Repository.verify_authorisation r auth with
        | Error e -> Repository.pp_error out e ; maybe_exit ()
        | Ok ok -> if debug then Repository.pp_ok out ok ;
          match Repository.read_releases r name with
          | Error e -> Repository.pp_r_err out e ; maybe_exit ()
          | Ok rel -> match Repository.verify_releases r auth rel with
            | Error e -> Repository.pp_error out e ; maybe_exit ()
            | Ok ok -> if debug then Repository.pp_ok out ok ;
              let good = ref true in
              S.iter (fun rname ->
                  match Repository.read_checksum r rname with
                  | Error e -> Repository.pp_r_err out e ; good := false ; maybe_exit ()
                  | Ok cs -> match Repository.verify_checksum r auth rel cs with
                    | Error e -> Repository.pp_error out e ; good := false ; maybe_exit ()
                    | Ok ok -> if debug then Repository.pp_ok out ok)
                rel.Releases.releases ;
              if !good then Format.fprintf out "verified %a@." pp_name name)
    (Repository.all_authorisations r) ;
  exit 0

let () =
  match Sys.argv with
  | [| _ ; directory ; trust |] -> verify_complete_repository directory trust
  | _ -> Printf.eprintf "expecting two arguments: <directory> <trust anchors>"
