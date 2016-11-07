open Conex_core
open Conex_resource

open Conex_common

let out = Format.std_formatter

let debug = false

let strict = false

let maybe_exit () = if strict then exit 1

let verify_complete_repository directory trust =
  if not (Persistency.exists directory) then begin
    Format.fprintf out "directory %s does not exist@." directory ;
    exit (-1)
  end ;
  if not (Persistency.exists trust) then begin
    Format.fprintf out "trust anchor directory %s does not exist@." trust ;
    exit (-1)
  end ;
  (* a) load trust anchors and janitors *)
  let p = Provider.fs_ro_provider directory in
  let r = Repository.repository p in
  let r = load_anchors_janitors r out debug maybe_exit trust in
  (* b) load all other identities [can also be teams!] *)
  let r =
    S.fold (load_id out debug maybe_exit)
      (S.diff (Repository.all_ids r) (Repository.team r "janitors")) r
  in
  (* c) for each package: read & verify authorisation, releases, checksums *)
  S.iter (fun name ->
      match Repository.read_authorisation r name with
      | Error e -> Repository.pp_r_err out e ; maybe_exit ()
      | Ok auth -> match Repository.verify_authorisation r auth with
        | Error e -> Repository.pp_error out e ; maybe_exit ()
        | Ok ok -> if debug then Repository.pp_ok out ok ;
          match Repository.read_releases r name with
          | Error e -> Repository.pp_r_err out e ; maybe_exit ()
          | Ok rel ->
            let good = ref true in
            (match Repository.verify_releases r auth rel with
             | Error e -> Repository.pp_error out e ; good := false ; maybe_exit ()
             | Ok ok -> if debug then Repository.pp_ok out ok) ;
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
  | _ -> Printf.eprintf "expecting two arguments: <directory> <trust anchors>\n"
