open Conex_result
open Conex_core
open Conex_resource

open Conex_common

let out = Format.std_formatter

let debug = false

let strict = false

let maybe_exit () = if strict then exit 1

let verify_complete_repository directory trust =
  if not (Conex_persistency.exists directory) then begin
    Format.fprintf out "directory %s does not exist@." directory ;
    exit (-1)
  end ;
  if not (Conex_persistency.exists trust) then begin
    Format.fprintf out "trust anchor directory %s does not exist@." trust ;
    exit (-1)
  end ;
  (* a) load trust anchors and janitors *)
  let p = Conex_provider.fs_ro_provider directory in
  let r = Conex_repository.repository p in
  let r = load_anchors_janitors r out debug maybe_exit trust in
  (* b) load all other identities [can also be teams!] *)
  let r =
    S.fold (load_id out debug maybe_exit)
      (S.diff (Conex_repository.ids r)
         (match Conex_repository.find_team r "janitors" with None -> S.empty | Some s -> s)) r
  in
  (* c) for each package: read & verify authorisation, releases, checksums *)
  S.iter (fun name ->
      match Conex_repository.read_authorisation r name with
      | Error e -> Conex_repository.pp_r_err out e ; maybe_exit ()
      | Ok auth -> match Conex_repository.verify_authorisation r auth with
        | Error e -> Conex_repository.pp_error out e ; maybe_exit ()
        | Ok ok -> if debug then Conex_repository.pp_ok out ok ;
          match Conex_repository.read_releases r name with
          | Error e -> Conex_repository.pp_r_err out e ; maybe_exit ()
          | Ok rel ->
            let good = ref true in
            (match Conex_repository.verify_releases r auth rel with
             | Error e -> Conex_repository.pp_error out e ; good := false ; maybe_exit ()
             | Ok ok -> if debug then Conex_repository.pp_ok out ok) ;
            S.iter (fun rname ->
                match Conex_repository.read_checksum r rname with
                | Error e -> Conex_repository.pp_r_err out e ; good := false ; maybe_exit ()
                | Ok cs -> match Conex_repository.verify_checksum r auth rel cs with
                  | Error e -> Conex_repository.pp_error out e ; good := false ; maybe_exit ()
                  | Ok ok -> if debug then Conex_repository.pp_ok out ok)
              rel.Releases.releases ;
            if !good then Format.fprintf out "verified %a@." pp_name name)
    (Conex_repository.items r) ;
  exit 0

let () =
  match Sys.argv with
  | [| _ ; directory ; trust |] -> verify_complete_repository directory trust
  | _ -> Printf.eprintf "expecting two arguments: <directory> <trust anchors>\n"
