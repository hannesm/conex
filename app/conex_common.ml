open Conex_result
open Conex_core
open Conex_resource

let load_trust_anchors out maybe_exit dir =
  let keys = List.map (Filename.concat dir) (Conex_persistency.collect_dir dir) in
  let keys =
    List.fold_left (fun acc f ->
        match Conex_data.decode (Conex_persistency.read_file f) >>= Conex_data_persistency.t_to_publickey with
        | Ok key -> key :: acc
        | Error e -> Format.fprintf out "error while constructing key %a: %s@." pp_id f e ; maybe_exit () ; acc)
      [] keys
  in
  Format.fprintf out "Loaded %s, found %d trust anchors: %a@."
    dir (List.length keys) (pp_list pp_id) (List.map (fun k -> k.Publickey.name) keys) ;
  keys

let load_verify_idx out debug maybe_exit r k id =
  match Conex_repository.read_index r id with
  | Error e -> if debug then Conex_repository.pp_r_err out e ; maybe_exit () ; r
  | Ok i ->
    let r' = Conex_repository.add_trusted_key r k in
    match Conex_repository.verify_index r' i with
    | Error e -> if debug then pp_verification_error out e ; maybe_exit () ; r
    | Ok (r, warns, id) ->
      if debug then
        Format.fprintf out "loaded index for %a@.  %s"
          pp_id id
          (String.concat "\n  " warns) ;
      match Conex_repository.verify_key r' k with
      | Error e -> if debug then Conex_repository.pp_error out e ; maybe_exit () ; r
      | Ok (_, ok) -> if debug then Conex_repository.pp_ok out ok ; r'

let load_key_idx out debug maybe_exit id r =
  match Conex_repository.read_key r id with
  | Error e -> Conex_repository.pp_r_err out e ; maybe_exit () ; r
  | Ok k -> load_verify_idx out debug maybe_exit r k id

let load_id out debug maybe_exit id r =
  match Conex_repository.read_id r id with
  | Error e -> if debug then Conex_repository.pp_r_err out e ; maybe_exit () ; r
  | Ok (`Team t) ->
    begin match Conex_repository.verify_team r t with
      | Ok (r, _) -> if debug then Format.fprintf out "loaded team %a@." pp_id t.Team.name ; r
      | Error e -> if debug then Conex_repository.pp_error out e ; maybe_exit () ; r
    end
  | Ok (`Key k) -> load_verify_idx out debug maybe_exit r k id

let load_index out debug maybe_exit id r =
  match Conex_repository.read_index r id with
  | Error e -> Conex_repository.pp_r_err out e ; maybe_exit () ; r
  | Ok i -> match Conex_repository.verify_index r i with
    | Error e -> pp_verification_error out e ; maybe_exit () ; r
    | Ok (r, warns, id) ->
      if debug then
        Format.fprintf out "loaded trust anchor index for %a@.  %s"
          pp_id id
          (String.concat "\n  " warns) ;
      r

let load_anchors_janitors repo out debug maybe_exit dir =
  (* find trust anchors *)
  let anchors = load_trust_anchors out maybe_exit dir in
  let repo = List.fold_left Conex_repository.add_trusted_key repo anchors in
  let members = S.of_list (List.map (fun k -> k.Publickey.name) anchors) in
  let repo = Conex_repository.add_team repo (Team.team ~members "janitors") in
  (* we read the indexes and verify those *)
  let repo = S.fold (load_index out debug maybe_exit) members repo in
  (* now load the actual janitors team on disk *)
  let js = match Conex_repository.read_team repo "janitors" with
    | Error e -> Conex_repository.pp_r_err out e ; maybe_exit () ; S.empty
    | Ok team -> match Conex_repository.verify_team repo team with
      | Error e -> Conex_repository.pp_error out e ; maybe_exit () ; S.empty
      | Ok _ -> team.Team.members
  in
  let repo = S.fold (load_key_idx out debug maybe_exit) js repo in
  Conex_repository.add_team repo (Team.team ~members:js "janitors")

