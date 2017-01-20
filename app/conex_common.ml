open Conex_result
open Conex_core
open Conex_resource

(*
let load_anchors repo valid verbose strict =
  match Conex_repository.read_team repo "janitors" with
  | Error e -> verbose Conex_repository.pp_r_err e ; strict true
  | Ok team ->
    let keys = S.fold load_key team.Team.members [] in
    let trusted, rest =
      List.partition
        (fun k -> valid (Conex_nocrypto.digest
                           (Conex_data.encode
                              (Conex_data_persistency.publickey_to_t k))))
        keys
    in
    let repo = List.fold_left add_trusted_key repo trusted in
    let repo = List.fold_left load_verify_idx repo (List.map (fun p -> p.Publickey.name) trusted) in
    match foldM Conex_repository.verify_key repo trusted with
    | Error _ -> Error "couldn't verify keys for TA"
    | Ok _ -> match Conex_repository.verify_team repo team with
      | Error _ -> Error "couldn't verify janitors team from TA"
      | Ok _ ->
        let repo = List.fold_left add_trusted_key repo rest in
        List.fold_left load_verify_index repo

   load rest members
*)
