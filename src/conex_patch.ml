open Conex_result
open Conex_core
open Conex_resource
open Conex_diff

let apply_diff provider diff =
  let read path =
    if file diff = path_to_string path then
      match provider.Provider.read path with
      | Ok data -> Ok (apply (Some data) diff)
      | Error _ -> Ok (apply None diff)
    else
      provider.Provider.read path
  and file_type path =
    let pn = path_to_string path
    and name = file diff
    in
    if pn = name then
      Ok File
    else if Conex_utils.String.is_prefix ~prefix:(pn ^ "/") name then
      Ok Directory
    else
      provider.Provider.file_type path
  and read_dir path =
    let name = List.rev (string_to_path (file diff))
    and path = List.rev path
    in
    (* XXX: unlikely to be correct... *)
    let data = match name with
      | fn::xs when xs = path -> Some (`File fn)
      | _ -> None
    in
    match provider.Provider.read_dir path, data with
      | Ok files, Some data -> Ok (data :: files)
      | Ok files, None -> Ok files
      | Error _, Some data -> Ok [data]
      | Error e, None -> Error e
  and write _ = invalid_arg "cannot write on a diff provider, is read-only!"
  and exists path =
    let pn = path_to_string path
    and name = file diff
    in
    if pn = name then
      true
    else
      provider.Provider.exists path
  and name = provider.Provider.name
  and description = "Patch provider"
  in
  { Provider.name ; description ; file_type ; read ; write ; read_dir ; exists }

let apply repo diff =
  let provider = Conex_repository.provider repo in
  let new_provider = apply_diff provider diff in
  Conex_repository.change_provider repo new_provider

let categorise diff =
  let p = string_to_path (file diff) in
  match Conex_opam_layout.(is_index p, is_authorisation p, is_releases p, is_item p, is_old_item p, is_compiler p) with
  | Some id, None, None, None, None, None ->
    (* Printf.printf "found an index in diff %s\n" id; *)
    `Id id
  | None, Some id, None, None, None, None ->
    (* Printf.printf "found an authorisation in diff %s\n" id; *)
    `Authorisation id
  | None, None, Some id, None, None, None ->
    `Releases id
  | None, None, None, Some (d, p), None, None ->
    (* Printf.printf "found a dir in diff %s %s\n" d p; *)
    `Package (d, p)
  | None, None, None, None, Some (d, p), None ->
    (* Printf.printf "found an olddir in diff %s %s\n" d p; *)
    `Package (d, p)
  | None, None, None, None, None, Some id ->
    (* Printf.printf "found a compiler in diff %s %s\n" d p; *)
    `Compiler id
  | _ ->
    (* XXX: handle error properly! *)
    Printf.printf "couldn't categorise %s\n" (path_to_string p) ;
    `Unknown

let diffs_to_components diffs =
  List.fold_left (fun (ids, auths, rels, pkgs) diff ->
      match categorise diff with
      | `Id id -> S.add id ids, auths, rels, pkgs
      | `Authorisation id -> ids, S.add id auths, rels, pkgs
      | `Releases id -> ids, auths, S.add id rels, pkgs
      | `Package (name, version) ->
        let s = try M.find name pkgs with Not_found -> S.empty in
        ids, auths, rels, M.add name (S.add version s) pkgs
      | _ -> ids, auths, rels, pkgs)
    (S.empty, S.empty, S.empty, M.empty) diffs


type err = [ verification_error
           | Conex_repository.base_error
           | `InsufficientQuorum of name * resource * S.t
           | `InvalidReleases of name * S.t * S.t
           | `AuthRelMismatch of name * name
           | `NotInReleases of name * S.t
           | `FileNotFound of name
           | `NotADirectory of name
           | `ChecksumsDiff of name * name list * name list * (Checksum.c * Checksum.c) list
           | `NameMismatch of string * string
           | `ParseError of name * string
           | `MissingSignature of identifier
           | `NotFound of string * string
           | `CounterNotIncreased
           | `CounterNotZero
           | `IllegalId
           | `IllegalName
           | `InvalidKeyTeam
           | `MissingAuthorisation of name ]


(* verification goes repo -> update -> (repo, err) result *)
(*  what needs to be verified?

we have an old repository (repo) and a diff file (patch) as input

take and split patch into categories:
 - ids
 - authorisation?!
 - releases!?
 - package

and process them in order:
- load janitors from repo
--> use this to load + verify janitors team ++ load+verify members
- process modified authors (unless already present)!?
--> check for everything removed (from resources -- but we can't atm) that it's still properly signed

- process each package in diff fully (if releases or authorisation changes, full pkg)


is there any need from not applying the id changes in one go?

actually, the whole diff... the parsing of diff file is only done to reduce the set of verifications which need to be done

    on key rollover (again):
    - read key, verify (should be good)
    - verify index

    --> incoming diff has both parts, but index needs to be loaded first in any case
    (what happens with new index with sig from new key? -- need to apply key first, but that's not verified then  :/)
*)
(*let verify repo = function
  | Idx (id, diff) ->
    let repo' = apply repo diff in
    begin match
        Conex_repository.read_index repo id,
        Conex_repository.read_index repo' id
      with
      | Ok idx, Ok idx' ->
        guard (idx.Index.counter < idx'.Index.counter) `CounterNotIncreased >>= fun _ ->
        (*        Conex_repository.verify_index repo idx' >>= fun (r, _, _) -> *)
        Ok repo
      | Error _, Ok idx' ->
        guard (idx'.Index.counter = Uint.zero) `CounterNotZero >>= fun () ->
        guard (Conex_opam_layout.valid_id id) `IllegalId >>= fun () ->
        guard (Conex_opam_layout.unique_id (Conex_repository.ids repo) id) `IllegalId >>= fun () ->
        (*        Conex_repository.verify_index repo idx' >>= fun (r, _, _) -> *)
        Ok repo
          (* XXX: correctness - verify now adds (but to repo, not repo'!),
             used to be:
             Ok (Conex_repository.add_index repo' idx') *)
      | _, Error e -> Error e
    end

  | Authorisation (name, diff) ->
    let repo' = apply repo diff in
    begin match
        Conex_repository.read_authorisation repo name,
        Conex_repository.read_authorisation repo' name
      with
      | Ok a, Ok a' ->
        guard (a.Authorisation.counter < a'.Authorisation.counter) `CounterNotIncreased >>= fun () ->
        Conex_repository.verify_authorisation repo a' >>= fun _ ->
        Ok repo'
      | Error _, Ok a' ->
        guard (a'.Authorisation.counter = Uint.zero) `CounterNotZero >>= fun () ->
        guard (Conex_opam_layout.valid_name name) `IllegalName >>= fun () ->
        guard (Conex_opam_layout.unique_data (Conex_repository.items repo) name) `IllegalName >>= fun () ->
        Conex_repository.verify_authorisation repo a' >>= fun _ ->
        Ok repo'
      | _, Error e -> Error e
    end

  | Dir (p, v, diffs) ->
    let repo' = List.fold_left apply repo diffs in
    let cs_ok a r = match
        Conex_repository.read_checksum repo v,
        Conex_repository.read_checksum repo' v
      with
      | Ok cs, Ok cs' ->
        guard (cs.Checksum.counter < cs'.Checksum.counter) `CounterNotIncreased >>= fun () ->
        Conex_repository.verify_checksum repo' a r cs' >>= fun _ ->
        Ok repo'
      | Error _, Ok cs' ->
        guard (cs'.Checksum.counter = Uint.zero) `CounterNotZero >>= fun () ->
        Conex_repository.verify_checksum repo' a r cs' >>= fun _ ->
        Ok repo'
      | Ok _, Error _ -> Ok repo' (* deletion *)
      | Error _, Error e -> Error e
    in
    begin match
        Conex_repository.read_authorisation repo p,
        Conex_repository.read_releases repo p,
        Conex_repository.read_releases repo' p
      with
      | Ok a, Ok r, Ok r' ->
        guard (r.Releases.counter < r'.Releases.counter) `CounterNotIncreased >>= fun () ->
        Conex_repository.verify_releases repo' a r' >>= fun _ ->
        cs_ok a r'
      | Ok a, Error _, Ok r' ->
        guard (r'.Releases.counter = Uint.zero) `CounterNotZero >>= fun () ->
        Conex_repository.verify_releases repo' a r' >>= fun _ ->
        cs_ok a r'
      | Error _, _, _ -> Error (`MissingAuthorisation p)
      | Ok _, _, Error e -> Error e
    end

  | OldDir _ -> Ok repo
    *)
let verify_patch repo _newrepo (_ids, _auths, _rels, _pkgs) =
  Ok repo
(*  quo vadis? *)

let verify_diff repo data =
  let diffs = to_diffs data in
  let comp = diffs_to_components diffs in
  let repo' = List.fold_left apply repo diffs in
  verify_patch repo repo' comp
