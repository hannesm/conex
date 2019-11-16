open Conex_utils
open Conex_io
open Conex_diff

(* this a very basic implementation, far from being general:
   - only a single (well-formed) patch file is used for creating a diff provider

this implies the invariant that for each path p, there exists at most one
  matching diff (multiple kinds are possible:
- edited
 --- p
 +++ p
- removed
 --- p
 +++ /dev/null
- created
 --- /dev/null
 +++ p
- renamed
 --- p2
 +++ p

TODO: what happens if p2 now becomes a directory?

so for any incoming path p, there are four functions to support:
- exists: if theirs = p then true, if mine = p (&& not theirs p) then false
- file_type: if theirs = p then Ok file, p^"/" 'is a prefix of' theirs then Ok directory
- read: if theirs = p then read_old (if none, no) and apply patch
- read_dir: edit and created are trivial, remove and rename are tricky!
*)
let apply provider diff =
  let read path =
    let pn = path_to_string path in
    match diff.their_name with
    | Some p when p = pn ->
      let old = match diff.mine_name with
        | None -> None
        | Some p -> match provider.read (string_to_path_exn p) with
          | Ok data -> Some data
          | Error _ -> None
      in
      Ok (patch old diff)
    | _ -> match diff.mine_name with
      | Some p when p = pn -> Error "does not exist anymore"
      | _ -> provider.read path
  and file_type path =
    let pn = path_to_string path in
    match diff.their_name with
    | Some p when p = pn -> Ok File
    | Some p when String.is_prefix ~prefix:(pn ^ "/") p -> Ok Directory
    | t -> match t, diff.mine_name with
      | _, Some p when p = pn -> Error "does not exist"
      | _ -> provider.file_type path
  and read_dir path =
    let rec strip parent x = match parent, x with
      | [], xs -> Some xs
      | hd::tl, hd'::tl' when hd = hd' -> strip tl tl'
      | _ -> None
    in
    let local_add, local_remove =
      let prefix = path_to_string path in
      let dropped_pre p =
        match string_to_path p with
        | Error _ -> None
        | Ok p ->
          match strip path p with
          | None -> None
          | Some [] -> None
          | Some [ x ] -> Some (File, x)
          | Some (x::_) -> Some (Directory, x)
      in
      match diff.mine_name, diff.their_name with
      | Some p, Some p' when p = p' -> None, None
      | Some p, Some p' ->
        (if String.is_prefix ~prefix p' then dropped_pre p' else None),
        (if String.is_prefix ~prefix p then dropped_pre p else None)
      | None, Some p' ->
        (if String.is_prefix ~prefix p' then dropped_pre p' else None), None
      | Some p, None ->
        None, (if String.is_prefix ~prefix p then dropped_pre p else None)
      | None, None -> assert false
    in
    let f ys =
      match local_remove with
      | None -> ys
      | Some remove -> List.filter (fun x -> not (remove = x)) ys
    in
    match provider.read_dir path, local_add with
    | Ok files, Some add -> Ok (f (add :: files))
    | Ok files, None -> Ok (f files)
    | Error _, Some add -> Ok [add]
    | Error e, None -> Error e
  and write _ _ = Error "read only"
  and exists path =
    let pn = path_to_string path in
    match diff.their_name with
    | Some p when p = pn -> true
    | t -> match t, diff.mine_name with
      | _, Some p when p = pn -> false
      | _ -> provider.exists path
  and basedir = provider.basedir
  and description = "Patch provider"
  in
  { basedir ; description ; file_type ; read ; write ; read_dir ; exists }

let apply_diff io data =
  let diffs = Conex_diff.to_diffs data in
  List.fold_left apply io diffs, diffs
