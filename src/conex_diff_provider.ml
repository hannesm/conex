open Conex_utils
open Conex_io
open Conex_diff

(* this a very basic implementation, far from being general:
   - only a single (well-formed) patch file is used for creating a diff provider
*)
let target = function
  | Edit name
  | Rename (_, name)
  | Rename_only (_, name)
  | Create name -> Some name
  | Delete _ -> None

let source = function
  | Rename (old, _) | Rename_only (old, _) | Delete old | Edit old -> Some old
  | _ -> None

module FS = Set.Make(struct
    type t = file_type * string
    let compare (t, v) (t', v') = match t, t' with
      | File, File | Directory, Directory -> String.compare v v'
      | File, Directory -> 1 | Directory, File -> -1
  end)

let apply provider diffs =
  let find_diff f path =
    List.find_opt (fun x ->
        match f x.operation with
        | None -> false
        | Some path' -> path_equal (string_to_path_exn path') path)
      diffs
  in
  let read path =
    match find_diff target path, find_diff source path with
    | None, None -> provider.read path
    | None, Some _ -> Error "no data"
    | Some diff, _ ->
      let res_patch old = match patch old diff with
        | None -> Error "no data"
        | Some data -> Ok data
      in
      match source diff.operation with
      | None -> res_patch None
      | Some x -> match provider.read (string_to_path_exn x) with
        | Error x -> Error x
        | Ok data -> res_patch (Some data)
  and file_type path =
    match find_diff target path, find_diff source path with
    | None, None -> provider.file_type path
    | None, Some _ -> Error "does not exist anymore (deleted)"
    | Some _, _ -> Ok File
  and read_dir path =
    (* this results in some empty directories which are not present on disk *)
    (* the reason is that I get the old files and directories,
       I also know the diffs (i.e. added and removed and renamed files), but
       for inspecting whether a directory is empty (i.e. all files are removed),
       I'd need the whole subdir information (prune empty ones) *)
    let old = match provider.read_dir path with
      | Ok files -> FS.of_list files
      | Error _ -> FS.empty
    in
    let drop_pre dir path' =
      let rec dropit a b = match a, b with
        | [], [ x ] -> Some (File, x)
        | [], x::_ -> if dir then Some (Directory, x) else None
        | x::xs, y::ys when String.equal x y -> dropit xs ys
        | _ -> None
      in
      dropit path (string_to_path_exn path')
    and opt_add x xs = match x with None -> xs | Some x -> FS.add x xs
    and opt_rem x xs = match x with None -> xs | Some x -> FS.remove x xs
    in
    let stuff =
      List.fold_left (fun acc d ->
          match d.operation with
          | Create name | Edit name -> opt_add (drop_pre true name) acc
          | Rename (old, name) | Rename_only (old, name) ->
            opt_rem (drop_pre false old) (opt_add (drop_pre true name) acc)
          | Delete old -> opt_rem (drop_pre false old) acc)
        old diffs
    in
    Ok (FS.elements stuff)
  and write _ _ = Error "read only"
  and exists path =
    match find_diff target path, find_diff source path with
    | None, None -> provider.exists path
    | Some _, _ -> true
    | None, Some _ -> false
  and basedir = provider.basedir
  and description = "Patch provider"
  in
  { basedir ; description ; file_type ; read ; write ; read_dir ; exists }

let apply_diff io data =
  let diffs = Conex_diff.to_diffs data in
  apply io diffs, diffs
