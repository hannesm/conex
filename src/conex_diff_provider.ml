open Conex_utils
open Conex_io
open Conex_diff

(* this a very basic implementation, far from being general:
   - only a single (well-formed) patch file is used for creating a diff provider
*)
let apply provider diff =
  let read path =
    let pn = path_to_string path in
    let opt_read p = match provider.read (string_to_path_exn p) with
      | Ok data -> Some data
      | Error _ -> None
    in
    match diff.operation with
    | Delete p when p = pn -> Error "does not exist anymore (deleted)"
    | Create p when p = pn -> Ok (patch None diff)
    | Edit p when p = pn -> Ok (patch (opt_read p) diff)
    | Rename (previous, p) when p = pn -> Ok (patch (opt_read previous) diff)
    | Rename (p, _) when p = pn -> Error "does not exist anymore (renamed)"
    | _ -> provider.read path
  and file_type path =
    let pn = path_to_string path in
    match diff.operation with
    | Delete p when p = pn -> Error "does not exist anymore (deleted)"
    | Create p when p = pn -> Ok File
    | Edit p when p = pn -> Ok File
    | Rename (_, p) when p = pn -> Ok File
    | Rename (p, _) when p = pn -> Error "does not exist anymore (renamed)"
    | _ -> provider.file_type path
  and read_dir path =
    let rec strip parent x = match parent, x with
      | [], xs -> Some xs
      | hd::tl, hd'::tl' when hd = hd' -> strip tl tl'
      | _ -> None
    in
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
    let other = match provider.read_dir path with
      | Ok files -> files
      | Error _ -> []
    in
    let prefix = path_to_string path in
    let without o = function
      | None -> o
      | Some x -> List.filter (fun y -> not (y = x)) o
    and add o = function
      | None -> o
      | Some x -> x :: o
    in
    let r = match diff.operation with
      | Delete p when String.is_prefix ~prefix p -> without other (dropped_pre p)
      | Create p when String.is_prefix ~prefix p -> add other (dropped_pre p)
      | Edit _ -> other
      | Rename (o, n) ->
        let o' =
          if String.is_prefix ~prefix o then
            without other (dropped_pre o)
          else
            other
        in
        if String.is_prefix ~prefix n then add o' (dropped_pre n) else o'
      | _ -> other
    in
    if r = [] then Error "empty" else Ok r
  and write _ _ = Error "read only"
  and exists path =
    let pn = path_to_string path in
    match diff.operation with
    | Delete p when p = pn -> false
    | Create p when p = pn -> true
    | Edit p when p = pn -> true
    | Rename (_, p) when p = pn -> true
    | Rename (p, _) when p = pn -> false
    | _ -> provider.exists path
  and basedir = provider.basedir
  and description = "Patch provider"
  in
  { basedir ; description ; file_type ; read ; write ; read_dir ; exists }

let apply_diff io data =
  let diffs = Conex_diff.to_diffs data in
  List.fold_left apply io diffs, diffs
