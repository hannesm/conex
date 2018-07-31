open Conex_utils
open Conex_io
open Conex_diff

let apply provider diff =
  let read path =
    if file diff = path_to_string path then
      match provider.read path with
      | Ok data -> Ok (patch (Some data) diff)
      | Error _ -> Ok (patch None diff)
    else
      provider.read path
  and file_type path =
    let pn = path_to_string path
    and name = file diff
    in
    if pn = name then
      Ok File
    else if Conex_utils.String.is_prefix ~prefix:(pn ^ "/") name then
      Ok Directory
    else
      provider.file_type path
  and read_dir path =
    let rec strip parent x = match parent, x with
      | [], xs -> Some xs
      | hd::tl, hd'::tl' when hd = hd' -> strip tl tl'
      | _ -> None
    in
    let local =
      match string_to_path (file diff) with
      | Ok p ->
        begin match strip path p with
          | None -> None
          | Some [] -> None
          | Some [ x ] -> Some (File, x)
          | Some (x::_) -> Some (Directory, x)
        end
      | Error _ -> None
    in
    match provider.read_dir path, local with
      | Ok files, Some data -> Ok (data :: files)
      | Ok files, None -> Ok files
      | Error _, Some data -> Ok [data]
      | Error e, None -> Error e
  and write _ _ = Error "read only"
  and exists path =
    let pn = path_to_string path
    and name = file diff
    in
    if pn = name then
      true
    else
      provider.exists path
  and basedir = provider.basedir
  and description = "Patch provider"
  in
  { basedir ; description ; file_type ; read ; write ; read_dir ; exists }

let apply_diff io data =
    let diffs = Conex_diff.to_diffs data in
    (List.fold_left apply io diffs, diffs)
