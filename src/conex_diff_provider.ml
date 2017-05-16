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
    let name = List.rev (string_to_path (file diff))
    and path = List.rev path
    in
    (* XXX: unlikely to be correct... *)
    let data = match name with
      | fn::xs when xs = path -> Some (File, fn)
      | _ -> None
    in
    match provider.read_dir path, data with
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
