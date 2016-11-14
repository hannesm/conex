open Conex_result
open Conex_core
open Provider

let fs_provider basedir =
  if not (Persistency.exists basedir) then
    Unix.mkdir basedir 0o755 ;
  let get path = path_to_string (basedir :: path) in
  let ensure_dir path =
    let rec mkdir base = function
      | [] -> ()
      | [_] -> ()
      | x::xs ->
         let path = base @ [x] in
         let str = path_to_string path in
         if not (Persistency.exists str) then
           Unix.mkdir (path_to_string path) 0o755 ;
         if not (Sys.is_directory str) then
           invalid_arg (str ^ " is not a directory") ;
         mkdir path xs
    in
    mkdir [basedir] path
  in
  let file_type path =
    Conex_utils.option
      (Error `NotFound)
      (fun x -> Ok x)
      (Persistency.file_type (get path))
  and read path =
    let fn = get path in
    if Persistency.exists fn then
      Ok (Persistency.read_file fn)
    else
      Error `NotFound
  and write path data =
    ensure_dir path ;
    let nam = get path in
    Persistency.write_replace nam data
  and read_dir path =
    let abs = get path in
    if Persistency.exists abs then
      foldM (fun acc fn ->
          let fullfn = Filename.concat abs fn in
          match Persistency.file_type fullfn with
          | Some File -> Ok (`File fn :: acc)
          | Some Directory -> Ok (`Dir fn :: acc)
          | None -> Error (`UnknownFileType fullfn))
        [] (Persistency.collect_dir abs)
    else
      Error `NotFound
  and exists path =
    Persistency.exists (get path)
  in
  { name = basedir ; description = "File system provider" ; file_type ; read ; write ; read_dir ; exists }

let fs_ro_provider basedir =
  let fs = fs_provider basedir
  and write _ = invalid_arg "read only file system provider"
  and description = "Read only file system provider"
  in
  { fs with description ; write }
