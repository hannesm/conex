open Conex_result
open Conex_core
open Provider

let fs_provider basedir =
  if not (Conex_persistency.exists basedir) then Conex_persistency.mkdir basedir ;
  let get path = path_to_string (basedir :: path) in
  let ensure_dir path =
    let rec mkdir base = function
      | [] -> ()
      | [_] -> ()
      | x::xs ->
         let path = base @ [x] in
         let str = path_to_string path in
         if not (Conex_persistency.exists str) then
           Conex_persistency.mkdir (path_to_string path) ;
         match Conex_persistency.file_type str with
         | Some Directory -> mkdir path xs
         | _ -> invalid_arg (str ^ " is not a directory")
    in
    mkdir [basedir] path
  in
  let file_type path =
    let p = get path in
    if Conex_persistency.exists p then
      Conex_utils.option
        (Error `NotFound)
        (fun x -> Ok x)
        (Conex_persistency.file_type p)
    else
      Error `NotFound
  and read path =
    let fn = get path in
    if Conex_persistency.exists fn then
      Ok (Conex_persistency.read_file fn)
    else
      Error `NotFound
  and write path data =
    ensure_dir path ;
    let nam = get path in
    Conex_persistency.write_replace nam data
  and read_dir path =
    let abs = get path in
    if Conex_persistency.exists abs then
      foldM (fun acc fn ->
          let fullfn = Filename.concat abs fn in
          match Conex_persistency.file_type fullfn with
          | Some File -> Ok (`File fn :: acc)
          | Some Directory -> Ok (`Dir fn :: acc)
          | None -> Error (`UnknownFileType fullfn))
        [] (Conex_persistency.collect_dir abs)
    else
      Error `NotFound
  and exists path =
    Conex_persistency.exists (get path)
  in
  { name = basedir ; description = "File system provider" ; file_type ; read ; write ; read_dir ; exists }

let fs_ro_provider basedir =
  let fs = fs_provider basedir
  and write _ _ = ()
  and description = "Read only file system provider"
  in
  { fs with description ; write }
