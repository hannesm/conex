open Conex_utils
open Conex_provider

let fs_provider basedir =
  (if not (Conex_persistency.exists basedir) then
     Conex_persistency.mkdir basedir
   else
     Ok ()) >>= fun () ->
  let get path = path_to_string (basedir :: path) in
  let ensure_dir path =
    let rec mkdir base = function
      | [] -> Ok ()
      | [_] -> Ok ()
      | x::xs ->
         let path = base @ [x] in
         let str = path_to_string path in
         (if not (Conex_persistency.exists str) then
            Conex_persistency.mkdir (path_to_string path)
          else Ok ()) >>= fun () ->
         Conex_persistency.file_type str >>= function
         | Directory -> mkdir path xs
         | File -> Error (str ^ " is not a directory")
    in
    mkdir [basedir] path
  in
  let file_type path =
    let p = get path in
    Conex_persistency.file_type p
  and read path =
    let fn = get path in
    Conex_persistency.read_file fn
  and write path data =
    ensure_dir path >>= fun () ->
    let nam = get path in
    Conex_persistency.write_replace nam data
  and read_dir path =
    let abs = get path in
    Conex_persistency.collect_dir abs >>= fun files ->
    foldM (fun acc fn ->
        let fullfn = Filename.concat abs fn in
        Conex_persistency.file_type fullfn >>= function
        | File -> Ok (`File fn :: acc)
        | Directory -> Ok (`Dir fn :: acc))
      [] files
  and exists path =
    Conex_persistency.exists (get path)
  in
  Ok { name = basedir ; description = "File system provider" ; file_type ; read ; write ; read_dir ; exists }

let fs_ro_provider basedir =
  fs_provider basedir >>= fun fs ->
  let write _ _ = Ok ()
  and description = "Read only file system provider"
  in
  Ok { fs with description ; write }
