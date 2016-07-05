open Core

type item = [
  | `File of string
  | `Dir of string
]

type err = [ `NotFound | `UnknownFileType of string ]

type t = {
  name : string ;
  description : string ;
  file_type : path -> (file_type, err) result ;
  read : path -> (string, err) result ;
  write : path -> string -> unit ;
  read_dir : path -> (item list, err) result ;
  exists : path -> bool ;
}

let pp_provider ppf t =
  Format.fprintf ppf "repository %s: %s@." t.name t.description

let fs_provider basedir =
  let get path = path_to_string (basedir :: path) in
  let ensure_dir path =
    let rec mkdir base = function
      | [] -> ()
      | [_] -> ()
      | x::xs ->
         let path = base @ [x] in
         let str = path_to_string path in
         if not (Sys.is_directory str) then
           Unix.mkdir (path_to_string path) 0o755 ;
         mkdir path xs
    in
    mkdir [basedir] path
  in
  let file_type path =
    Utils.option (Error `NotFound) (fun x -> Ok x)
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
