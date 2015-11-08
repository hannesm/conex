open Core

type item = [
  | `File of string
  | `Dir of string
]

type t = {
  name : string ;
  description : string ;
  file_type : path -> Persistency.file_type ;
  read : path -> string option ;
  write : path -> string -> unit ;
  read_dir : path -> item list ;
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
  let file_type path = Persistency.file_type (get path)
  and read path =
    let fn = get path in
    if Persistency.exists fn then
      Some (Persistency.read_file fn)
    else
      None
  and write path data =
    ensure_dir path ;
    let nam = get path in
    Persistency.write_replace nam data
  and read_dir path =
    let abs = get path in
    List.map (fun f ->
        match Persistency.file_type (Filename.concat abs f) with
        | `File -> `File f
        | `Directory -> `Dir f)
      (Persistency.collect_dir abs)
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

