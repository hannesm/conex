open Conex_utils

let ( let* ) = Result.bind

let exists = Sys.file_exists

let guard_unix f x =
  try Ok (f x) with
  | Unix.Unix_error (e, f, arg) ->
    let msg = Unix.error_message e in
    Error (Printf.sprintf "error %s while %s (%s)" msg f arg)
  | _ -> Error "unknown error"

let mkdir ?(mode = 0o755) name = guard_unix (Unix.mkdir name) mode

let guard_sys f x =
  try Ok (f x) with
  | Sys_error msg -> Error msg
  | _ -> Error "unknown error"

let remove a = guard_sys Sys.remove a

let rename a b = guard_sys (Sys.rename a) b

let file_type filename =
  let* stat = guard_unix Unix.stat filename in
  match stat.Unix.st_kind with
  | Unix.S_REG -> Ok File
  | Unix.S_DIR -> Ok Directory
  | _ -> Error "unsupported file type"

let read_file filename =
  guard_unix (fun file ->
      let open Unix in
      let fd = openfile file [ O_RDONLY ] 0 in
      let len = (fstat fd).st_size in
      let buf = Bytes.create len in
      let rec rread idx =
        let r = read fd buf idx (len - idx) in
        if r + idx = len then
          close fd
        else
          rread (r + idx)
      in
      rread 0 ;
      Bytes.to_string buf) filename

let write_file ?(mode = 0o644) filename data =
  guard_unix (fun file ->
      let open Unix in
      let fd = openfile file [ O_WRONLY ; O_EXCL ; O_CREAT ] mode in
      let bytes = Bytes.of_string data in
      let length = Bytes.length bytes in
      let written = write fd bytes 0 length in
      assert (length = written) ;
      close fd)
    filename

let write_replace ?mode filename data =
  if exists filename then
    let tmp = filename ^ ".tmp" in
    let* () = if exists tmp then remove tmp else Ok () in
    let* () = write_file ?mode tmp data in
    rename tmp filename
  else
    write_file ?mode filename data

let collect_dir dir =
  guard_unix (fun dir ->
      let open Unix in
      let dh = opendir dir in
      let next () = try Some (readdir dh) with End_of_file -> None in
      let rec doone acc = function
        | Some "." | Some ".." -> doone acc (next ())
        | Some s -> doone (s :: acc) (next ())
        | None -> acc
      in
      let res = doone [] (next ()) in
      closedir dh ;
      res)
    dir
