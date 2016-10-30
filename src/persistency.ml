
let exists = Sys.file_exists

let remove = Sys.remove

let rename = Sys.rename

let file_type filename =
  let open Unix in
  let stat = stat filename in
  match stat.st_kind with
  | S_REG -> Some Core.File
  | S_DIR -> Some Core.Directory
  | _ -> None

let read_file filename =
  let open Unix in
  let fd = openfile filename [ O_RDONLY ] 0 in
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
  Bytes.to_string buf

let write_file ?(mode = 0o644) filename data =
  let open Unix in
  let fd = openfile filename [ O_WRONLY ; O_EXCL ; O_CREAT ] mode in
  let length = String.length data in
  let written = write fd (Bytes.of_string data) 0 length in
  assert (length = written) ;
  close fd

let write_replace ?mode filename data =
  if exists filename then
    let tmp = filename ^ ".tmp" in
    (if exists tmp then
       remove tmp) ;
    write_file ?mode tmp data ;
    remove filename ;
    rename tmp filename
  else
    write_file ?mode filename data

let collect_dir dir =
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
  res
