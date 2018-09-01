open Conex_utils
open Conex_unix_persistency

let private_dir = Filename.concat (Sys.getenv "HOME") ".conex"

let private_s = "private"

let ids () =
  match collect_dir private_dir with
  | Ok files ->
    List.fold_left (fun acc s ->
        match List.rev (String.cuts '.' s) with
        | p::tl when p = private_s -> (String.concat "." (List.rev tl))::acc
        | _ -> acc (* TODO warn *) ) [] files
  | Error _ -> []

let private_key_path id = Filename.concat private_dir (id ^ "." ^ private_s)

let backup id filename =
  if exists filename then begin
    let backfn =
      let open Unix in
      let t = gmtime (stat filename).st_mtime in
      Printf.sprintf "%s.%4d%2d%2d%2d%2d%2d" id
        (t.tm_year + 1900) (succ t.tm_mon) t.tm_mday t.tm_hour t.tm_min t.tm_sec
    in
    let backup = private_key_path backfn in
    let rec inc n =
      if n = 10 then Error "too many backup keys, only 10 supported"
      else
        let nam = backup ^ "." ^ string_of_int n in
        if exists nam then inc (succ n) else Ok nam
    in
    (if exists backup then inc 0 else Ok backup) >>= fun backup ->
    rename filename backup
  end else
    Ok ()

let write id key =
  let filename = private_key_path id in
  backup id filename >>= fun () ->
  (if not (exists private_dir) then
     mkdir ~mode:0o700 private_dir
   else Ok ()) >>= fun () ->
  match file_type private_dir with
  | Ok Directory -> write_file ~mode:0o400 filename key
  | _ -> Error (private_dir ^ " is not a directory!")

let read to_ts id =
  let fn = private_key_path id in
  if exists fn then
    read_file fn >>= fun key ->
    let stat = Unix.stat fn in
    match to_ts stat.Unix.st_mtime with
    | None -> Error ("couldn't convert modification time to Uint.t")
    | Some ts -> Ok (key, ts)
  else
    Error ("couldn't find private key for " ^ id)
