(*
open Core

module S = Set.Make(String)

let apply_diff provider diff =
  let read path =
    let orig = provider.Provider.read path in
    if Diff.file diff = path_to_string path then
      Diff.apply orig diff
    else
      orig
  and file_type path =
    let pn = path_to_string path
    and name = Diff.file diff
    in
    if pn = name then
      `File
    else if Strhelper.is_prefix ~prefix:(pn ^ "/") name then
      `Directory
    else
      provider.Provider.file_type path
  and read_dir path =
    let old = try provider.Provider.read_dir path with Unix.Unix_error _ -> [] in
    let name = List.rev (string_to_path (Diff.file diff))
    and path = List.rev path
    in
    (* XXX: unlikely to be correct... *)
    let data = match name with
      | fn::xs when xs = path -> (`File fn) :: old
      | _ -> old
    in
    data
  and write _ = invalid_arg "cannot write on a diff provider, is read-only!"
  and exists path =
    let pn = path_to_string path
    and name = Diff.file diff
    in
    if pn = name then
      true
    else
      provider.Provider.exists path
  and name = provider.Provider.name
  and description = "Patch provider"
  in
  { Provider.name ; description ; file_type ; read ; write ; read_dir ; exists }

let apply repo diff =
  let provider = Repository.provider repo in
  let new_provider = apply_diff provider diff in
  Repository.change_provider repo new_provider

type component =
  | Key of string * Diff.diff
  | Delegate of string * Diff.diff
  | Directory of string * string * Diff.diff list

let categorise diff =
  let p = string_to_path (Diff.file diff) in
  match Layout.is_key p, Layout.is_delegate p, Layout.is_item p with
  | Some id, None, None ->
    dbg "found a key in diff %s\n" id;
    Key (id, diff)
  | None, Some id, None ->
    dbg "found a delegate in diff %s\n" id;
    Delegate (id, diff)
  | None, None, Some (d, p) ->
    dbg "found a dir in diff %s %s\n" d p;
    Directory (d, p, [ diff ])
  | _ -> invalid_arg ("couldn't categorise " ^ (path_to_string p))

let diffs_to_components diffs =
  let has_key = List.exists (function Key _ -> true | _ -> false)
  and pdir p v = function Directory (p', v', _) when p = p' && v = v' -> true | _ -> false
  in
  List.fold_left
    (fun acc diff ->
     match categorise diff with
     | Key _ when has_key acc -> invalid_arg "only one key allowed"
     | Directory (p, v, d) as ele ->
        (match List.partition (pdir p v) acc with
         | [Directory (_, _, ds)], others -> Directory (p, v, d @ ds) :: others
         | [], others -> ele :: others
         | _, _ -> invalid_arg "unexpected thing here")
     | x -> x :: acc)
    [] diffs

let bind a f = match a with
  | `Ok x    -> f x
  | `Error e -> `Error e
let (>>=)    = bind
let fail e   = `Error e
let return r = `Ok r
let guard pred err = if pred then return () else fail err

let verify repo component =
  let id_of_sig (id, _, _) = id in
  match component with
  | Key (id, diff) ->
     (* TODO: pubkey change OR role downgrade: repository must not contain old invalid sigs *)
     (* TODO: key removal *)
     (* first, ensure the repo has the old key in store (if around)! *)
     let repo, old = match Repository.read_key repo id with
       | Some p -> (Repository.add_key repo p, true)
       | None -> (repo, false)
     in
     let repo' = apply repo diff in
     (match Repository.read_key repo' id with
      | None -> fail ("could not read key " ^ id)
      | Some key ->
         let repo' = Repository.load_keys repo' (List.map id_of_sig key.Publickey.signatures) in
         if Repository.verify_key repo' key then
           return (repo', key)
         else
           fail ("could not verify key " ^ id)) >>= fun (repo', key) ->
     let repo' = Repository.add_key repo' key in
     (if not old then
        (guard (key.Publickey.counter <> 0L) ("new key " ^ id ^ " where counter is not 0") >>= fun () ->
         guard (Layout.unique_keyid (Repository.all_keyids repo) key.Publickey.keyid)
               ("new key " ^ id ^ " whoise name is not unique"))
      else
        return ()) >>= fun () ->
     return repo'

  | Delegate (package, diff) ->
     (* TODO: ensure everything below delegate is properly signed (validids might have changed) *)
     let repo' = apply repo diff in
     (match Repository.read_delegate repo' package with
      | None -> fail ("missing delegate file for " ^ package)
      | Some d ->
         let repo' = Repository.load_keys repo' (List.map id_of_sig d.Delegate.signatures) in
         if Repository.verify_delegate repo' d then
           return (repo', d)
         else
           fail ("could not verify delegate " ^ package)) >>= fun (repo', del) ->
     (match Repository.read_delegate repo package with
      | None ->
         guard (del.Delegate.counter = 0L) ("new delegate " ^ package ^ " where counter is not 0") >>= fun () ->
         guard (Layout.unique_data (Repository.all_delegates repo) del.Delegate.name)
               ("new delegate " ^ package ^ " whose name is not unique")
      | Some odel ->
         (* counter increased *)
         guard (del.Delegate.counter > odel.Delegate.counter) ("counter of delegate " ^ package ^ " does not increase") >>= fun () ->
         (* new signed by some of the old guys *)
         guard (Repository.verify_delegate repo' ~validids:odel.Delegate.validids del) ("could not verify delegate " ^ package ^ " using old ids")) >>= fun () ->
     return repo'

  | Directory (p, v, diffs) ->
     let repo' = List.fold_left apply repo diffs in
     (match Repository.read_delegate repo' p with
      | None -> fail ("missing delegate for " ^ p)
      | Some d -> return d) >>= fun delegate ->
     (match Repository.read_checksum repo' v with
       | None -> fail ("no checksum file for " ^ v)
       | Some x -> return x) >>= fun cs ->
     (match Repository.read_checksum repo v with
       | None -> guard (cs.Checksum.counter = 0L) ("new checksum " ^ v ^ " where counter is not 0")
       | Some ocs -> guard (cs.Checksum.counter > ocs.Checksum.counter) ("counter of checksum " ^ v ^ " does not increase")) >>= fun () ->
     let repo' = Repository.load_keys repo' (List.map id_of_sig cs.Checksum.signatures) in
     guard (Repository.verify_checksum repo' delegate cs) ("could not verify checksum signatures " ^ v) >>= fun () ->
     guard (Checksum.checksums_equal (Repository.compute_checksum repo v) cs) ("could not validate checksum contents " ^ v) >>= fun () ->
     return repo'

let verify_patch repo patch =
  List.fold_left
    (fun res p -> match res with
                  | `Ok r -> verify r p
                  | `Error e -> `Error e)
    (`Ok repo)
    (* TODO: compare (first key, then delegates, dirs) *)
    (List.sort compare patch)

let verify_diff repo data =
  let diffs = Diff.to_diffs data in
  let comp = diffs_to_components diffs in
  verify_patch repo comp
*)
