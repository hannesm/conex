open Conex_utils
open Conex_resource

let ignore_pr = [
  "5190" ; (* added dev-repo to github packages (AltGr) *)
  "7159" ; "7212" ; (* - opam builder fixes (gasche) *)
  "20" ; (* descr updates (vbmithr) *)
  "2593" ; (* findlib files (samoht) *)
  "32" ;  (* fix compilation with 4.00.0 (tuong) *)
  "516" ; (* replace %{make}% with make (avsm) *)
  "538" ; (* Generating homepage tags from github archive links (AltGr) *)
  "30" ; (* move .0 to real version (vbmithr) *)
  (* some are too big and not worth it *)
  "1307" ; "5140" ; "6112" ; "1240" ; "1"
]

(*
Initial repo style with packages/foo.x.y

commit 13f3ab9dd3129306b8d062a06f0373a41937dc12
Merge: 308c03a 6008baf
Author: Thomas Gazagnaire <thomas@gazagnaire.org>
Date:   Fri Aug 24 06:01:39 2012 -0700

Initial repo style with packages/foo/foo.x.y

commit 0f0e610f6499bdf0151e4170411b4f05e4d076d4
Author: Louis Gesbert <louis.gesbert@ocamlpro.com>
Date:   Thu Nov 21 13:30:25 2013 +0100

    Reorganise packages into sub-directories
 *)


(*
several useful maps should be part of the output:
<github-id> -> mail addresses
<package> -> {<committer> -> [bool * PR] }
 *)

let handle_one io base commit pr github mail maps =
  Logs.app (fun m -> m "handle_one (%s) %s@." pr commit) ;
  if List.mem pr ignore_pr then
    (Logs.app (fun m -> m "ignored PR") ; Ok maps)
  else
  Conex_persistency.read_file
    (Filename.concat base (Filename.concat "diffs" (commit ^ ".diff"))) >>= fun content ->
  let diffs = Conex_diff.to_diffs content in
  let _ids, _auths, _rels, packages = Conex_diff.diffs_to_components diffs in
  let maps =
    M.fold (fun pn _pvs (github_map, package_map) ->
        Logs.app (fun m -> m "handling %s" pn) ;
        let github_map =
          let vals = if M.mem github github_map then M.find github github_map else S.empty in
          M.add github (S.add mail vals) github_map
        in
        if Sys.file_exists (Filename.concat base (Filename.concat "packages" pn)) then
          let ms = match Conex_io.read_authorisation io pn with
            | Error e ->
              Logs.err (fun m -> m "%a while reading authorisation %s" Conex_io.pp_r_err e pn) ;
              S.empty
            | Ok auth -> auth.Authorisation.authorised
          in
          let valid = S.mem mail ms in
          let package_map =
            let vals = try M.find pn package_map with Not_found -> M.empty in
            let entries = try M.find mail vals with Not_found -> [] in
            let maybe =
              if List.exists (fun (_, pr') -> pr = pr') entries then
                entries
              else
                (valid, pr) :: entries
            in
            M.add pn (M.add mail maybe vals) package_map
          in
          (github_map, package_map)
        else
          (Printf.printf "ignoring deleted package %s\n" pn ;
           (github_map, package_map)))
      packages maps
  in
  Ok maps

(* what do I want in the end? *)
(* map GitHub ID -> (email, package list) [of invalid pushes] *)
let handle_prs dir =
  Conex_unix_provider.fs_ro_provider dir >>= fun io ->
  let base = Filename.concat dir "prs" in
  Conex_persistency.collect_dir base >>= fun prs ->
  let total = List.length prs in
  let (github_map, package_map), _ = List.fold_left
      (fun (map, i) pr ->
         Logs.app (fun m -> m "%d/%d" i total) ;
         match Conex_persistency.read_file (Filename.concat base pr) with
         | Ok data ->
           let eles = Astring.String.cuts ~sep:" " data in
           let cid = List.nth eles 0
           and pr = List.nth eles 1
           and gid = List.nth eles 2
           and mail = String.trim (List.nth eles 3)
           in
           (match handle_one io dir cid pr gid mail map with
            | Ok x -> (x, succ i)
            | Error e ->
              Logs.warn (fun m -> m "error while handling %s: %s" pr e) ;
              (map, succ i))
         | Error e ->
           Logs.err (fun m -> m "error %s while reading pr %s" e pr) ;
           (map, succ i))
      ((M.empty, M.empty), 0)
      prs
  in
  Logs.app (fun m -> m "github id to mail") ;
  M.iter (fun k v -> Logs.app (fun m -> m "(%s (%s))" k (String.concat " " (S.elements v)))) github_map ;
  Logs.app (fun m -> m "changes") ;
  M.iter (fun k v ->
      Logs.app (fun m -> m "package %s" k) ;
      let bindings = M.bindings v in
      (* github id, [valid,pr] *)
      let good, bad = List.partition (fun (_, xs) -> match xs with | (true, _)::_ -> true | _ -> false) bindings in
      let goodprs = List.map (fun (id, xs) -> id, List.map snd xs) good in
      let badprs = List.map (fun (id, xs) -> id, List.map snd xs) bad in
      let p (id, prs) = Printf.sprintf "%s (%s)" id (String.concat ", " prs) in
      Logs.app (fun m -> m "GOOD:\n  %s\nBAD\n  %s\n\n"
                   (String.concat "\n  " (List.map p goodprs))
                   (String.concat "\n  " (List.map p badprs))))
    package_map ;
  Ok ()

let () =
  match Sys.argv with
  | [| _ ; prs |] -> (match handle_prs prs with
      | Ok () -> ()
      | Error e -> Logs.err (fun m -> m "error %s" e))
  | _ -> invalid_arg "expecting exactly one argument"
