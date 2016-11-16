open Conex_core


let ignore_pr = [
  "5190" ; (* added dev-repo to github packages (AltGr) *)
  "7159" ; "7212" ; (* - opam builder fixes (gasche) *)
  "20" ; (* descr updates (vbmithr) *)
  "2593" ; (* findlib files (samoht) *)
  "32" ;  (* fix compilation with 4.00.0 (tuong) *)
  "516" ; (* replace %{make}% with make (avsm) *)
  "538" ; (* Generating homepage tags from github archive links (AltGr) *)
  "30" ; (* move .0 to real version (vbmithr) *)
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


let get_authorised data =
  let open OpamParserTypes in
  let authorised =
    List.find
      (function Variable (_, s, _) when s = "authorised" -> true | _ -> false)
      data.file_contents
  in
  match authorised with
  | Variable (_, _, List (_, vs)) -> List.map (function String (_, id) -> id | _ -> invalid_arg "unexpected" ) vs
  | _ -> invalid_arg "unexpected"

let read_auth base pkgname =
  let path = Conex_opam_layout.authorisation_path pkgname in
  let file = Filename.concat base (path_to_string path) in
  if Sys.file_exists file then
    let data = Persistency.read_file file in
    let parsed = OpamParser.string data "" in
    get_authorised parsed
  else
    (Printf.printf "couldn't find authorisation for %s\n%!" file ;
     [])

(*
several useful maps should be part of the output:
<github-id> -> mail addresses
<package> -> {<committer> -> [bool * PR] }
 *)

let handle_one base commit pr github mail maps =
  Format.fprintf Format.std_formatter "handle_one (%s) %s@." pr commit ;
  if List.mem pr ignore_pr then
    (Printf.printf "ignored PR" ; maps)
  else
  let content = Persistency.read_file
      (Filename.concat base (Filename.concat "diffs" (commit ^ ".diff")))
  in
  let diffs = Diff.to_diffs content in
  let comps = Patch.diffs_to_components diffs in
  List.fold_left (fun (github_map, package_map) d ->
      Format.fprintf Format.std_formatter "handling %a" Patch.pp_component d ;
      match d with
      | Patch.Dir (p, _, _)
      | Patch.OldDir (p, _, _) ->
        let github_map =
          let vals = if M.mem github github_map then M.find github github_map else S.empty in
          M.add github (S.add mail vals) github_map
        in
        if Sys.file_exists (Filename.concat base (Filename.concat "packages" p)) then
          let ms = read_auth base p in
          let valid = List.mem mail ms in
          let package_map =
            let vals =
              if M.mem p package_map then
                M.find p package_map
              else
                M.empty
            in
            let entries =
              if M.mem mail vals then
                M.find mail vals
              else
                []
            in
            let maybe =
              if List.exists (fun (_, pr') ->  pr = pr') entries then
                entries
              else
                (valid, pr) :: entries
            in
            M.add p (M.add mail maybe vals) package_map
          in
          (github_map, package_map)
        else
          (Printf.printf "ignoring deleted package %s\n" p;
           (github_map, package_map))
      | _ -> Printf.printf "ignoring\n"; (github_map, package_map))
    maps comps

(* what do I want in the end? *)
(* map GitHub ID -> (email, package list) [of invalid pushes] *)
let handle_prs dir =
  let base = Filename.concat dir "prs" in
  let prs = Persistency.collect_dir base in
  let total = List.length prs in
  let (github_map, package_map), _ = List.fold_left
      (fun (map, i) pr ->
         Printf.printf "%d/%d\n%!" i total ;
         let data = Persistency.read_file (Filename.concat base pr) in
         let eles = Astring.String.cuts ~sep:" " data in
         let cid = List.nth eles 0
         and pr = List.nth eles 1
         and gid = List.nth eles 2
         and mail = String.trim (List.nth eles 3)
         in
         handle_one dir cid pr gid mail map, succ i)
      ((M.empty, M.empty), 0)
      prs
  in
  print_endline "github id to mail" ;
  M.iter (fun k v -> Printf.printf "(%s (%s))\n" k (String.concat " " (S.elements v))) github_map ;
  print_endline "\nchanges" ;
  M.iter (fun k v ->
      Printf.printf "package %s\n" k ;
      let bindings = M.bindings v in
      (* github id, [valid,pr] *)
      let good, bad = List.partition (fun (_, xs) -> match xs with | (true, _)::_ -> true | _ -> false) bindings in
      let goodprs = List.map (fun (id, xs) -> id, List.map snd xs) good in
      let badprs = List.map (fun (id, xs) -> id, List.map snd xs) bad in
      let p (id, prs) = Printf.sprintf "%s (%s)" id (String.concat ", " prs) in
      Printf.printf "GOOD:\n  %s\nBAD\n  %s\n\n"
        (String.concat "\n  " (List.map p goodprs))
        (String.concat "\n  " (List.map p badprs)))
    package_map

let () =
  match Sys.argv with
  | [| _ ; prs |] -> handle_prs prs
  | _ -> invalid_arg "expecting exactly one argument"
