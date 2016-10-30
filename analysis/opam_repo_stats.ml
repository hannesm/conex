
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

command line:
 git log --no-merges --topo-order --pretty=format:%H c955aa6889514868317c92701a1774492057284d..HEAD
 *)

module SM = Map.Make(String)
module S = Set.Make(String)

let handle_one base filename github mail maps =
  Format.fprintf Format.std_formatter "handle_one %s@." filename ;
  let content = Persistency.read_file
      (Filename.concat base (Filename.concat "diffs" (filename ^ ".diff")))
  in
  let diffs = Diff.to_diffs content in
  let comps = Patch.diffs_to_components diffs in
  List.fold_left (fun (invalid, valid) d ->
      Format.fprintf Format.std_formatter "handling %a@." Patch.pp_component d ;
      match d with
      | Patch.Dir (p, _, _)
      | Patch.OldDir (p, _, _) ->
        let ms =
          match try Some (Sexplib.Sexp.load_sexp (Filename.concat base (Filename.concat "packages" (Filename.concat p "maintainers.sexp")))) with _ -> None with
          | Some (Sexplib.Type.List xs) -> List.map Sexplib.Conv.string_of_sexp xs
          | _ -> []
        in
        let (map, ret) =
          if List.mem mail ms then
            (valid, fun v -> invalid, v)
          else
            (invalid, fun i -> i, valid)
        in
        let mymails, mails, pl =
          if SM.mem github map then
            SM.find github map
          else
            S.empty, S.empty, S.empty
        in
        ret (SM.add github (S.add mail mymails, S.union (S.of_list ms) mails, S.add p pl) map)
      | _ -> Printf.printf "ignoring\n"; (invalid, valid))
    maps comps

(* what do I want in the end? *)
(* map GitHub ID -> (email, package list) [of invalid pushes] *)
let handle_prs dir =
  let base = Filename.concat dir "prs" in
  let prs = Persistency.collect_dir base in
  let total = List.length prs in
  let (invalid, valid), _ = List.fold_left
      (fun (map, i) pr ->
         Printf.printf "%d/%d\n%!" i total ;
         let data = Persistency.read_file (Filename.concat base pr) in
         let eles = Astring.String.cuts ~sep:" " data in
         let cid = List.nth eles 0
         and gid = List.nth eles 1
         and mail = List.nth eles 2
         in
         handle_one dir cid gid mail map, succ i)
      ((SM.empty, SM.empty), 0)
      prs
  in
  let print p_m map =
    let bindings = SM.bindings map in
    let sorted = List.sort (fun (_, (_, _, x)) (_, (_, _, y)) -> compare (S.cardinal x) (S.cardinal y)) bindings in
    List.iter (fun (gid, (mymails, others, pl)) ->
        Printf.printf "%s (%s%s): %s\n" gid
          (String.concat ", " (S.elements mymails))
          (if p_m then " allowed: " ^ String.concat ", " (S.elements others) else "")
          (String.concat ", " (S.elements pl)))
      sorted
  in
  print_endline "" ;
  print_endline "RESULTS:";
  print_endline "invalid" ;
  print true invalid ;
  print_endline "" ;
  print_endline "valid" ;
  print false valid ;
  print_endline "" ;
  print_endline "github id to mail" ;
  let users =
    SM.fold (fun k (m, _, _) acc -> SM.add k m acc)
  in
  let u = users invalid SM.empty in
  let u = users valid u in
  let bind = SM.bindings u in
  List.iter
    (fun (g, m) -> Printf.printf "%s -> %s\n" g (String.concat ", " (S.elements m)))
    (List.sort (fun (a, _) (b, _) -> String.compare a b) bind)

let () =
  match Sys.argv with
  | [| _ ; prs |] -> handle_prs prs
  | _ -> invalid_arg "expecting exactly one argument"
