
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

let handle_one base filename github mail map =
  let content = Persistency.read_file
      (Filename.concat base (Filename.concat "diffs" (filename ^ ".diff")))
  in
  let diffs = Diff.to_diffs content in
  let comps = Patch.diffs_to_components diffs in
  List.fold_left (fun map d ->
      Format.fprintf Format.std_formatter "handling %a@." Patch.pp_component d ;
      match d with
      | Patch.Dir (p, _, _)
      | Patch.OldDir (p, _, _) ->
        let ms =
          match try Some (Sexplib.Sexp.load_sexp (Filename.concat base (Filename.concat "packages" (Filename.concat p "maintainers.sexp")))) with _ -> None with
          | Some (Sexplib.Type.List xs) -> List.map Sexplib.Conv.string_of_sexp xs
          | _ -> []
        in
        if List.mem mail ms then
          map
        else
          begin
            let pl =
              if SM.mem github map then
                snd (SM.find github map)
              else
                S.empty
            in
            SM.add github (mail, S.add p pl) map
          end
      | _ -> Printf.printf "ignoring\n"; map)
    map comps

(* what do I want in the end? *)
(* map GitHub ID -> (email, package list) [of invalid pushes] *)
let handle_prs dir =
  let base = Filename.concat dir "prs" in
  let prs = Persistency.collect_dir base in
  let total = List.length prs in
  let map, _ = List.fold_left
      (fun (map, i) pr ->
         Printf.printf "%d/%d\n%!" i total ;
         let data = Persistency.read_file (Filename.concat base pr) in
         let eles = Astring.String.cuts ~sep:" " data in
         let cid = List.nth eles 0
         and gid = List.nth eles 1
         and mail = List.nth eles 2
         in
         handle_one dir cid gid mail map, succ i)
      (SM.empty, 0)
      prs
  in
  let bindings = SM.bindings map in
  let sorted = List.sort (fun (_, (_, x)) (_, (_, y)) -> compare (S.cardinal x) (S.cardinal y)) bindings in
  List.iter (fun (gid, (mail, pl)) ->
      Printf.printf "%s (%s): %s\n" gid mail (String.concat ", " (S.elements pl)))
    sorted

let () =
  match Sys.argv with
  | [| _ ; prs |] -> handle_prs prs
  | _ -> invalid_arg "expecting exactly one argument"
