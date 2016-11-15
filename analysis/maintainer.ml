#!/usr/bin/env ocaml

#use "topfind"
#require "bigarray"
#require "opam-format"
#require "opam-file-format"
#require "astring"

module S = Set.Make(String)

let find_m s = match Astring.String.cut ~sep:"<" s with
  | None -> s
  | Some (_, r) -> match Astring.String.cut ~sep:">" r with
    | Some (l, _) -> l
    | None -> invalid_arg ("cannot parse maintainer " ^ s)

let replacements = [
  " [at] ", "@" ;
  "[at]", "@" ;
  " at ", "@" ;
  " (at) ", "@" ;
  "buenzl i", "buenzli"
]

let sanitize_mail str =
  let escape1 sep replacement data =
    let pieces = Astring.String.cuts ~sep data in
    String.concat replacement pieces
  in
  List.fold_left
    (fun s (c, r) -> escape1 c r s)
    str replacements

let maintainers pr =
  (* contact@ocamlpro.com -- good if tag org:ocamlpro is around (opam lint does this)
    opam-devel@lists.ocaml.org *)
  let filename = Filename.concat pr "opam" in
  let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string filename)) in
  let ms = S.of_list (List.map sanitize_mail (List.map find_m (OpamFile.OPAM.maintainer opam))) in
  let ms = S.remove "opam-devel@lists.ocaml.org" ms in
  if S.mem "contact@ocamlpro.com" ms then
    if List.mem "org:ocamlpro" (OpamFile.OPAM.tags opam) then
      ms
    else
      S.remove "contact@ocamlpro.com" ms
  else
    ms

let collect_dir dir =
  let open Unix in
  let dh = opendir dir in
  let next () = try Some (readdir dh) with End_of_file -> None in
  let rec doone acc = function
    | Some "." | Some ".." -> doone acc (next ())
    | Some s when Sys.is_directory (Filename.concat dir s) -> doone (s :: acc) (next ())
    | Some _ -> doone acc (next ())
    | None -> acc
  in
  let res = doone [] (next ()) in
  closedir dh ;
  res

let write path data =
  let fd = Unix.openfile path [Unix.O_WRONLY ; Unix.O_CREAT ; Unix.O_TRUNC] 0o644 in
  ignore(Unix.write fd (Bytes.of_string data) 0 (String.length data)) ;
  Unix.close fd

let np = ("", 0, 0)

let print_ms package ms path =
  let open OpamParserTypes in
  let file_contents = [
    Variable (np, "counter", Ident (np, "0")) ;
    Variable (np, "version", Ident (np, "0")) ;
    Variable (np, "name", Ident (np, package)) ;
    Variable (np, "authorised", List (np, List.map (fun n -> Ident (np, n)) ms))
  ]
  in
  let data = { file_contents ; file_name = "" } in
  let norm = OpamPrinter.Normalise.opamfile data in
  write (Filename.concat path "authorisation") norm

let mainloop repo =
  let base = Filename.concat repo "packages" in
  let packages = collect_dir base in
  let empty =
    List.fold_left (fun acc p ->
        let b = Filename.concat base p in
        let releases = collect_dir b in
        let maintainers = List.fold_left (fun s r -> S.union (maintainers (Filename.concat b r)) s) S.empty releases in
        match S.elements maintainers with
        | [] -> S.add p acc
        | xs -> print_ms p xs b ; acc)
      S.empty
      packages
  in
  let empties = List.sort String.compare (S.elements empty) in
  let fd = Unix.openfile (Filename.concat base "unmaintained.sexp") [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let str = Printf.sprintf "(%s)" (String.concat "\n" empties) in
  ignore(Unix.write fd (Bytes.of_string str) 0 (String.length str)) ;
  Unix.close fd ;
  Printf.printf "%d packages without maintainer\n" (S.cardinal empty)

let () = match Sys.argv with
  | [| _ ; repo |] -> mainloop repo
  | _ -> invalid_arg "expecting one argument"
