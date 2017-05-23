open Conex_utils

(* this is the barebones verify with minimal dependencies
   (goal: cmdliner, opam-file-format, Unix, external openssl)
 *)

(* to be called by opam (see http://opam.ocaml.org/doc/2.0/Manual.html#configfield-repository-validation-command, https://github.com/ocaml/opam/pull/2754/files#diff-5f9ccd1bb288197c5cf2b18366a73363R312):

%{quorum}% - a non-negative integer (--quorum)
%{anchors}% - list of digests, separated by "," (--trust-anchors -- to be used in full verification)
%{root}% - the repository root (--repository)

(we need --strict and --no-strict [initially default])

two modes of operation (%{incremental}% will just be "true" or "false"):

-full
%{dir}% is only defined for a full update, and is the dir to verify (--dir)

-incremental
%{patch}% - path to a patch (to be applied with -p1, generated by diff -ruaN dir1 dir2) (--patch)

exit code success = 0, failure otherwise

example:

repository-validation-command: [
   "conex" "--root" "%{root}%" "--trust-anchors" "%{anchors}%" "--patch" "%{patch}%"
]

> cat conex
#!/bin/bash -ue
echo "$*"
true

 *)

let err_to_cmdliner = function
  | Ok _ -> `Ok ()
  | Error m -> `Error (false, m)

module IO = Conex_io

module VERIFY (L : LOGS) (V : Conex_verify.S) = struct

  module C = Conex.Make(L)(V)

  let verify_diff io repo patch valid ignore_missing =
    Conex_unix_persistency.read_file patch >>= fun x ->
    let newio, diffs = Conex_diff_provider.apply_diff io x in
    C.verify_snapshot newio repo >>= fun () ->
    C.verify_patch ~ignore_missing ~valid repo io newio diffs >>= fun _ ->
    let ws = L.warn_count () in
    Printf.printf "verification successfull with %d warnings\n" ws ;
    Ok ()

  let verify_full io repo valid ignore_missing =
    C.verify_janitors ~valid io repo >>= fun repo ->
    C.verify_snapshot io repo >>= fun () ->
    C.verify_ids io repo >>= fun repo ->
    IO.packages io >>= fun packages ->
    foldS (C.verify_package ~ignore_missing io) repo packages >>= fun _ ->
    let ws = L.warn_count () in
    Printf.printf "verification of %d packages successfull with %d warnings\n"
      (S.cardinal packages - ws) ws ;
    Ok ()


  let verify_it repodir quorum anchors incremental dir patch nostrict =
    let ta = Conex_opts.convert_anchors anchors in
    let valid id (_, digest) =
      if S.mem digest ta then
        (L.debug (fun m -> m "accepting ta %s" id) ; true)
      else
        (L.debug (fun m -> m "rejecting ta %s" id) ; false)
    in
    let repo = Conex_repository.repository ?quorum V.digest () in
    match repodir, incremental, patch, dir with
    | Some repodir, true, Some p, None ->
      Conex_unix_provider.fs_ro_provider repodir >>= fun io ->
      L.debug (fun m -> m "repository %a" Conex_io.pp io) ;
      verify_diff io repo p valid nostrict
    | _, false, None, Some d ->
      Conex_unix_provider.fs_ro_provider d >>= fun io ->
      L.debug (fun m -> m "repository %a" Conex_io.pp io) ;
      verify_full io repo valid nostrict
    | None, _, _, _ -> Error "--repo is required"
    | _ -> Error "invalid combination of incremental, patch and dir"
end

let doc = "Verify a signed community repository"
and man = [
  `S "DESCRIPTION" ;
  `P "$(tname) verifies a cryptographically signed community repository" ;
  `P "Two verification modes are supported: $(b,initial) and $(b,incremental)." ;
  `P "During $(b,initial) verification, the trust is rooted in $(b,--anchors), and the repository $(b,--dir) is verified." ;
  `P "In $(b,incremental) mode, an existing trusted repository is used as trust root, and a provided $(b,--patch) is verified: signed and monotonicity is preserved.";
]
