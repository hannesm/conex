
module Keys = struct
  open Cmdliner

  let docs = "COMMON OPTIONS"

  let id_c =
    let parse s =
      if Conex_utils.String.is_ascii ~p:(function '_' | '.' | '=' | '-' -> true | _ -> false) s then
        `Ok s
      else
        `Error "invalid identifier (valid: A-Za-z0-9)"
    in
    (parse, fun ppf s -> Format.pp_print_string ppf s)

  let id =
    let doc = "Use a specific identity (not needed unless you have more than one identity)." in
    Arg.(value & opt (some id_c) None & info ["id"] ~docs ~doc)

  let ignore_missing =
    let doc = "Non-strict verification mode.  Packages where no release is signed are ignored." in
    Arg.(value & flag & info [ "ignore-missing" ; "nostrict" ] ~doc)

  let quorum =
    let doc = "The quorum of maintainers used for verification of the repository" in
    Arg.(value & opt (some int) None & info [ "quorum" ] ~doc)

  let repo =
    let doc = "Repository base directory (defaults to cwd)" in
    Arg.(value & opt (some dir) None & info [ "r" ; "repository" ] ~docs ~doc)

  let anchors =
    let doc = "Trust anchors (Hex encoded hashes, seperated by ',').  Can be repeated." in
    Arg.(value & opt_all string [] & info [ "t" ; "trust-anchors" ] ~doc)

  let dry =
    let doc = "Dry run. Do not write anything to persistent storage." in
    Arg.(value & flag & info ["dry-run"] ~docs ~doc)

  let force =
    let doc = "Force." in
    Arg.(value & flag & info ["force"] ~docs ~doc)

  let no_incr =
    let doc = "Do not increment counter." in
    Arg.(value & flag & info ["no-incr"] ~docs ~doc)

  let root =
    let doc = "Root filename, defaults to root" in
    Arg.(value & opt string "root" & info ["root"] ~docs ~doc)

  let package =
    let doc = "Package name" in
    Arg.(value & opt (some id_c) None & info [ "pkg" ] ~doc)

  let incremental =
    let doc = "Incremental verification mode" in
    Arg.(value & flag & info [ "incremental" ] ~doc)

  let dir =
    let doc = "Directory which is verified." in
    Arg.(value & opt (some string) None & info [ "dir" ] ~doc)

  let patch =
    let doc = "Patch file which is verified." in
    Arg.(value & opt (some file) None & info [ "patch" ] ~doc)

  let no_opam =
    let doc = "Do not verify opam repository layout" in
    Arg.(value & flag & info [ "no-opam" ] ~doc)
end

let repo ?(rw = false) repodir =
  let dir = match repodir with
    | None -> Unix.getcwd ()
    | Some x -> x
  in
  if rw
  then Conex_unix_provider.fs_provider dir
  else Conex_unix_provider.fs_ro_provider dir

let valid a =
  let ta = List.fold_left
      (fun acc str -> match Conex_resource.Digest.of_string str with
         | Error _ -> Printf.printf "ignoring malformed trust anchor %s" str ; acc
         | Ok dgst -> dgst :: acc)
      [] (List.flatten (List.map (Conex_utils.String.cuts ',') a))
  in
  let valid digest _ = List.exists (Conex_resource.Digest.equal digest) ta in
  valid

let msg_to_cmdliner = function
  | Ok () -> `Ok ()
  | Error m -> `Error (false, m)
