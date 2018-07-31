open Conex_utils
open Conex_resource

module IO = Conex_io

module Make (L : LOGS) (C : Conex_verify.S) = struct

  let verify_root ?(valid = fun _ _ -> false) io filename =
    L.debug (fun m -> m "verifying root %a" pp_name filename) ;
    err_to_str IO.pp_r_err (IO.read_root io filename) >>= fun (root, warn) ->
    List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) warn ;
    (* verify signatures *)
    let sigs, errs =
      C.verify (Root.wire_raw root) root.Root.keys root.Root.signatures
    in
    List.iter (fun e -> L.warn (fun m -> m "%a" Conex_verify.pp_error e)) errs ;
    let s = S.fold (fun id acc ->
        match M.find id root.Root.keys with
        | None ->
          L.info (fun m -> m "couldn't find key for %a" pp_id id) ;
          acc
        | Some key ->
          let digest = Key.keyid C.raw_digest key in
          if valid digest id then
            S.add id acc
          else begin
            L.info (fun m -> m "%a (%a) is not a valid root key"
                       pp_id id Digest.pp digest) ;
            acc
          end)
        sigs S.empty
    in
    match M.find "root" root.Root.roles with
    | None -> Error "no 'root' role in root file"
    | Some e ->
      if Expression.eval e Digest_map.empty s
      then Ok (Conex_repository.create root)
      else Error "couldn't validate root role"

  let targets_cache = ref M.empty

  let verify_targets io repo id =
    L.debug (fun m -> m "verifying target %a" pp_id id) ;
    match M.find id !targets_cache with
    | Some targets ->
      L.debug (fun m -> m "found in cache") ;
      Ok targets
    | None ->
      let root = Conex_repository.root repo in
      err_to_str IO.pp_r_err (IO.read_targets io root id) >>= fun (targets, warn) ->
      List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) warn ;
      let s, es =
        Targets.(C.verify (wire_raw targets) targets.keys targets.signatures)
      in
      List.iter (fun e -> L.warn (fun m -> m "%a" Conex_verify.pp_error e)) es ;
      (* assumes that expression is valid using only target-local (keys and) signatures! *)
      if Expression.eval targets.Targets.valid Digest_map.empty s then begin
        targets_cache := M.add id targets !targets_cache ;
        Ok targets
      end else
        Error ("couldn't validate expression in targets " ^ id)

  let compare_with_disk ignore_missing io repo =
    let non_strict_res = function
      | `Only_on_disk p ->
        (* we have sth like foo/foo.0/<file>, and check that targets doesn't
           have anything in foo/  *)
        begin match p with
          | [] -> L.warn (fun m -> m "shouldn't happen, empty path") ; false (* unclear, should not happen *)
          | pkg::_ -> not (Tree.(is_empty (sub [pkg] (Conex_repository.targets repo))))
        end
      | _ -> true
    in
    IO.compute_checksum_tree io C.raw_digest >>= fun on_disk ->
    let errs = Conex_repository.validate_targets repo on_disk in
    List.iter (fun r ->
        L.warn (fun m -> m "%a" Conex_repository.pp_res r))
      errs ;
    match ignore_missing, errs with
    | _, [] -> Ok ()
    | false, _ -> Error "comparison failed"
    | true, _ -> match List.filter non_strict_res errs with
      | [] -> Ok ()
      | _ -> Error "non-strict comparison failed"

  let collect_targets io repo keyrefs =
    M.fold (fun id (dgst, epoch) (dm, id_d, targets) ->
        match verify_targets io repo id with
        | Error msg ->
          L.warn (fun m -> m "couldn't load or verify target %a: %s" pp_id id msg) ;
          (dm, id_d, targets)
        | Ok target ->
          let keys =
            M.fold (fun id key m -> M.add id (Key.to_string key) m)
              target.Targets.keys M.empty
          in
          match Expression.hash C.raw_digest keys target.Targets.valid with
          | Error e ->
            L.warn (fun m -> m "%s" e) ;
            (dm, id_d, targets)
          | Ok valid_h ->
            if
              Uint.compare target.Targets.epoch epoch = 0 &&
              Digest.equal valid_h dgst
            then
              (* now we add as _id_ (of our keyref) the epoch and dgst to dm *)
              let id_d' = M.add id (dgst, epoch) id_d in
              (Digest_map.add dgst (id, epoch) dm, id_d', target :: targets)
            else begin
              L.warn (fun m -> m "dropping %a since epoch (%a vs %a) or digest mismatch"
                         pp_id id Uint.pp epoch Uint.pp target.Targets.epoch) ;
              (dm, id_d, targets)
            end)
      keyrefs (Digest_map.empty, M.empty, [])

  let verify_one io repo path expr terminating =
    let keyrefs = Expression.keys M.empty expr in
    let dm, id_d, targets = collect_targets io repo keyrefs in
    if Expression.eval expr dm S.empty then begin
      let tree = Conex_repository.targets repo in
      let tree' = Conex_repository.collect_and_validate_targets ~tree id_d path expr targets in
      let repo' = Conex_repository.with_targets repo tree' in
      let delegations =
        if terminating then begin
          L.debug (fun m -> m "terminating delegation, not inspecting further") ;
          []
        end else
          Conex_repository.collect_and_validate_delegations id_d path expr targets
      in
      (repo', delegations)
    end else begin
      let pp_id_ep ppf (id, epoch) = Format.fprintf ppf "%a (#%s)" pp_id id (Uint.to_string epoch) in
      L.warn (fun m -> m "expression %a does not eval to true with %a"
                 Expression.pp expr (Digest_map.pp pp_id_ep) dm) ;
      (repo, [])
    end

  let verify ?(ignore_missing = false) io repo =
    match Conex_repository.janitor_delegation repo with
    | None -> Error "no delegation for janitors"
    | Some (expr, term, supp) ->
      (* queue is mutable, not thread safe, raises on pop when empty
         - it does not leave the scope here
         - pop is guarded by the is_empty *)
      let q = Queue.create () in
      (* termination argument is tricky here, but:
         - delegations may delegate (unless terminating = true) any subpath
            (subpath ~parent:p p is false forall p!)
         - maybe path should be limited in length (2?3?)
      *)
      let rec process_delegation repo =
        if Queue.is_empty q
        then repo
        else begin
          let (path, expr, terminating, _supp) = Queue.pop q in
          let repo', dels = verify_one io repo path expr terminating in
          List.iter (fun (p, e, t, s) ->
              L.debug (fun m -> m "pushing delegation path %a expr %a terminating %b s %a"
                         pp_path p Expression.pp e t S.pp s) ;
              Queue.push (p, e, t, s) q)
            dels ;
          process_delegation repo'
        end
      in
      Queue.push (root, expr, term, supp) q ;
      let repo' = process_delegation repo in
      let pp_t ppf (dgst, len, s) =
        Format.fprintf ppf "digest %a len %s supporters %a@."
          Digest.pp dgst (Uint.decimal len) S.pp s
      in
      L.debug (fun m -> m "end of verification, targets tree is %a"
                  (Tree.pp pp_t) (Conex_repository.targets repo')) ;
      compare_with_disk ignore_missing io repo'

  let verify_diffs root io newio diffs =
    err_to_str IO.pp_r_err (IO.read_root io root) >>= fun (old_root, warn) ->
    List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) warn ;
    err_to_str IO.pp_r_err (IO.read_root newio root) >>= fun (new_root, warn') ->
    List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) warn' ;
    guard (path_equal old_root.Root.keydir new_root.Root.keydir)
      "old and new key directories are differrent" >>= fun () ->
    Conex_diff.ids root new_root.Root.keydir diffs >>= fun (r, ids) ->
    L.debug (fun m -> m "root is modified? %b, ids %a" r S.pp ids) ;
    (match r, Uint.compare old_root.Root.counter new_root.Root.counter with
     | _, 1 -> Error "root counter decremented"
     | false, 0 -> Ok ()
     | true, -1 -> Ok ()
     | true, 0 -> Error "root counter same, expected to increase"
     | r, c -> invalid_arg ("shouldn't happen, r is " ^ string_of_bool r ^ ", compare " ^ string_of_int c)) >>= fun () ->
    S.fold (fun id acc ->
        acc >>= fun () ->
        match IO.read_targets io old_root id, IO.read_targets newio new_root id with
        | Error _, Ok _ -> Ok ()
        | Error _, Error e -> err_to_str IO.pp_r_err (Error e)
        | Ok _, Error e -> err_to_str IO.pp_r_err (Error e) (* TODO allow delete? *)
        | Ok (t, _), Ok (t', _) ->
          match
            Uint.compare t.Targets.epoch t'.Targets.epoch,
            Uint.compare t.Targets.counter t'.Targets.counter
          with
          | 0, -1 -> Ok ()
          | -1, _ -> Ok ()
          | 1, _ -> Error ("epoch of " ^ id ^ " is moving backwards")
          | 0, 0 -> Error ("counter and epoch of " ^ id ^ " same")
          | 0, 1 -> Error ("counter of " ^ id ^ " is moving backwards")
          | _ -> invalid_arg "unexpected")
      ids (Ok ())
end
