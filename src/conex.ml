open Conex_utils
open Conex_resource

module IO = Conex_io

let ( let* ) = Result.bind

module Make (L : LOGS) (C : Conex_verify.S) = struct

  let valid_ids valid sigs =
    Digest_map.fold (fun dgst id acc ->
        if valid dgst id
        then S.add id acc
        else begin
          L.info (fun m -> m "%a (%a) is not a valid root key"
                     pp_id id Digest.pp dgst) ;
          acc
        end)
      sigs S.empty

  let verify_root ?(valid = fun _ _ -> false) ?quorum io filename =
    L.debug (fun m -> m "verifying root %a" pp_name filename) ;
    let* root, warn = err_to_str IO.pp_r_err (IO.read_root io filename) in
    List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) warn ;
    (* verify signatures *)
    let sigs, errs =
      C.verify (Root.wire_raw root) root.Root.keys root.Root.signatures
    in
    List.iter (fun e -> L.warn (fun m -> m "%a" Conex_verify.pp_error e)) errs ;
    (* need to unique over keyids *)
    let ids = valid_ids valid sigs in
    let quorum_satisfied = match quorum with
      | None -> true
      | Some q -> q <= S.cardinal ids
    in
    match
      Expression.eval root.Root.valid Digest_map.empty ids,
      quorum_satisfied
    with
    | true, true -> Ok (Conex_repository.create root)
    | false, _ -> Error "couldn't validate root role"
    | _, false -> Error "provided quorum was not matched"

  let verify_timestamp io repo ~timestamp_expiry ~now =
    L.debug (fun m -> m "verifying timestamp") ;
    match Root.RM.find `Timestamp (Conex_repository.root repo).Root.roles with
    | None ->
      L.warn (fun m -> m "no timestamp role found in root") ;
      Ok ()
    | Some (Expression.Quorum (1, ks)) when Expression.KS.cardinal ks = 1 ->
      (* assume a single timestamp *)
      begin match Expression.KS.choose ks with
        | Local _ ->
          Error "only a single remote key expression allowed for timestamp"
        | Remote (id, dgst, epoch) ->
          let* ts, warn = err_to_str IO.pp_r_err (IO.read_timestamp io id) in
          List.iter (fun w -> L.warn (fun m -> m "%s" w)) warn ;
          let sigs, es =
            Timestamp.(C.verify (wire_raw ts) ts.keys ts.signatures)
          in
          List.iter (fun e -> L.warn (fun m -> m "%a" Conex_verify.pp_error e)) es ;
          match Digest_map.find dgst sigs with
          | Some id' when id_equal id id' && Uint.compare epoch ts.Timestamp.epoch = 0 ->
            (* AND also ctr increased if we had an on-disk version (for incremental verification) *)
            begin match timestamp_to_int64 ts.Timestamp.created with
              | Ok ts ->
                if ts >= Int64.sub now timestamp_expiry then
                  Ok ()
                else
                  Error "timestamp is no longer valid"
              | Error _ as e -> e
            end
          | _ -> Error "couldn't validate timestamp signature"
      end
    | _ ->
      Error "only a single key expression with quorum 1 allowed for timestamp"

  let targets_cache = ref M.empty

  let verify_targets io repo opam id =
    L.debug (fun m -> m "verifying target %a" pp_id id) ;
    match M.find id !targets_cache with
    | Some targets ->
      L.debug (fun m -> m "found in cache") ;
      Ok targets
    | None ->
      let root = Conex_repository.root repo in
      let* targets, warn =
        err_to_str IO.pp_r_err (IO.read_targets io root opam id)
      in
      List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) warn ;
      let sigs, es =
        Targets.(C.verify (wire_raw targets) targets.keys targets.signatures)
      in
      List.iter (fun e -> L.warn (fun m -> m "%a" Conex_verify.pp_error e)) es ;
      let s = Digest_map.fold (fun _ id acc -> S.add id acc) sigs S.empty in
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
    let* on_disk = IO.compute_checksum_tree io C.raw_digest in
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

  let collect_targets io repo opam keyrefs =
    M.fold (fun id (dgst, epoch) (dm, id_d, targets) ->
        match verify_targets io repo opam id with
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

  let verify_one io repo opam path expr terminating =
    let keyrefs = Expression.keys M.empty expr in
    let dm, id_d, targets = collect_targets io repo opam keyrefs in
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

  let verify ?(ignore_missing = false) io repo opam =
    match Conex_repository.maintainer_delegation repo with
    | None -> Error "no delegation for maintainers"
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
          let repo', dels = verify_one io repo opam path expr terminating in
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

  let verify_diffs root io newio diffs opam =
    let* old_root, warn = err_to_str IO.pp_r_err (IO.read_root io root) in
    List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) warn ;
    let* new_root, warn' = err_to_str IO.pp_r_err (IO.read_root newio root) in
    List.iter (fun msg -> L.warn (fun m -> m "%s" msg)) warn' ;
    let* () =
      guard (path_equal old_root.Root.keydir new_root.Root.keydir)
        "old and new key directories are differrent"
    in
    let* r, ids = Conex_diff.ids root new_root.Root.keydir diffs in
    L.debug (fun m -> m "root is modified? %b, ids %a" r S.pp ids) ;
    let* () =
      match Uint.compare old_root.Root.counter new_root.Root.counter with
      | 0 when r -> Error "root counter same, expected to increase"
      | 0 (* when not r *) -> Ok ()
      | x when x > 0 -> Error "root counter decremented"
      | _ (* when x < 0 *) -> Ok ()
    in
    S.fold (fun id acc ->
        let* () = acc in
        match IO.read_targets io old_root opam id, IO.read_targets newio new_root opam id with
        | Error _, Ok _ -> Ok ()
        | Error _, Error e -> err_to_str IO.pp_r_err (Error e)
        | Ok _, Error e -> err_to_str IO.pp_r_err (Error e) (* TODO allow delete? *)
        | Ok (t, _), Ok (t', _) ->
          match
            Uint.compare t.Targets.epoch t'.Targets.epoch,
            Uint.compare t.Targets.counter t'.Targets.counter
          with
          | 0, 0 -> Error ("counter and epoch of " ^ id ^ " same")
          | 0, y ->
            if y > 0 then
              Error ("counter of " ^ id ^ " is moving backwards")
            else (* y < 0 *)
              Ok ()
          | x, _ ->
            if x < 0 then
              Ok ()
            else (* x > 0 *)
              Error ("epoch of " ^ id ^ " is moving backwards"))
      ids (Ok ())
end
