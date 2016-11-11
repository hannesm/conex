open Conex_result
open Conex_core
open Conex_resource

open Common

module Mem = struct
  open Provider
  type tree = Leaf of string * string | Node of string * tree list

  let name = function Leaf (x, _) -> x | Node (x, _) -> x

  let find_e x es =
    try Some (List.find (fun n -> name n = x) es) with Not_found -> None

  let rec find t path =
    match t, path with
    | x, [p] when name x = p -> Ok x
    | _, [_] -> Error `NotFound
    | Node (_, xs), _::p'::ps ->
      (match find_e p' xs with
       | None -> Error `NotFound
       | Some x -> find x (p'::ps))
    | _ -> Error `NotFound

  let rec insert t path v =
    match t, path with
    (* leaf case, just add the leaf *)
    | Node (x, xs), _::ps::[] ->
      let l = Leaf (ps, v) in
      let ns = List.filter (fun n -> name n <> ps) xs in
      Node (x, l::ns)
    (* on the route towards.. move along *)
    | Node (x, xs), _::p'::ps ->
      let node =
        match find_e p' xs with
        (* need to create an intermediary node *)
        | None ->
          let n = Node (p', []) in
          insert n (p'::ps) v
        (* follow on *)
        | Some n -> insert n (p'::ps) v
      in
      let xs = List.filter (fun n -> name n <> p') xs in
      Node (x, node::xs)
    | _ -> invalid_arg "should not happen"

  let find_f t path = find t ("/"::path)

  let ins t path v = insert t ("/"::path) v

  let mem_provider () : Provider.t =
    let root = ref (Node ("/", [])) in
    let file_type path =
      find_f !root path >>= function
      | Leaf _ -> Ok File
      | Node _ -> Ok Directory
    and read path =
      find_f !root path >>= function
      | Leaf (_, v) -> Ok v
      | _ -> Error `NotFound
    and write path data =
      let r = ins !root path data in
      root := r
    and read_dir path =
      find_f !root path >>= function
      | Node (_, ch) -> Ok (List.map (function Node (x, _) -> `Dir x | Leaf (x, _) -> `File x) ch)
      | Leaf _ -> Error `NotFound
    and exists path =
      match find_f !root path with Ok _ -> true | Error _ -> false
    in
    { name = "mem" ; description = "Memory provider" ; file_type ; read ; write ;read_dir ; exists }
end

let perr =
  let module M = struct
    type t = Provider.err
    let pp ppf = function
      | `NotFound -> Format.pp_print_string ppf "not found"
      | `UnknownFileType _ -> Format.pp_print_string ppf "unknown file type"
    let equal a b = match a, b with
      | `NotFound, `NotFound -> true
      | `UnknownFileType _, `UnknownFileType _ -> true
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let ft =
  let module M = struct
    type t = file_type
    let pp ppf = function
      | File -> Format.pp_print_string ppf "file"
      | Directory -> Format.pp_print_string ppf "directory"
    let equal a b = match a, b with
      | File, File -> true
      | Directory, Directory -> true
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let it =
  let module M = struct
    type t = Provider.item
    let pp ppf = function
      | `File f -> Format.fprintf ppf "file %s" f
      | `Dir d -> Format.fprintf ppf "directory %s" d
    let equal a b = match a, b with
      | `File f, `File g -> f = g
      | `Dir a, `Dir b -> a = b
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let empty_p () =
  let open Provider in
  let p = Mem.mem_provider () in
  Alcotest.check Alcotest.bool "no 'foo' in empty mem store" false (p.exists ["foo"]) ;
  Alcotest.check (result Alcotest.string perr) "reading 'foo' in empty mem store" (Error `NotFound) (p.read ["foo"]) ;
  Alcotest.check (result ft perr) "file-type of 'foo' in empty mem store" (Error `NotFound) (p.file_type ["foo"]) ;
  Alcotest.check (result (Alcotest.list it) perr) "read_dir of 'foo' in empty mem store" (Error `NotFound) (p.read_dir ["foo"])

let basic_p () =
  let open Provider in
  let p = Mem.mem_provider () in
  p.write ["foo"] "bar" ;
  Alcotest.check Alcotest.bool "foo in basic store" true (p.exists ["foo"]) ;
  Alcotest.check Alcotest.bool "foo/bar not in basic store" false (p.exists ["foo";"bar"]) ;
  Alcotest.check Alcotest.bool "foobar not in basic store" false (p.exists ["foobar"]) ;
  Alcotest.check (result Alcotest.string perr) "foo contains bar in simple store" (Ok "bar") (p.read ["foo"]) ;
  p.write ["foo"] "barf" ;
  Alcotest.check (result Alcotest.string perr) "foo contains barf in simple store" (Ok "barf") (p.read ["foo"]) ;
  Alcotest.check (result (Alcotest.list it) perr) "read_dir of 'foo' in simple store" (Error `NotFound) (p.read_dir ["foo"])

let more_p () =
  let open Provider in
  let p = Mem.mem_provider () in
  p.write ["packages"; "foo"] "bar" ;
  Alcotest.check Alcotest.bool "packages in more store" true (p.exists ["packages"]) ;
  Alcotest.check Alcotest.bool "packages/foo in more store" true (p.exists ["packages"; "foo"]) ;
  Alcotest.check Alcotest.bool "packages/foo/bar not in more store" false (p.exists ["packages"; "foo"; "bar"]) ;
  p.write ["packages"; "foo2"] "bar2" ;
  Alcotest.check (result (Alcotest.list it) perr) "read_dir of 'packages' in more mem store"
    (Ok [ `File "foo2" ; `File "foo" ]) (p.read_dir ["packages"]) ;
  p.write ["packages"; "foo3"] "bar3" ;
  p.write ["packages"; "foo4"] "bar4" ;
  Alcotest.check (result (Alcotest.list it) perr) "read_dir of 'packages' in even more mem store"
    (Ok [ `File "foo4" ; `File "foo3" ; `File "foo2" ; `File "foo" ]) (p.read_dir ["packages"]) ;
  Alcotest.check (result (Alcotest.list it) perr) "read_dir of 'packages2' in even more mem store"
    (Error `NotFound) (p.read_dir ["packages2"]) ;
  Alcotest.check (result Alcotest.string perr) "foo contains bar in more store" (Ok "bar") (p.read ["packages";"foo"]) ;
  Alcotest.check (result Alcotest.string perr) "foo2 contains bar2 in more store" (Ok "bar2") (p.read ["packages"; "foo2"]) ;
  Alcotest.check (result Alcotest.string perr) "foo3 contains bar3 in more store" (Ok "bar3") (p.read ["packages"; "foo3"]) ;
  Alcotest.check (result Alcotest.string perr) "foo4 contains bar4 in more store" (Ok "bar4") (p.read ["packages"; "foo4"]) ;
  Alcotest.check (result Alcotest.string perr) "foo5 not contained in more store" (Error `NotFound) (p.read ["packages"; "foo5"]) ;
  p.write ["packages"; "foobar" ; "barfoo" ; "foobar" ; "foo"] "foobar" ;
  Alcotest.check (result Alcotest.string perr) "packages/foobar/barfoo/foobar/foo contained in more store"
    (Ok "foobar") (p.read ["packages"; "foobar" ; "barfoo" ; "foobar" ; "foo"]) ;
  Alcotest.check Alcotest.bool "packages/foobar/barfoo/foobar contained in more store"
    true (p.exists ["packages"; "foobar" ; "barfoo" ; "foobar"])

let mem_provider_tests = [
  "empty provider", `Quick, empty_p ;
  "basic provider", `Quick, basic_p ;
  "more provider", `Quick, more_p ;
]

let re =
  let module M = struct
    type t = Repository.r_err
    let pp = Repository.pp_r_err
    let equal a b = match a, b with
      | `NotFound a, `NotFound a' -> name_equal a a'
      | `NameMismatch (a, b), `NameMismatch (a', b') -> name_equal a a' && name_equal b b'
      | `ParseError (n, _), `ParseError (n', _) -> name_equal n n'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let ch_err =
  let module M = struct
    type t = [ `FileNotFound of name | `NotADirectory of name ]
    let pp = Repository.pp_error
    let equal a b = match a, b with
      | `FileNotFound a, `FileNotFound a' -> name_equal a a'
      | `NotADirectory a, `NotADirectory a' -> name_equal a a'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

(* basic operations work, and we have an in-memory data provider!  let the games begin *)
let empty_r () =
  let p = Mem.mem_provider () in
  let r = Repository.repository p in
  Alcotest.check sset "empty repo has no keys" S.empty (Repository.all_ids r) ;
  Alcotest.check sset "empty repo has no authorisations" S.empty (Repository.all_authorisations r) ;
  Alcotest.check (result publickey re) "reading key foo in empty repo fails"
    (Error (`NotFound "foo")) (Repository.read_key r "foo") ;
  Alcotest.check (result auth re) "reading authorisation foo in empty repo fails"
    (Error (`NotFound "foo")) (Repository.read_authorisation r "foo") ;
  Alcotest.check (result releases re) "reading releases foo in empty repo fails"
    (Error (`NotFound "foo")) (Repository.read_releases r "foo") ;
  Alcotest.check (result ji re) "reading janitorindex foo in empty repo fails"
    (Error (`NotFound "foo")) (Repository.read_index r "foo") ;
  Alcotest.check (result cs re) "reading checksum foo.0 in empty repo fails"
    (Error (`NotFound "foo.0")) (Repository.read_checksum r "foo.0") ;
  Alcotest.check (result cs ch_err) "computing checksum foo.0 in empty repo fails"
    (Error (`FileNotFound "foo.0")) (Repository.compute_checksum r "foo.0")

let key_r () =
  let p = Mem.mem_provider () in
  let r = Repository.repository p in
  let k, _pk = gen_pub "foo" in
  Repository.write_key r k ;
  Alcotest.check sset "key repo has one key" (S.singleton "foo") (Repository.all_ids r) ;
  Repository.write_key r k ;
  Alcotest.check sset "key repo+ has one key" (S.singleton "foo") (Repository.all_ids r) ;
  let k2, _pk2 = gen_pub "foobar" in
  Repository.write_key r k2 ;
  Alcotest.check sset "key repo has two keys" (S.add "foobar" (S.singleton "foo")) (Repository.all_ids r) ;
  Alcotest.check (result publickey re) "reading key gives back right key"
    (Ok k) (Repository.read_key r "foo") ;
  let jk, _jpk = gen_pub "janitor" in
  Repository.write_key r jk ;
  Alcotest.check sset "key repo has three keys" (S.add "janitor" (S.add "foobar" (S.singleton "foo"))) (Repository.all_ids r)

let team_r () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:10 p in
  Alcotest.(check int "quorum is correct" 10 (Repository.quorum r)) ;
  Alcotest.check sset "team foo is empty" S.empty (Repository.team r "foo") ;
  let t = Team.team ~members:(S.singleton "bar") "foo" in
  let r = Repository.add_team r t in
  Alcotest.check sset "team foo has one member" (S.singleton "bar") (Repository.team r "foo")

let checks_r () =
  let open Provider in
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  p.write ["packages"; "foo"; "foo.0"; "foo"] "bar" ;
  p.write ["packages"; "foo"; "foo.0"; "bar"] "foo" ;
  p.write ["packages"; "foo"; "foo.0"; "files"; "patch1"] "p1" ;
  p.write ["packages"; "foo"; "foo.0"; "files"; "patch2"] "p2" ;
  (* manually crafted using echo -n XXX | openssl dgst -sha256 -binary | b64encode -m - *)
  let csums = [
    { Checksum.filename = "bar" ; bytesize = 3L ; checksum = "LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm564=" } ;
    { Checksum.filename = "foo" ; bytesize = 3L ; checksum = "/N4rLtula/QIYB+3If6bXDONEO5CnqBPrlURto+/j7k=" } ;
    { Checksum.filename = "files/patch1" ; bytesize = 2L ; checksum = "9kVR/NbweCPLh5cc+5FEZCXaGChrOrHvk14MvXpp9oo=" } ;
    { Checksum.filename = "files/patch2" ; bytesize = 2L ; checksum = "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=" }
  ]
  in
  let css = Checksum.checksums "foo.0" csums in
  Alcotest.check (result cs ch_err) "checksum computation works"
    (Ok css) (Repository.compute_checksum r "foo.0")

open Conex_data_persistency

let bad_id_r () =
  let open Provider in
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  Alcotest.check (result publickey re) "key foo not found"
    (Error (`NotFound "foo")) (Repository.read_key r "foo") ;
  Alcotest.check (result team re) "team foo not found"
    (Error (`NotFound "foo")) (Repository.read_team r "foo") ;
  Alcotest.check (result id re) "ID foo not found"
    (Error (`NotFound "foo")) (Repository.read_id r "foo") ;
  p.write ["keys"; "foo"] "barf" ;
  Alcotest.check (result publickey re) "parse error on key foo"
    (Error (`ParseError ("foo", ""))) (Repository.read_key r "foo") ;
  Alcotest.check (result team re) "parse error on team foo"
    (Error (`ParseError ("foo", ""))) (Repository.read_team r "foo") ;
  Alcotest.check (result id re) "parse error on id foo"
    (Error (`ParseError ("foo", ""))) (Repository.read_id r "foo") ;
  let key = Publickey.publickey "foo" None in
  Repository.write_key r key ;
  Alcotest.check (result team re) "parse error on team foo"
    (Error (`ParseError ("foo", ""))) (Repository.read_team r "foo") ;
  Alcotest.check (result publickey re) "key foo parses"
    (Ok key) (Repository.read_key r "foo") ;
  Alcotest.check (result id re) "id foo parses"
    (Ok (`Key key)) (Repository.read_id r "foo") ;
  p.write ["keys"; "foobar"] (Data.encode (publickey_to_t key)) ;
  Alcotest.check (result team re) "parse error on team foobar"
    (Error (`ParseError ("foobar", ""))) (Repository.read_team r "foobar") ;
  Alcotest.check (result publickey re) "key foobar namemismatch"
    (Error (`NameMismatch ("foobar", "foo"))) (Repository.read_key r "foobar") ;
  Alcotest.check (result id re) "namemismatch id foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Repository.read_id r "foobar") ;
  let t = Team.team "foo" in
  Repository.write_team r t ;
  Alcotest.check (result publickey re) "parse error on key foo"
    (Error (`ParseError ("foo", ""))) (Repository.read_key r "foo") ;
  Alcotest.check (result team re) "team foo parses"
    (Ok t) (Repository.read_team r "foo") ;
  Alcotest.check (result id re) "id foo parses"
    (Ok (`Team t)) (Repository.read_id r "foo") ;
  p.write ["keys"; "foobar"] (Data.encode (team_to_t t)) ;
  Alcotest.check (result publickey re) "parse error on key foobar"
    (Error (`ParseError ("foobar", ""))) (Repository.read_key r "foobar") ;
  Alcotest.check (result team re) "name mismatch on team foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Repository.read_team r "foobar") ;
  Alcotest.check (result id re) "name mismatch on id foo"
    (Error (`NameMismatch ("foobar", "foo"))) (Repository.read_id r "foobar")

let bad_idx_r () =
  let open Provider in
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  Alcotest.check (result ji re) "index foo not found"
    (Error (`NotFound "foo")) (Repository.read_index r "foo") ;
  p.write ["index"; "foo"] "bla" ;
  Alcotest.check (result ji re) "good index foo"
    (Error (`ParseError ("foo", ""))) (Repository.read_index r "foo") ;
  let idx = Index.index "foo" in
  Repository.write_index r idx ;
  Alcotest.check (result ji re) "good index foo"
    (Ok idx) (Repository.read_index r "foo") ;
  p.write ["index"; "foobar"] (Data.encode (index_sigs_to_t idx)) ;
  Alcotest.check (result ji re) "name mismatch in foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Repository.read_index r "foobar")

let bad_auth_r () =
  let open Provider in
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  Alcotest.check (result auth re) "authorisation foo not found"
    (Error (`NotFound "foo")) (Repository.read_authorisation r "foo") ;
  p.write ["packages"; "foo"; "authorisation"] "foobar" ;
  Alcotest.check (result auth re) "parse error on authorisation foo"
    (Error (`ParseError ("foo", ""))) (Repository.read_authorisation r "foo") ;
  let a = Authorisation.authorisation "foo" in
  Repository.write_authorisation r a ;
  Alcotest.check (result auth re) "authorisation foo good"
    (Ok a) (Repository.read_authorisation r "foo") ;
  p.write ["packages"; "foobar"; "authorisation"] (Data.encode (authorisation_to_t a)) ;
  Alcotest.check (result auth re) "name mismatch on authorisation foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Repository.read_authorisation r "foobar")

let bad_rel_r () =
  let open Provider in
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  Alcotest.check (result releases re) "releases foo not found"
    (Error (`NotFound "foo")) (Repository.read_releases r "foo") ;
  p.write ["packages"; "foo"; "releases"] "foobar" ;
  Alcotest.check (result releases re) "parse error on releases foo"
    (Error (`ParseError ("foo", ""))) (Repository.read_releases r "foo") ;
  let rel = match Releases.releases "foo" with Ok r -> r | Error _ -> assert false in
  Repository.write_releases r rel ;
  Alcotest.check (result releases re) "releases foo good"
    (Ok rel) (Repository.read_releases r "foo") ;
  p.write ["packages"; "foobar"; "releases"] (Data.encode (releases_to_t rel)) ;
  Alcotest.check (result releases re) "name mismatch on releases foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Repository.read_releases r "foobar")

let bad_cs_r () =
  let open Provider in
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  Alcotest.check (result cs re) "checksum foo not found"
    (Error (`NotFound "foo")) (Repository.read_checksum r "foo") ;
  p.write ["packages"; "foo"; "foo.0"; "checksum"] "foobar" ;
  Alcotest.check (result cs re) "parse error on checksum foo.0"
    (Error (`ParseError ("foo.0", ""))) (Repository.read_checksum r "foo.0") ;
  let c = Checksum.checksums "foo.0" [] in
  Repository.write_checksum r c ;
  Alcotest.check (result cs re) "checksum foo.0 good"
    (Ok c) (Repository.read_checksum r "foo.0") ;
  p.write ["packages"; "foo"; "foo.1"; "checksum"] (Data.encode (checksums_to_t c)) ;
  Alcotest.check (result cs re) "name mismatch on checksum foo.1"
    (Error (`NameMismatch ("foo.1", "foo.0"))) (Repository.read_checksum r "foo.1") ;
  p.write ["packages"; "foo"; "foo.2"] "blubb" ;
  Alcotest.check (result cs ch_err) "checksum is a file, should be a directory"
    (Error (`NotADirectory "foo.2")) (Repository.compute_checksum r "foo.2")

let basic_repo_tests = [
  "empty repo", `Quick, empty_r ;
  "key repo", `Quick, key_r ;
  "team", `Quick, team_r ;
  "checksum computation is sane", `Quick, checks_r ;
  "bad id in repo", `Quick, bad_id_r ;
  "bad idx in repo", `Quick, bad_idx_r ;
  "bad auth in repo", `Quick, bad_auth_r ;
  "bad rel in repo", `Quick, bad_rel_r ;
  "bad cs in repo", `Quick, bad_cs_r ;
]


let idx_sign () =
  let p = Mem.mem_provider () in
  let r = Repository.repository p in
  let id = "foo" in
  let idx = Index.index id in
  Alcotest.check (result Alcotest.string verr) "index must be signed"
    (Error (`NoSignature id)) (Repository.verify_index r idx) ;
  let pub, priv = gen_pub id in
  let signed_idx = sign_idx idx priv in
  Alcotest.check (result Alcotest.string verr) "id must be in keystore"
    (Error (`InvalidIdentifier id)) (Repository.verify_index r signed_idx) ;
  let r = Repository.add_trusted_key r pub in
  Alcotest.check (result Alcotest.string verr) "idx signed properly"
    (Ok id) (Repository.verify_index r signed_idx) ;
  let oid = "bar" in
  let signed' =
    match signed_idx.Index.signatures with
    | [(_, s, ts)] -> Index.add_sig idx (oid, s, ts)
    | _ -> assert false
  in
  Alcotest.check (result Alcotest.string verr) "id not authorised"
    (Error (`NotAuthorised (id, oid))) (Repository.verify_index r signed')

let k_ok =
  let module M = struct
    type t = [ `Quorum of S.t | `Both of identifier * S.t ]
    let pp = Repository.pp_ok
    let equal a b = match a, b with
      | `Quorum js, `Quorum is -> S.equal js is
      | `Both (a, js), `Both (b, is) -> id_equal a b && S.equal js is
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let k_err =
  let module M = struct
    type t = [ Repository.base_error | `InsufficientQuorum of name * S.t | `MissingSignature of identifier ]
    let pp = Repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && resource_equal w w' && resource_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && resource_equal r r' && S.equal js js'
      | `InsufficientQuorum (id, q), `InsufficientQuorum (id', q') -> id_equal id id' && S.equal q q'
      | `MissingSignature id, `MissingSignature id' -> id_equal id id'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let res d =
  let data = Data.encode d in
  (Int64.of_int (String.length data), Conex_nocrypto.digest data)

let empty_key () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let id = "foo" in
  let pub = Publickey.publickey id None in
  let resources =
    let s, d = res (publickey_to_t pub) in
    [ Index.r 0L id s `PublicKey d ] in
  let jidx =
    let idx = Index.index ~resources jid in
    sign_idx idx jpriv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  Alcotest.check (result k_ok k_err) "not signed"
    (Error (`NotSigned (id, `PublicKey, S.empty)))
    (Repository.verify_key r pub) ;
  let r' = Repository.add_index r jidx in
  Alcotest.check (result k_ok k_err) "empty publickey good"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_key r' pub)

let key_good () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let id = "foo" in
  let pub, priv = gen_pub id in
  let resources =
    let s, d = res (publickey_to_t pub) in
    [ Index.r 0L id s `PublicKey d ] in
  let jidx =
    let idx = Index.index ~resources jid in
    sign_idx idx jpriv
  in
  let idx =
    let idx = Index.index ~resources id in
    sign_idx idx priv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  Alcotest.check (result k_ok k_err) "not signed"
    (Error (`NotSigned (id, `PublicKey, S.empty)))
    (Repository.verify_key r pub) ;
  let r' = Repository.add_index r jidx in
  Alcotest.check (result k_ok k_err) "publickey missing self-sig"
    (Error (`MissingSignature id))
    (Repository.verify_key r' pub) ;
  let r'' = Repository.add_index r idx in
  Alcotest.check (result k_ok k_err) "publickey missing quorum"
    (Error (`InsufficientQuorum (id, S.empty)))
    (Repository.verify_key r'' pub) ;
  let r''' = Repository.add_index r' idx in
  Alcotest.check (result k_ok k_err) "publickey is fine"
    (Ok (`Both (id, S.singleton jid)))
    (Repository.verify_key r''' pub)

let key_good_quorum () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:3 p in
  let id = "foo" in
  let pub, priv = gen_pub id in
  let resources =
    let s, d = res (publickey_to_t pub) in
    [ Index.r 0L id s `PublicKey d ] in
  let idx =
    let idx = Index.index ~resources id in
    sign_idx idx priv
  in
  let r = Repository.add_index r idx in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 0"
    (Error (`InsufficientQuorum (id, S.empty)))
    (Repository.verify_key r pub) ;
  let jidx r jid =
    let jpub, jpriv = gen_pub jid in
    let idx = Index.index ~resources jid in
    let idx = sign_idx idx jpriv in
    let r = Repository.add_trusted_key r jpub in
    let r =
      let mems = Repository.team r "janitors" in
      Repository.add_team r (Team.team ~members:(S.add jid mems) "janitors")
    in
    Repository.add_index r idx
  in
  let r = jidx r "jana" in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 1"
    (Error (`InsufficientQuorum (id, S.singleton "jana")))
    (Repository.verify_key r pub) ;
  let r = jidx r "janb" in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 2"
    (Error (`InsufficientQuorum (id, S.add "janb" (S.singleton "jana"))))
    (Repository.verify_key r pub) ;
  let r = jidx r "janc" in
  Alcotest.check (result k_ok k_err) "publickey is fine"
    (Ok (`Both (id, S.add "janc" (S.add "janb" (S.singleton "jana")))))
    (Repository.verify_key r pub)

let no_janitor () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let id = "foo" in
  let pub, priv = gen_pub id in
  let id' = "bar" in
  let pem = Publickey.publickey id' None in
  let resources =
    let s, d = res (publickey_to_t pub)
    and s', d' = res (publickey_to_t pem)
    in
    [ Index.r 0L id s `PublicKey d ; Index.r 1L id' s' `PublicKey d' ]
  in
  let jidx =
    let idx = Index.index ~resources jid in
    sign_idx idx jpriv
  in
  let idx =
    let idx = Index.index ~resources id in
    sign_idx idx priv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  let r = Repository.add_index r idx in
  Alcotest.check (result k_ok k_err) "missing quorum"
    (Error (`InsufficientQuorum (id, S.empty)))
    (Repository.verify_key r pub) ;
  Alcotest.check (result k_ok k_err) "missing quorum for empty"
    (Error (`NotSigned (id', `PublicKey, S.empty)))
    (Repository.verify_key r pem)

let k_wrong_resource () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let id = "foo" in
  let pub, priv = gen_pub id in
  let resources =
    let s, d = res (publickey_to_t pub) in
    [ Index.r 0L id s `Checksums d ]
  in
  let jidx =
    let idx = Index.index ~resources jid in
    sign_idx idx jpriv
  in
  let idx =
    let idx = Index.index ~resources id in
    sign_idx idx priv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  let r = Repository.add_index r idx in
  Alcotest.check (result k_ok k_err) "wrong resource"
    (Error (`InvalidResource (id, `PublicKey, `Checksums)))
    (Repository.verify_key r pub)

let k_wrong_name () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let id = "foo" in
  let pub, priv = gen_pub id in
  let resources =
    let s, d = res (publickey_to_t pub) in
    [ Index.r 0L jid s `PublicKey d ]
  in
  let jidx =
    let idx = Index.index ~resources jid in
    sign_idx idx jpriv
  in
  let idx =
    let idx = Index.index ~resources id in
    sign_idx idx priv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  let r = Repository.add_index r idx in
  Alcotest.check (result k_ok k_err) "wrong name"
    (Error (`InvalidName (id, jid)))
    (Repository.verify_key r pub)

let key_repo_tests = [
  "signed index", `Quick, idx_sign ;
  "empty key", `Quick, empty_key ;
  "key good", `Quick, key_good ;
  "key good with quorum = 3", `Quick, key_good_quorum ;
  "no janitor", `Quick, no_janitor ;
  "wrong resource", `Quick, k_wrong_resource ;
  "wrong name", `Quick, k_wrong_name ;
]


let a_ok =
  let module M = struct
    type t = [ `Quorum of S.t ]
    let pp = Repository.pp_ok
    let equal a b = match a, b with
      | `Quorum js, `Quorum is -> S.equal js is
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let a_err =
  let module M = struct
    type t = [ Repository.base_error | `InsufficientQuorum of name * S.t ]
    let pp = Repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InsufficientQuorum (id, q), `InsufficientQuorum (id', q') -> id_equal id id' && S.equal q q'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && resource_equal w w' && resource_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && resource_equal r r' && S.equal js js'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let team () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop" in
  let team = Team.team pname in
  Alcotest.check (result a_ok a_err) "team missing quorum"
    (Error (`InsufficientQuorum (pname, S.empty)))
    (Repository.verify_team r team) ;
  let resources =
    let s, d = res (team_to_t team) in
    [ Index.r 0L pname s `Team d ]
  in
  let j_sign r jid =
    let jpub, jpriv = gen_pub jid in
    let idx =
      let i = Index.index ~resources jid in
      sign_idx i jpriv
    in
    let r = Repository.add_trusted_key r jpub in
    let r =
      let mems = Repository.team r "janitors" in
      Repository.add_team r (Team.team ~members:(S.add jid mems) "janitors")
    in
    Repository.add_index r idx
  in
  let r = j_sign r "janitor" in
  Alcotest.check (result a_ok a_err) "team properly signed"
    (Ok (`Quorum (S.singleton "janitor")))
    (Repository.verify_team r team) ;
  let r = Repository.repository ~quorum:2 p in
  let r = j_sign r "janitor" in
  Alcotest.check (result a_ok a_err) "team missing quorum of 2"
    (Error (`InsufficientQuorum (pname, S.singleton "janitor")))
    (Repository.verify_team r team) ;
  let r = j_sign r "janitor2" in
  Alcotest.check (result a_ok a_err) "team quorum of 2 good"
    (Ok (`Quorum (S.add "janitor2" (S.singleton "janitor"))))
    (Repository.verify_team r team)

let team_self_signed () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let id = "foo" in
  let pname = "foop" in
  let team = Team.team pname in
  let pub, priv = gen_pub id in
  let resources =
    let s, d = res (team_to_t team)
    and s', d' = res (publickey_to_t pub)
    in
    [ Index.r 0L pname s `Team d ; Index.r 1L id s' `PublicKey d' ]
  in
  let s_idx id priv =
    sign_idx (Index.index ~resources id) priv
  in
  let idx = s_idx id priv in
  let r = Repository.add_index r idx in
  Alcotest.check (result a_ok a_err) "team missing quorum"
    (Error (`InsufficientQuorum (pname, S.empty)))
    (Repository.verify_team r team) ;
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let jidx = s_idx jid jpriv in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r jidx in
  Alcotest.check (result a_ok a_err) "team ok"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_team r team)

let team_wrong_resource () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop" in
  let team = Team.team pname in
  let resources =
    let s, d = res (team_to_t team) in
    [ Index.r 0L pname s `Checksums d ]
  in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let jidx =
    let idx = Index.index ~resources jid in
    sign_idx idx jpriv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  Alcotest.check (result a_ok a_err) "wrong resource"
    (Error (`InvalidResource (pname, `Team, `Checksums)))
    (Repository.verify_team r team)

let team_wrong_name () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop" in
  let team = Team.team pname in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let resources =
    let s, d = res (team_to_t team) in
    [ Index.r 0L "barf" s `Team d ]
  in
  let jidx =
    let idx = Index.index ~resources jid in
    sign_idx idx jpriv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  Alcotest.check (result a_ok a_err) "wrong name"
    (Error (`InvalidName (pname, "barf")))
    (Repository.verify_team r team)

let team_dyn () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop" in
  let team = Team.team pname in
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors")
  in
  let j_sign resources =
    let idx =
      let i = Index.index ~resources jid in
      sign_idx i jpriv
    in
    Repository.add_index r idx
  in
  let resources =
    let s, d = res (team_to_t team) in
    [ Index.r 0L pname s `Team d ]
  in
  let r = j_sign resources in
  Alcotest.check (result a_ok a_err) "team properly signed"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_team r team) ;
  let team = Team.add team "foobar" in
  Alcotest.check (result a_ok a_err) "team not properly signed"
    (Error (`InsufficientQuorum (pname, S.empty)))
    (Repository.verify_team r team) ;
  let resources =
    let s, d = res (team_to_t team) in
    [ Index.r 0L pname s `Team d ]
  in
  let r = j_sign resources in
  Alcotest.check (result a_ok a_err) "team properly signed"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_team r team) ;
  let team = Team.remove team "foo" in
  Alcotest.check (result a_ok a_err) "team properly signed (nothing changed)"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_team r team) ;
  let team = Team.add team "foobar" in
  Alcotest.check (result a_ok a_err) "team properly signed (nothing changed)"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_team r team) ;
  let team = Team.remove team "foobar" in
  Alcotest.check (result a_ok a_err) "team not properly signed (rm'ed, counter incr)"
    (Error (`InsufficientQuorum (pname, S.empty)))
    (Repository.verify_team r team)

let team_repo_tests = [
  "basic team", `Quick, team ;
  "also self signed", `Quick, team_self_signed ;
  "wrong resource", `Quick, team_wrong_resource ;
  "wrong name", `Quick, team_wrong_name ;
  "dynamic team", `Quick, team_dyn
]


let auth () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop" in
  let auth = Authorisation.authorisation pname in
  Alcotest.check (result a_ok a_err) "auth missing quorum"
    (Error (`InsufficientQuorum (pname, S.empty)))
    (Repository.verify_authorisation r auth) ;
  let resources =
    let s, d = res (authorisation_to_t auth) in
    [ Index.r 0L pname s `Authorisation d ]
  in
  let j_sign r jid =
    let jpub, jpriv = gen_pub jid in
    let idx =
      let i = Index.index ~resources jid in
      sign_idx i jpriv
    in
    let r = Repository.add_trusted_key r jpub in
    let r =
      let mems = Repository.team r "janitors" in
      Repository.add_team r (Team.team ~members:(S.add jid mems) "janitors")
    in
    Repository.add_index r idx
  in
  let r = j_sign r "janitor" in
  Alcotest.check (result a_ok a_err) "auth properly signed"
    (Ok (`Quorum (S.singleton "janitor")))
    (Repository.verify_authorisation r auth) ;
  let r = Repository.repository ~quorum:2 p in
  let r = j_sign r "janitor" in
  Alcotest.check (result a_ok a_err) "auth missing quorum of 2"
    (Error (`InsufficientQuorum (pname, S.singleton "janitor")))
    (Repository.verify_authorisation r auth) ;
  let r = j_sign r "janitor2" in
  Alcotest.check (result a_ok a_err) "auth quorum of 2 good"
    (Ok (`Quorum (S.add "janitor2" (S.singleton "janitor"))))
    (Repository.verify_authorisation r auth)

let auth_self_signed () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let id = "foo" in
  let pname = "foop" in
  let auth = Authorisation.authorisation pname in
  let pub, priv = gen_pub id in
  let resources =
    let s, d = res (authorisation_to_t auth)
    and s', d' = res (publickey_to_t pub)
    in
    [ Index.r 0L pname s `Authorisation d ; Index.r 1L id s' `PublicKey d' ]
  in
  let s_idx id priv =
    sign_idx (Index.index ~resources id) priv
  in
  let idx = s_idx id priv in
  let r = Repository.add_index r idx in
  Alcotest.check (result a_ok a_err) "auth missing quorum"
    (Error (`InsufficientQuorum (pname, S.empty)))
    (Repository.verify_authorisation r auth) ;
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let jidx = s_idx jid jpriv in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r jidx in
  Alcotest.check (result a_ok a_err) "auth ok"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_authorisation r auth)

let a_wrong_resource () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop" in
  let auth = Authorisation.authorisation pname in
  let resources =
    let s, d = res (authorisation_to_t auth) in
    [ Index.r 0L pname s `Checksums d ]
  in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let jidx =
    let idx = Index.index ~resources jid in
    sign_idx idx jpriv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  Alcotest.check (result a_ok a_err) "wrong resource"
    (Error (`InvalidResource (pname, `Authorisation, `Checksums)))
    (Repository.verify_authorisation r auth)

let a_wrong_name () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop" in
  let auth = Authorisation.authorisation pname in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let resources =
    let s, d = res (authorisation_to_t auth) in
    [ Index.r 0L "barf" s `Authorisation d ]
  in
  let jidx =
    let idx = Index.index ~resources jid in
    sign_idx idx jpriv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  Alcotest.check (result a_ok a_err) "wrong name"
    (Error (`InvalidName (pname, "barf")))
    (Repository.verify_authorisation r auth)

let auth_dyn () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop" in
  let auth = Authorisation.authorisation pname in
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors")
  in
  let j_sign resources =
    let idx =
      let i = Index.index ~resources jid in
      sign_idx i jpriv
    in
    Repository.add_index r idx
  in
  let resources =
    let s, d = res (authorisation_to_t auth) in
    [ Index.r 0L pname s `Authorisation d ]
  in
  let r = j_sign resources in
  Alcotest.check (result a_ok a_err) "authorisation properly signed"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_authorisation r auth) ;
  let auth = Authorisation.add auth "foobar" in
  Alcotest.check (result a_ok a_err) "authorisation not properly signed"
    (Error (`InsufficientQuorum (pname, S.empty)))
    (Repository.verify_authorisation r auth) ;
  let resources =
    let s, d = res (authorisation_to_t auth) in
    [ Index.r 0L pname s `Authorisation d ]
  in
  let r = j_sign resources in
  Alcotest.check (result a_ok a_err) "authorisation properly signed"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_authorisation r auth) ;
  let auth = Authorisation.remove auth "foo" in
  Alcotest.check (result a_ok a_err) "authorisation properly signed (nothing changed)"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_authorisation r auth) ;
  let auth = Authorisation.add auth "foobar" in
  Alcotest.check (result a_ok a_err) "authorisation properly signed (nothing changed)"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_authorisation r auth) ;
  let auth = Authorisation.remove auth "foobar" in
  Alcotest.check (result a_ok a_err) "authorisation not properly signed (rm'ed, counter incr)"
    (Error (`InsufficientQuorum (pname, S.empty)))
    (Repository.verify_authorisation r auth)


let auth_repo_tests = [
  "basic auth", `Quick, auth ;
  "also self signed", `Quick, auth_self_signed ;
  "wrong resource", `Quick, a_wrong_resource ;
  "wrong name", `Quick, a_wrong_name ;
  "dynamic authorisation", `Quick, auth_dyn ;
]

let r_ok =
  let module M = struct
    type t = [ `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ]
    let pp = Repository.pp_ok
    let equal a b = match a, b with
      | `Signed a, `Signed b -> id_equal a b
      | `Quorum js, `Quorum is -> S.equal js is
      | `Both (a, js), `Both (b, is) -> id_equal a b && S.equal js is
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let r_err =
  let module M = struct
    type t = [ Repository.base_error | `AuthRelMismatch of name * name | `InvalidReleases of name * S.t * S.t ]
    let pp = Repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && resource_equal w w' && resource_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && resource_equal r r' && S.equal js js'
      | `AuthRelMismatch (a, r), `AuthRelMismatch (a', r') -> name_equal a a' && name_equal r r'
      | `InvalidReleases (n, h, w), `InvalidReleases (n', h', w') -> name_equal n n' && S.equal h h' && S.equal w w'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let safe_rel ?releases name =
  match Releases.releases ?releases name with
  | Ok r -> r
  | Error _ -> invalid_arg "shouldn't happen"

let rel () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel pname in
  Alcotest.check (result r_ok r_err) "not signed"
    (Error (`NotSigned (pname, `Releases, S.empty)))
    (Repository.verify_releases r auth rel) ;
  let _pub, priv = gen_pub id in
  let sidx =
    let resources =
      let s, d = res (releases_to_t rel) in
      [ Index.r 0L pname s `Releases d ]
    in
    sign_idx (Index.index ~resources id) priv
  in
  let r = Repository.add_index r sidx in
  Alcotest.check (result r_ok r_err) "properly signed"
    (Ok (`Signed id))
    (Repository.verify_releases r auth rel)

let rel_quorum () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel pname in
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let resources =
    let s, d = res (releases_to_t rel) in
    [ Index.r 0L pname s `Releases d ]
  in
  let sidx =
    sign_idx (Index.index ~resources jid) jpriv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r sidx in
  Alcotest.check (result r_ok r_err) "properly signed (quorum)"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_releases r auth rel) ;
  let _pub, priv = gen_pub id in
  let sidx' =
    sign_idx (Index.index ~resources id) priv
  in
  let r = Repository.add_index r sidx' in
  Alcotest.check (result r_ok r_err) "properly signed (both)"
    (Ok (`Both (id, S.singleton jid)))
    (Repository.verify_releases r auth rel)

let rel_not_authorised () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton "foo") pname in
  let rel = safe_rel pname in
  let _pub, priv = gen_pub id in
  let sidx =
    let resources =
      let s, d = res (releases_to_t rel) in
      [ Index.r 0L pname s `Releases d ]
    in
    sign_idx (Index.index ~resources id) priv
  in
  let r = Repository.add_index r sidx in
  Alcotest.check (result r_ok r_err) "not authorised"
    (Error (`NotSigned (pname, `Releases, S.empty)))
    (Repository.verify_releases r auth rel)

let rel_bad_releases () =
  let pname = "foop"
  and id = "id"
  in
  Alcotest.check (result Alcotest.pass Alcotest.string) "bad releases"
    (Error "all releases must have the same package name")
    (Releases.releases ~releases:(S.singleton id) pname) ;
  Alcotest.check (result Alcotest.pass Alcotest.string) "bad releases 2"
    (Error "all releases must have the same package name")
    (Releases.releases ~releases:(S.singleton pname) pname)

let rel_missing_releases () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let v = pname ^ ".0" in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let _pub, priv = gen_pub id in
  let sidx =
    let resources =
      let s, d = res (releases_to_t rel) in
      [ Index.r 0L pname s `Releases d ]
    in
    sign_idx (Index.index ~resources id) priv
  in
  let r = Repository.add_index r sidx in
  Alcotest.check (result r_ok r_err) "missing on disk"
    (Error (`InvalidReleases (pname, S.empty, S.singleton v)))
    (Repository.verify_releases r auth rel) ;
  p.Provider.write ["packages"; pname; v; "checksum"] "" ;
  Alcotest.check (result r_ok r_err) "all good"
    (Ok (`Signed id))
    (Repository.verify_releases r auth rel) ;
  let v2 = pname ^ ".1" in
  p.Provider.write ["packages"; pname; v2; "checksum"] "" ;
  Alcotest.check (result r_ok r_err) "missing in releases"
    (Error (`InvalidReleases (pname, S.singleton v2, S.empty)))
    (Repository.verify_releases r auth rel) ;
  let v3 = pname ^ ".2" in
  p.Provider.write ["packages"; pname; v3; "checksum"] "" ;
  Alcotest.check (result r_ok r_err) "missing in releases"
    (Error (`InvalidReleases (pname, S.add v3 (S.singleton v2), S.empty)))
    (Repository.verify_releases r auth rel)


let rel_name_mismatch () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) "foo" in
  let rel = safe_rel pname in
  let _pub, priv = gen_pub id in
  let sidx =
    let resources =
      let s, d = res (releases_to_t rel) in
      [ Index.r 0L pname s `Releases d ]
    in
    sign_idx (Index.index ~resources id) priv
  in
  let r = Repository.add_index r sidx in
  Alcotest.check (result r_ok r_err) "releases and authorisation names do not match"
    (Error (`AuthRelMismatch ("foo", pname)))
    (Repository.verify_releases r auth rel)

let rel_wrong_name () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel pname in
  let _pub, priv = gen_pub id in
  let sidx =
    let resources =
      let s, d = res (releases_to_t rel) in
      [ Index.r 0L "foo" s `Releases d ]
    in
    sign_idx (Index.index ~resources id) priv
  in
  let r = Repository.add_index r sidx in
  Alcotest.check (result r_ok r_err) "wrong name in releases"
    (Error (`InvalidName (pname, "foo")))
    (Repository.verify_releases r auth rel)

let rel_wrong_resource () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel pname in
  let _pub, priv = gen_pub id in
  let sidx =
    let resources =
      let s, d = res (releases_to_t rel) in
      [ Index.r 0L pname s `Authorisation d ]
    in
    sign_idx (Index.index ~resources id) priv
  in
  let r = Repository.add_index r sidx in
  Alcotest.check (result r_ok r_err) "wrong resource for releases"
    (Error (`InvalidResource (pname, `Releases, `Authorisation)))
    (Repository.verify_releases r auth rel)

let rel_repo_tests = [
  "basic release", `Quick, rel ;
  "quorum on releases", `Quick, rel_quorum ;
  "not authorised", `Quick, rel_not_authorised ;
  "bad release", `Quick, rel_bad_releases ;
  "missing some releases", `Quick, rel_missing_releases ;
  "name mismatch (releases and authorisation)", `Quick, rel_name_mismatch ;
  "wrong name", `Quick, rel_wrong_name ;
  "wrong resource", `Quick, rel_wrong_resource ;
]


let c_err =
  let module M = struct
    type t = [ Repository.base_error  | `AuthRelMismatch of name * name | `NotInReleases of name * S.t | `FileNotFound of name | `NotADirectory of name | `ChecksumsDiff of name * name list * name list * (Checksum.c * Checksum.c) list ]
    let pp = Repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && resource_equal w w' && resource_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && resource_equal r r' && S.equal js js'
      | `AuthRelMismatch (a, r), `AuthRelMismatch (a', r') -> name_equal a a' && name_equal r r'
      | `NotInReleases (c, r), `NotInReleases (c', r') -> name_equal c c' && S.equal r r'
      | `FileNotFound n, `FileNotFound n' -> name_equal n n'
      | `NotADirectory n, `NotADirectory n' -> name_equal n n'
      | `ChecksumsDiff (n, a, b, cs), `ChecksumsDiff (n', a', b', cs') ->
        name_equal n n' &&
        S.equal (S.of_list a) (S.of_list a') &&
        S.equal (S.of_list b) (S.of_list b') &&
        List.length cs = List.length cs' &&
        List.for_all (fun (c, d) -> List.exists (fun (c', d') -> Checksum.checksum_equal c c' && Checksum.checksum_equal d d') cs) cs'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let cs_base () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let cs = Checksum.checksums v [] in
  Alcotest.check (result r_ok c_err) "checksum not signed"
    (Error (`NotSigned (v, `Checksums, S.empty)))
    (Repository.verify_checksum r auth rel cs) ;
  let resources =
    let s, d = res (releases_to_t rel)
    and s', d' = res (checksums_to_t cs)
    in
    [ Index.r 0L pname s `Releases d ; Index.r 1L v s' `Checksums d' ]
  in
  let _pub, priv = gen_pub id in
  let sidx = sign_idx (Index.index ~resources id) priv in
  let r = Repository.add_index r sidx in
  p.Provider.write ["packages"; pname; v; "checksum"] "" ;
  Alcotest.check (result r_ok c_err) "good checksum"
    (Ok (`Signed id))
    (Repository.verify_checksum r auth rel cs) ;
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let sjidx = sign_idx (Index.index ~resources jid) jpriv in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r sjidx in
  Alcotest.check (result r_ok c_err) "good checksum (both)"
    (Ok (`Both (id, S.singleton jid)))
    (Repository.verify_checksum r auth rel cs)

let cs_quorum () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (releases_to_t rel)
    and s', d' = res (checksums_to_t cs)
    in
    [ Index.r 0L pname s `Releases d ; Index.r 1L v s' `Checksums d' ]
  in
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let sjidx = sign_idx (Index.index ~resources jid) jpriv in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r sjidx in
  p.Provider.write ["packages"; pname; v; "checksum"] "" ;
  Alcotest.check (result r_ok c_err) "good checksum (quorum)"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_checksum r auth rel cs)

let cs_bad () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let open Provider in
  let pname = "foo" in
  let v = pname ^ ".0" in
  p.write ["packages"; pname; v; "foo"] "bar" ;
  p.write ["packages"; pname; v; "bar"] "foo" ;
  p.write ["packages"; pname; v; "files"; "patch1"] "p1" ;
  p.write ["packages"; pname; v; "files"; "patch2"] "p2" ;
  (* manually crafted using echo -n XXX | openssl dgst -sha256 -binary | b64encode -m - *)
  let csums = [
    { Checksum.filename = "bar" ; bytesize = 3L ; checksum = "LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm564=" } ;
    { Checksum.filename = "foo" ; bytesize = 3L ; checksum = "/N4rLtula/QIYB+3If6bXDONEO5CnqBPrlURto+/j7k=" } ;
    { Checksum.filename = "files/patch1" ; bytesize = 2L ; checksum = "9kVR/NbweCPLh5cc+5FEZCXaGChrOrHvk14MvXpp9oo=" } ;
    { Checksum.filename = "files/patch2" ; bytesize = 2L ; checksum = "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=" }
  ]
  in
  let css = Checksum.checksums v csums in
  Alcotest.check (result cs ch_err) "checksum computation works"
    (Ok css) (Repository.compute_checksum r "foo.0") ;
  let css' = Checksum.checksums v (List.tl csums) in
  let css'' = Checksum.checksums v ({ Checksum.filename = "foobar" ; bytesize = 3L ; checksum = "" } :: csums) in
  let other = { Checksum.filename = "bar" ; bytesize = 3L ; checksum = "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=" } in
  let css''' = Checksum.checksums v (other :: List.tl csums) in
  let id = "id" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let resources =
    let s, d = res (releases_to_t rel)
    and s1, d1 = res (checksums_to_t css)
    and s2, d2 = res (checksums_to_t css')
    and s3, d3 = res (checksums_to_t css'')
    and s4, d4 = res (checksums_to_t css''')
    in
    [ Index.r 0L pname s `Releases d ;
      Index.r 1L v s1 `Checksums d1 ;
      Index.r 2L v s2 `Checksums d2 ;
      Index.r 3L v s3 `Checksums d3 ;
      Index.r 4L v s4 `Checksums d4 ]
  in
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let sjidx = sign_idx (Index.index ~resources jid) jpriv in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r sjidx in
  Alcotest.check (result r_ok c_err) "good checksum (quorum)"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_checksum r auth rel css) ;
  Alcotest.check (result r_ok c_err) "bad checksum (missing in cs file)"
    (Error (`ChecksumsDiff (v, [], ["bar"], [])))
    (Repository.verify_checksum r auth rel css') ;
  Alcotest.check (result r_ok c_err) "bad checksum (missing on disk)"
    (Error (`ChecksumsDiff (v, ["foobar"], [], [])))
    (Repository.verify_checksum r auth rel css'') ;
  Alcotest.check (result r_ok c_err) "bad checksum (differ)"
    (Error (`ChecksumsDiff (v, [], [], [(List.hd csums, other)])))
    (Repository.verify_checksum r auth rel css''')

let cs_bad_name () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let reln = "foo" in
  let v = reln ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) reln in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (releases_to_t rel)
    and s', d' = res (checksums_to_t cs)
    in
    [ Index.r 0L reln s `Releases d ; Index.r 1L v s' `Checksums d' ]
  in
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let sjidx = sign_idx (Index.index ~resources jid) jpriv in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r sjidx in
  Alcotest.check (result r_ok c_err) "bad name (auth != rel)"
    (Error (`AuthRelMismatch (pname, reln)))
    (Repository.verify_checksum r auth rel cs)

let cs_bad_name2 () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let reln = pname ^ ".0" in
  let v = reln ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton reln) pname in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (releases_to_t rel)
    and s', d' = res (checksums_to_t cs)
    in
    [ Index.r 0L pname s `Releases d ; Index.r 1L v s' `Checksums d' ]
  in
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let sjidx = sign_idx (Index.index ~resources jid) jpriv in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r sjidx in
  Alcotest.check (result r_ok c_err) "bad name (not member of releases)"
    (Error (`NotInReleases (v, rel.Releases.releases)))
    (Repository.verify_checksum r auth rel cs)

let cs_wrong_name () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (releases_to_t rel)
    and s', d' = res (checksums_to_t cs)
    in
    [ Index.r 0L pname s `Releases d ; Index.r 1L pname s' `Checksums d' ]
  in
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let sjidx = sign_idx (Index.index ~resources jid) jpriv in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r sjidx in
  Alcotest.check (result r_ok c_err) "wrong name"
    (Error (`InvalidName (v, pname)))
    (Repository.verify_checksum r auth rel cs)

let cs_wrong_resource () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (releases_to_t rel)
    and s', d' = res (checksums_to_t cs)
    in
    [ Index.r 0L pname s `Releases d ; Index.r 1L v s' `Releases d' ]
  in
  let jid = "janitor" in
  let jpub, jpriv = gen_pub jid in
  let sjidx = sign_idx (Index.index ~resources jid) jpriv in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = Repository.add_index r sjidx in
  Alcotest.check (result r_ok c_err) "wrong resource"
    (Error (`InvalidResource (v, `Checksums, `Releases)))
    (Repository.verify_checksum r auth rel cs)

let cs_repo_tests = [
  "basic checksum", `Quick, cs_base ;
  "quorum checksum", `Quick, cs_quorum ;
  "bad checksum", `Quick, cs_bad ;
  "bad releases name", `Quick, cs_bad_name ;
  "checksum name not in releases", `Quick, cs_bad_name2 ;
  "wrong checksum name", `Quick, cs_wrong_name ;
  "wrong checksum resource", `Quick, cs_wrong_resource ;
]

let tests = [
  "MemoryProvider", mem_provider_tests ;
  "RepositoryBasics", basic_repo_tests ;
  "RepositoryKeys", key_repo_tests ;
  "RepositoryTeam", team_repo_tests ;
  "RepositoryAuthorisation", auth_repo_tests ;
  "RepositoryReleases", rel_repo_tests ;
  "RepositoryChecksums", cs_repo_tests ;
]
