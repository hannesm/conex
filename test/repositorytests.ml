open Core
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
  p.write ["data"; "foo"] "bar" ;
  Alcotest.check Alcotest.bool "data in more store" true (p.exists ["data"]) ;
  Alcotest.check Alcotest.bool "data/foo in more store" true (p.exists ["data"; "foo"]) ;
  Alcotest.check Alcotest.bool "data/foo/bar not in more store" false (p.exists ["data"; "foo"; "bar"]) ;
  p.write ["data"; "foo2"] "bar2" ;
  Alcotest.check (result (Alcotest.list it) perr) "read_dir of 'data' in more mem store"
    (Ok [ `File "foo2" ; `File "foo" ]) (p.read_dir ["data"]) ;
  p.write ["data"; "foo3"] "bar3" ;
  p.write ["data"; "foo4"] "bar4" ;
  Alcotest.check (result (Alcotest.list it) perr) "read_dir of 'data' in even more mem store"
    (Ok [ `File "foo4" ; `File "foo3" ; `File "foo2" ; `File "foo" ]) (p.read_dir ["data"]) ;
  Alcotest.check (result (Alcotest.list it) perr) "read_dir of 'data2' in even more mem store"
    (Error `NotFound) (p.read_dir ["data2"]) ;
  Alcotest.check (result Alcotest.string perr) "foo contains bar in more store" (Ok "bar") (p.read ["data";"foo"]) ;
  Alcotest.check (result Alcotest.string perr) "foo2 contains bar2 in more store" (Ok "bar2") (p.read ["data"; "foo2"]) ;
  Alcotest.check (result Alcotest.string perr) "foo3 contains bar3 in more store" (Ok "bar3") (p.read ["data"; "foo3"]) ;
  Alcotest.check (result Alcotest.string perr) "foo4 contains bar4 in more store" (Ok "bar4") (p.read ["data"; "foo4"]) ;
  Alcotest.check (result Alcotest.string perr) "foo5 not contained in more store" (Error `NotFound) (p.read ["data"; "foo5"]) ;
  p.write ["data"; "foobar" ; "barfoo" ; "foobar" ; "foo"] "foobar" ;
  Alcotest.check (result Alcotest.string perr) "data/foobar/barfoo/foobar/foo contained in more store"
    (Ok "foobar") (p.read ["data"; "foobar" ; "barfoo" ; "foobar" ; "foo"]) ;
  Alcotest.check Alcotest.bool "data/foobar/barfoo/foobar contained in more store"
    true (p.exists ["data"; "foobar" ; "barfoo" ; "foobar"])

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
      | `NotFound x, `NotFound y -> x = y
      | `NameMismatch _, `NameMismatch _ -> true
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

(* basic operations work, and we have an in-memory data provider!  let the games begin *)
let empty_r () =
  let p = Mem.mem_provider () in
  let r = Repository.repository p in
  Alcotest.(check (list string) "empty repo has no keys" [] (Repository.all_keyids r)) ;
  Alcotest.(check (list string) "empty repo has no janitors" [] (Repository.all_janitors r)) ;
  Alcotest.(check (list string) "empty repo has no authorisations" [] (Repository.all_authorisations r)) ;
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
  Alcotest.check (result cs re) "computing checksum foo.0 in empty repo fails"
    (Error (`NotFound "foo.0")) (Repository.compute_checksum r "foo.0")

let key_r () =
  let p = Mem.mem_provider () in
  let r = Repository.repository p in
  Alcotest.(check (list string) "empty key repo has no keys" [] (Repository.all_keyids r)) ;
  let k, _pk = gen_pub "foo" in
  Repository.write_key r k ;
  Alcotest.(check (list string) "key repo has one key" ["foo"] (Repository.all_keyids r)) ;
  Repository.write_key r k ;
  Alcotest.(check (list string) "key repo+ has one key" ["foo"] (Repository.all_keyids r)) ;
  let k2, _pk2 = gen_pub "foobar" in
  Repository.write_key r k2 ;
  Alcotest.(check (list string) "key repo has two keys" ["foobar" ; "foo"] (Repository.all_keyids r)) ;
  Alcotest.check (result publickey re) "reading key gives back right key"
    (Ok k) (Repository.read_key r "foo")

let idx_sign () =
  let p = Mem.mem_provider () in
  let r = Repository.repository p in
  let id = "foo" in
  let idx = Index.index id in
  Alcotest.check (result Alcotest.string verr) "index must be signed"
    (Error (`NoSignature id)) (Repository.verify_index r idx) ;
  let pub, priv = gen_pub id in
  let signed_idx = Private.sign_index idx priv in
  Alcotest.check (result Alcotest.string verr) "id must be in keystore"
    (Error (`InvalidIdentifier id)) (Repository.verify_index r signed_idx) ;
  let r = Repository.add_trusted_key r pub in
  Alcotest.check (result Alcotest.string verr) "idx signed properly"
    (Ok id) (Repository.verify_index r signed_idx) ;
  let oid = "bar" in
  let signed' =
    match signed_idx.Index.signature with
    | None -> assert false
    | Some (_, s) -> Index.add_sig idx (oid, s)
  in
  Alcotest.check (result Alcotest.string verr) "id not authorised"
    (Error (`NotAuthorised (id, oid))) (Repository.verify_index r signed')

let ok =
  let module M = struct
    type t = Repository.ok
    let pp = Repository.pp_ok
    let equal a b = match a, b with
      | `Identifier a, `Identifier b -> id_equal a b
      | `Quorum js, `Quorum is -> S.equal js is
      | `Both (a, js), `Both (b, is) -> id_equal a b && S.equal js is
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let empty_key () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub ~role:`Janitor jid in
  let id = "foo" in
  let pub = match Publickey.publickey id None with
    | Ok p -> p
    | _ -> assert false
  in
  let resources = [ id, `PublicKey, digest (Data.publickey_raw pub) ] in
  let jidx =
    let idx = Index.index ~resources jid in
    Private.sign_index idx jpriv
  in
  let r = Repository.add_trusted_key r jpub in
  Alcotest.check (result ok err) "not signed"
    (Error (`NotSigned (id, `PublicKey)))
    (Repository.verify_key r pub) ;
  let r' = Repository.add_index r jidx in
  Alcotest.check (result ok err) "empty publickey good"
    (Ok (`Quorum (S.singleton jid)))
    (Repository.verify_key r' pub)

let key_good () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub ~role:`Janitor jid in
  let id = "foo" in
  let pub, priv = gen_pub id in
  let resources = [ id, `PublicKey, digest (Data.publickey_raw pub) ] in
  let jidx =
    let idx = Index.index ~resources jid in
    Private.sign_index idx jpriv
  in
  let idx =
    let idx = Index.index ~resources id in
    Private.sign_index idx priv
  in
  let r = Repository.add_trusted_key r jpub in
  Alcotest.check (result ok err) "not signed"
    (Error (`NotSigned (id, `PublicKey)))
    (Repository.verify_key r pub) ;
  let r' = Repository.add_index r jidx in
  Alcotest.check (result ok err) "publickey missing self-sig"
    (Error (`MissingSignature id))
    (Repository.verify_key r' pub) ;
  let r'' = Repository.add_index r idx in
  Alcotest.check (result ok err) "publickey missing quorum"
    (Error (`InsufficientQuorum (id, S.empty)))
    (Repository.verify_key r'' pub) ;
  let r''' = Repository.add_index r' idx in
  Alcotest.check (result ok err) "publickey is fine"
    (Ok (`Both (id, S.singleton jid)))
    (Repository.verify_key r''' pub)

let no_janitor () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let id = "foo" in
  let pub, priv = gen_pub id in
  let id' = "bar" in
  let pem = match Publickey.publickey id' None with
    | Ok p -> p
    | _ -> assert false
  in
  let resources = [
    id, `PublicKey, digest (Data.publickey_raw pub) ;
    id', `PublicKey, digest (Data.publickey_raw pem)
  ] in
  let jidx =
    let idx = Index.index ~resources jid in
    Private.sign_index idx jpriv
  in
  let idx =
    let idx = Index.index ~resources id in
    Private.sign_index idx priv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  let r = Repository.add_index r idx in
  Alcotest.check (result ok err) "missing quorum"
    (Error (`InsufficientQuorum (id, S.empty)))
    (Repository.verify_key r pub) ;
  Alcotest.check (result ok err) "missing quorum for empty"
    (Error (`NotSigned (id', `PublicKey)))
    (Repository.verify_key r pem)

let wrong_resource () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let id = "foo" in
  let pub, priv = gen_pub id in
  let resources = [ id, `Checksum, digest (Data.publickey_raw pub) ] in
  let jidx =
    let idx = Index.index ~resources jid in
    Private.sign_index idx jpriv
  in
  let idx =
    let idx = Index.index ~resources id in
    Private.sign_index idx priv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  let r = Repository.add_index r idx in
  Alcotest.check (result ok err) "wrong resource"
    (Error (`InvalidResource (`PublicKey, `Checksum)))
    (Repository.verify_key r pub)

let wrong_name () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let jid = "aaa" in
  let jpub, jpriv = gen_pub jid in
  let id = "foo" in
  let pub, priv = gen_pub id in
  let resources = [ jid, `PublicKey, digest (Data.publickey_raw pub) ] in
  let jidx =
    let idx = Index.index ~resources jid in
    Private.sign_index idx jpriv
  in
  let idx =
    let idx = Index.index ~resources id in
    Private.sign_index idx priv
  in
  let r = Repository.add_trusted_key r jpub in
  let r = Repository.add_index r jidx in
  let r = Repository.add_index r idx in
  Alcotest.check (result ok err) "wrong name"
    (Error (`InvalidName (id, jid)))
    (Repository.verify_key r pub)

(*
let ji_key_r () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let sign_ji ji priv =
    let raw = Data.index_raw ji in
    let s = Private.sign "foo" priv `JanitorIndex raw in
    Index.add_sig ji s
  in
  let k, pk = gen_pub ~role:`Janitor "foo" in
  let ak, apk = gen_pub "foobar" in
  let raw = Data.publickey_raw ak in
  let resources = ["foobar", `PublicKey, digest raw] in
  let ji = Index.index ~resources "foo" in
  let ji = sign_ji ji pk in
  let ak =
    Publickey.add_sig ak (Private.sign "foobar" apk `PublicKey raw)
  in
  let r = Repository.add_trusted_key r k in
  Alcotest.check (result ok err) "janitorindex verifies good"
    (Ok (`Identifier "foo")) (Repository.verify_index r ji) ;
  Alcotest.check (result ok err) "ak does not verify"
    (Error (`InsufficientQuorum ("foobar", [])))
    (Repository.verify_key r ak) ;
  Repository.write_key r k ; (* TODO: atm needed, but maybe get rid of this *)
  Repository.write_index r ji ;
  let r = Repository.load_janitor ~verify:true r "foo" in
  Alcotest.check (result ok err) "ak verifies"
    (Ok (`Both ("foobar", ["foo"])))
    (Repository.verify_key r ak)

let auth_rel_key_r () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  let sign_ji ji priv =
    let raw = Data.index_raw ji in
    let s = Private.sign "foo" priv `JanitorIndex raw in
    Index.add_sig ji s
  in
  let k, pk = gen_pub ~role:`Janitor "foo" in
  let ak, apk = gen_pub "foobar" in
  let ak_raw = Data.publickey_raw ak in
  let auth = Authorisation.authorisation ~authorised:["foobar"] "barf" in
  let auth_raw = Data.authorisation_raw auth in
  let resources = [
    "foobar", `PublicKey, digest ak_raw ;
    "barf", `Authorisation, digest auth_raw ]
  in
  let ji = Index.index ~resources "foo" in
  let ji = sign_ji ji pk in
  let ak =
    Publickey.add_sig ak (Private.sign "foobar" apk `PublicKey ak_raw)
  in
  let releases =
    let r = Releases.releases "barf" in
    let raw = Data.releases_raw r in
    let s = Private.sign "foobar" apk `Releases raw in
    Releases.add_sig r s
  in
  Repository.write_authorisation r auth ;
  Repository.write_releases r releases ;
  Repository.write_key r k ; (* TODO: atm needed, but maybe get rid of this *)
  Repository.write_key r ak ;
  Repository.write_index r ji ;
  Alcotest.(check (list string) "authorisations list is non-empty"
    ["barf"] (Repository.all_authorisations r)) ;
  let r = Repository.add_trusted_key r k in
  let r = Repository.load_janitors r in (* TODO: atm needed, but maybe get rid of this *)
  Alcotest.check (result ok err) "key of foobar verifies"
    (Ok (`Both ("foobar", ["foo"])))
    (Repository.verify_key r ak) ;
  Alcotest.check (result ok err) "auth verifies"
    (Ok (`Quorum ["foo"]))
    (Repository.verify_authorisation r auth) ;
  let r = Repository.load_keys ~verify:true r ["foobar"] in
  Alcotest.check (result ok err) "releases verifies"
    (Ok (`Identifier "foobar"))
    (Repository.verify_releases r auth releases)
*)


let checks_r () =
  let open Provider in
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:1 p in
  p.write ["data"; "foo"; "foo.0"; "foo"] "bar" ;
  p.write ["data"; "foo"; "foo.0"; "bar"] "foo" ;
  p.write ["data"; "foo"; "foo.0"; "files"; "patch1"] "p1" ;
  p.write ["data"; "foo"; "foo.0"; "files"; "patch2"] "p2" ;
  (* manually crafted using echo -n XXX | openssl dgst -sha256 -binary | b64encode -m - *)
  let csums = [
    { Checksum.filename = "bar" ; bytesize = 3L ; checksum = "LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm564=" } ;
    { Checksum.filename = "foo" ; bytesize = 3L ; checksum = "/N4rLtula/QIYB+3If6bXDONEO5CnqBPrlURto+/j7k=" } ;
    { Checksum.filename = "files/patch1" ; bytesize = 2L ; checksum = "9kVR/NbweCPLh5cc+5FEZCXaGChrOrHvk14MvXpp9oo=" } ;
    { Checksum.filename = "files/patch2" ; bytesize = 2L ; checksum = "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=" }
  ]
  in
  let css = Checksum.checksums "foo.0" csums in
  Alcotest.check (result cs re) "checksum computation works"
    (Ok css) (Repository.compute_checksum r "foo.0")

let repo_tests = [
  "empty repo", `Quick, empty_r ;
  "key repo", `Quick, key_r ;
  "signed index", `Quick, idx_sign ;
  "empty key", `Quick, empty_key ;
  "key good", `Quick, key_good ;
  "no janitor", `Quick, no_janitor ;
  "wrong resource", `Quick, wrong_resource ;
  "wrong name", `Quick, wrong_name ;
(*  "signed key repo", `Quick, key_sign_r ;
  "signed ji repo", `Quick, empty_ji_r ;
  "key in signed ji repo", `Quick, ji_key_r ;
    "releases and authorisation in repo", `Quick, auth_rel_key_r ; *)
  "checksum computation is sane", `Quick, checks_r ;
]

let tests = [
  "Memory provider", mem_provider_tests ;
  "Repository", repo_tests ;
]
