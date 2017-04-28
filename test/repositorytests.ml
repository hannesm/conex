open Conex_utils
open Conex_resource
open Conex_io

open Common

module Mem = struct
  type tree = Leaf of string * string | Node of string * tree list

  let name = function Leaf (x, _) -> x | Node (x, _) -> x

  let find_e x es =
    try Some (List.find (fun n -> name n = x) es) with Not_found -> None

  let rec find t path =
    match t, path with
    | x, [p] when name x = p -> Ok x
    | _, [_] -> Error "couldn't find what you were looking for"
    | Node (_, xs), _::p'::ps ->
      (match find_e p' xs with
       | None -> Error "not found what you're looking for"
       | Some x -> find x (p'::ps))
    | _ -> Error "couldn't find"

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
    | _ -> Alcotest.fail "should not happen"

  let find_f t path = find t ("/"::path)

  let ins t path v = insert t ("/"::path) v

  let mem_provider () : Conex_io.t =
    let root = ref (Node ("/", [])) in
    let file_type path =
      find_f !root path >>= function
      | Leaf _ -> Ok File
      | Node _ -> Ok Directory
    and read path =
      find_f !root path >>= function
      | Leaf (_, v) -> Ok v
      | _ -> Error "couldn't find what was searched for"
    and write path data =
      let r = ins !root path data in
      root := r ;
      Ok ()
    and read_dir path =
      find_f !root path >>= function
      | Node (_, ch) -> Ok (List.map (function Node (x, _) -> Directory, x | Leaf (x, _) -> File, x) ch)
      | Leaf _ -> Error "reading a directory, but found files"
    and exists path =
      match find_f !root path with Ok _ -> true | Error _ -> false
    in
    { basedir = "mem" ; description = "Memory provider" ; file_type ; read ; write ;read_dir ; exists }
end

let str_err =
  let module M = struct
    type t = string
    let pp ppf x = Format.pp_print_string ppf x
    let equal _ _ = true
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
    type t = Conex_utils.item
    let pp ppf = function
      | File, f -> Format.fprintf ppf "file %s" f
      | Directory, d -> Format.fprintf ppf "directory %s" d
    let equal a b = match a, b with
      | (File, f), (File, g) -> f = g
      | (Directory, a), (Directory, b) -> a = b
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let empty_p () =
  let p = Mem.mem_provider () in
  Alcotest.check Alcotest.bool "no 'foo' in empty mem store" false (p.exists ["foo"]) ;
  Alcotest.check (result Alcotest.string str_err) "reading 'foo' in empty mem store" (Error "") (p.read ["foo"]) ;
  Alcotest.check (result ft str_err) "file-type of 'foo' in empty mem store" (Error "") (p.file_type ["foo"]) ;
  Alcotest.check (result (Alcotest.list it) str_err) "read_dir of 'foo' in empty mem store" (Error "") (p.read_dir ["foo"])

let basic_p () =
  let p = Mem.mem_provider () in
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar' to 'foo'"
    (Ok ()) (p.write ["foo"] "bar") ;
  Alcotest.check Alcotest.bool "foo in basic store" true (p.exists ["foo"]) ;
  Alcotest.check Alcotest.bool "foo/bar not in basic store" false (p.exists ["foo";"bar"]) ;
  Alcotest.check Alcotest.bool "foobar not in basic store" false (p.exists ["foobar"]) ;
  Alcotest.check (result Alcotest.string str_err) "foo contains bar in simple store" (Ok "bar") (p.read ["foo"]) ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'barf' to 'foo"
    (Ok ()) (p.write ["foo"] "barf") ;
  Alcotest.check (result Alcotest.string str_err) "foo contains barf in simple store" (Ok "barf") (p.read ["foo"]) ;
  Alcotest.check (result (Alcotest.list it) str_err) "read_dir of 'foo' in simple store" (Error "") (p.read_dir ["foo"])

let more_p () =
  let p = Mem.mem_provider () in
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar' to 'packages/foo'"
    (Ok ()) (p.write ["packages"; "foo"] "bar") ;
  Alcotest.check Alcotest.bool "packages in more store" true (p.exists ["packages"]) ;
  Alcotest.check Alcotest.bool "packages/foo in more store" true (p.exists ["packages"; "foo"]) ;
  Alcotest.check Alcotest.bool "packages/foo/bar not in more store" false (p.exists ["packages"; "foo"; "bar"]) ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar2' to 'packages/foo2'"
    (Ok ()) (p.write ["packages"; "foo2"] "bar2") ;
  Alcotest.check (result (Alcotest.list it) str_err) "read_dir of 'packages' in more mem store"
    (Ok [ File, "foo2" ; File, "foo" ]) (p.read_dir ["packages"]) ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar3' to 'packages/foo3'"
    (Ok ()) (p.write ["packages"; "foo3"] "bar3") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar4' to 'packages/foo4'"
    (Ok ()) (p.write ["packages"; "foo4"] "bar4") ;
  Alcotest.check (result (Alcotest.list it) str_err) "read_dir of 'packages' in even more mem store"
    (Ok [ File, "foo4" ; File, "foo3" ; File, "foo2" ; File, "foo" ]) (p.read_dir ["packages"]) ;
  Alcotest.check (result (Alcotest.list it) str_err) "read_dir of 'packages2' in even more mem store"
    (Error "") (p.read_dir ["packages2"]) ;
  Alcotest.check (result Alcotest.string str_err) "foo contains bar in more store"
    (Ok "bar") (p.read ["packages";"foo"]) ;
  Alcotest.check (result Alcotest.string str_err) "foo2 contains bar2 in more store"
    (Ok "bar2") (p.read ["packages"; "foo2"]) ;
  Alcotest.check (result Alcotest.string str_err) "foo3 contains bar3 in more store"
    (Ok "bar3") (p.read ["packages"; "foo3"]) ;
  Alcotest.check (result Alcotest.string str_err) "foo4 contains bar4 in more store"
    (Ok "bar4") (p.read ["packages"; "foo4"]) ;
  Alcotest.check (result Alcotest.string str_err) "foo5 not contained in more store"
    (Error "") (p.read ["packages"; "foo5"]) ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foobar' to 'packages/foobar/barfoo/foobar/foo'"
    (Ok ()) (p.write ["packages"; "foobar" ; "barfoo" ; "foobar" ; "foo"] "foobar") ;
  Alcotest.check (result Alcotest.string str_err) "packages/foobar/barfoo/foobar/foo contained in more store"
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
    type t = Conex_io.r_err
    let pp = Conex_io.pp_r_err
    let equal a b = match a, b with
      | `NotFound (t, a), `NotFound (t', a') -> typ_equal t t' && name_equal a a'
      | `NameMismatch (t, a, b), `NameMismatch (t', a', b') -> typ_equal t t' && name_equal a a' && name_equal b b'
      | `ParseError (t, n, _), `ParseError (t', n', _) -> typ_equal t t' && name_equal n n'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let ch_err =
  let module M = struct
    type t = Conex_io.cc_err
    let pp = Conex_io.pp_cc_err
    let equal a b = match a, b with
      | `FileNotFound a, `FileNotFound a' -> name_equal a a'
      | `NotADirectory a, `NotADirectory a' -> name_equal a a'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let io_ex f = match f with Error e -> Alcotest.fail e | Ok x -> x

module RTest (V : Conex_verify.S) = struct
(* basic operations work, and we have an in-memory data provider!  let the games begin *)
let empty_r () =
  let io = Mem.mem_provider () in
  Alcotest.check (result sset str_err) "empty repo errors on ids"
    (Error "") (Conex_io.ids io) ;
  Alcotest.check (result sset str_err) "empty repo errors on packages"
    (Error "") (Conex_io.packages io) ;
  Alcotest.check (result sset str_err) "empty repo errors on releases"
    (Error "") (Conex_io.releases io "foo") ;
  Alcotest.check (result auth re) "reading authorisation foo in empty repo fails"
    (Error (`NotFound (`Authorisation, "foo"))) (Conex_io.read_authorisation io "foo") ;
  Alcotest.check (result rels re) "reading releases foo in empty repo fails"
    (Error (`NotFound (`Releases, "foo"))) (Conex_io.read_releases io "foo") ;
  Alcotest.check (result ji re) "reading janitorindex foo in empty repo fails"
    (Error (`NotFound (`Author, "foo"))) (Conex_io.read_author io "foo") ;
  Alcotest.check (result css re) "reading checksum foo.0 in empty repo fails"
    (Error (`NotFound (`Checksums, "foo.0"))) (Conex_io.read_checksums io "foo.0") ;
  Alcotest.check (result css ch_err) "computing checksum foo.0 in empty repo fails"
    (Error (`FileNotFound "foo.0")) (Conex_io.compute_checksums V.raw_digest io Uint.zero "foo.0")

let key_r () =
  let io = Mem.mem_provider () in
  let k = Author.t Uint.zero "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing index 'foo'"
    (Ok ()) (Conex_io.write_author io k) ;
  Alcotest.check sset "key repo has one key"
    (S.singleton "foo") (io_ex (Conex_io.ids io)) ;
  Alcotest.check (result Alcotest.unit str_err) "writing index 'foo' again"
    (Ok ()) (Conex_io.write_author io k) ;
  Alcotest.check sset "key repo+ has one key"
    (S.singleton "foo") (io_ex (Conex_io.ids io)) ;
  let k2 = Author.t Uint.zero "foobar" in
  Alcotest.check (result Alcotest.unit str_err) "writing index 'foobar'"
    (Ok ()) (Conex_io.write_author io k2) ;
  Alcotest.check sset "key repo has two keys"
    (S.add "foobar" (S.singleton "foo")) (io_ex (Conex_io.ids io)) ;
  let jk = Author.t Uint.zero "janitor" in
  Alcotest.check (result Alcotest.unit str_err) "writing index 'janitor'"
    (Ok ()) (Conex_io.write_author io jk) ;
  Alcotest.check sset "key repo has three keys"
    (S.add "janitor" (S.add "foobar" (S.singleton "foo")))
    (io_ex (Conex_io.ids io)) ;
  Alcotest.check (result ji re) "reading author 'janitor'"
    (Ok jk) (Conex_io.read_author io "janitor")


let team_r () =
  let r = Conex_repository.repository ~quorum:10 V.digest () in
  Alcotest.(check int "quorum is correct" 10 (Conex_repository.quorum r)) ;
  Alcotest.(check (option sset) "team foo is empty" None (Conex_repository.find_team r "foo")) ;
  let t = Team.t ~members:(S.singleton "bar") Uint.zero "foo" in
  let r = Conex_repository.add_team r t in
  Alcotest.(check (option sset) "team foo has one member"
              (Some (S.singleton "bar")) (Conex_repository.find_team r "foo"))

let checks_r () =
  let io = Mem.mem_provider () in
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar' to 'packages/foo/foo.0/foo'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "foo"] "bar") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foo' to 'packages/foo/foo.0/bar'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "bar"] "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'p1' to 'packages/foo/foo.0/files/patch1'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "files"; "patch1"] "p1") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'p2' to 'packages/foo/foo.0/files/patch2'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "files"; "patch2"] "p2") ;
  (* manually crafted using echo -n XXX | openssl dgst -sha256 -hex *)
  let csums = [
    { Checksums.filename = "bar" ; digest = (`SHA256, "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae") } ;
    { Checksums.filename = "foo" ; digest = (`SHA256, "fcde2b2edba56bf408601fb721fe9b5c338d10ee429ea04fae5511b68fbf8fb9") } ;
    { Checksums.filename = "files/patch1" ; digest = (`SHA256, "f64551fcd6f07823cb87971cfb91446425da18286b3ab1ef935e0cbd7a69f68a") } ;
    { Checksums.filename = "files/patch2" ; digest = (`SHA256, "3946ca64ff78d93ca61090a437cbb6b3d2ca0d488f5f9ccf3059608368b27693") }
  ]
  in
  let css' = Checksums.t Uint.zero "foo.0" csums in
  Alcotest.check (result css ch_err) "checksum computation works"
    (Ok css') (Conex_io.compute_checksums V.raw_digest io Uint.zero "foo.0")

let key =
  let module M = struct
    type t = Key.t
    let pp = Key.pp
    let equal = Key.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let typ =
  let module M = struct
    type t = typ
    let pp = pp_typ
    let equal = typ_equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let signature =
  let module M = struct
    type t = Signature.t
    let pp = Signature.pp
    let equal ((alg, c), data) ((alg', c'), data') = alg = alg' && String.compare data data' = 0 && Uint.compare c c' = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let digest =
  let module M = struct
    type t = Digest.t
    let pp = Digest.pp
    let equal = Digest.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let basic_persistency () =
  let open Wire in
  Alcotest.check (result team str_err) "couldn't parse team"
    (Error "") (Team.of_wire M.empty) ;
  Alcotest.check (result typ str_err) "couldn't parse typ"
    (Error "") (typ_of_wire (Identifier "frabs")) ;
  Alcotest.check (result typ str_err) "couldn't parse typ"
    (Error "") (typ_of_wire (Data "key")) ;
  Alcotest.check (result typ str_err) "couldn't parse typ"
    (Error "") (typ_of_wire (Int Uint.zero)) ;
  Alcotest.check (result signature str_err) "couldn't parse signature"
    (Error "") (Signature.of_wire (List [ Int Uint.zero ; Data "foo" ; Data "baz" ])) ;
  Alcotest.check (result signature str_err) "couldn't parse signature"
    (Error "") (Signature.of_wire (List [ Int Uint.zero ; Identifier "foo" ; Data "baz" ])) ;
  Alcotest.check (result signature str_err) "couldn't parse signature"
    (Error "") (Signature.of_wire (List [ Int Uint.zero ; Data "foo" ; Identifier "baz" ])) ;
  Alcotest.check (result signature str_err) "couldn't parse signature"
    (Error "") (Signature.of_wire (List [ Int Uint.zero ; Identifier "foo" ; Identifier "baz" ])) ;
  Alcotest.check (result signature str_err) "couldn't parse signature"
    (Error "") (Signature.of_wire (List [ Data "foo" ; Identifier "foo" ; Int Uint.zero ])) ;
  Alcotest.check (result signature str_err) "couldn't parse signature"
    (Error "") (Signature.of_wire (List [ Int Uint.zero ; Int Uint.zero ; Int Uint.zero ])) ;
  Alcotest.check (result signature str_err) "could parse signature"
    (Ok ((`RSA_PSS_SHA256, Uint.zero), "frab"))
    (Signature.of_wire (List [ Int Uint.zero ; Identifier "RSA-PSS-SHA256" ; Data "frab" ])) ;
  Alcotest.check (result digest str_err) "couldn't parse digest"
    (Error "") (Digest.of_wire (List [ Int Uint.zero ; Data "foobar" ])) ;
  Alcotest.check (result digest str_err) "couldn't parse digest"
    (Error "") (Digest.of_wire (List [ Data "bar" ; Data "foobar" ])) ;
  Alcotest.check (result digest str_err) "couldn't parse digest"
    (Error "") (Digest.of_wire (List [ Data "bar" ; Int Uint.zero ])) ;
  Alcotest.check (result digest str_err) "could parse digest"
    (Ok (`SHA256, "1234")) (Digest.of_wire (Data "sha256=1234")) ;
  let bad_key = List [Int Uint.zero ; Data "foo" ; Data "bar"] in
  Alcotest.check (result key str_err) "couldn't parse key"
    (Error "") (Key.of_wire bad_key) ;
  let bad_key = List [Data "foobar" ; Data "foo" ; Int Uint.zero] in
  Alcotest.check (result key str_err) "couldn't parse key (bad typ)"
    (Error "") (Key.of_wire bad_key) ;
  let bad_c = M.add "counter" (Data "foo") M.empty in
  Alcotest.check (result team str_err) "couldn't parse team counter"
    (Error "") (Team.of_wire bad_c) ;
  let bad_n =
    M.add "name" (Int Uint.zero)
      (M.add "counter" (Int Uint.zero)
         (M.add "version" (Int Uint.zero)
            M.empty))
  in
  Alcotest.check (result team str_err) "couldn't parse team name"
    (Error "") (Team.of_wire bad_n) ;
  let bad_n =
    M.add "name" (Data "foo") bad_n
  in
  Alcotest.check (result team str_err) "couldn't parse team (type, epoch missing)"
    (Error "") (Team.of_wire bad_n) ;
  let bad_n =
    M.add "typ" (Int Uint.zero) bad_n
  in
  Alcotest.check (result team str_err) "couldn't parse team (bad type, epoch missing)"
    (Error "") (Team.of_wire bad_n) ;
  let bad_n =
    M.add "typ" (Identifier "team") bad_n
  in
  Alcotest.check (result team str_err) "couldn't parse team (epoch missing)"
    (Error "") (Team.of_wire bad_n) ;
  let good_n =
    M.add "epoch" (Int Uint.zero) (M.add "created" (Int Uint.zero) bad_n)
  in
  let t = Team.t Uint.zero "foo" in
  Alcotest.check (result team str_err) "could parse team"
    (Ok t) (Team.of_wire good_n) ;
  let bad_n = M.add "typ" (Identifier "author") good_n in
  Alcotest.check (result team str_err) "couldn't parse team (wrong typ)"
    (Error "") (Team.of_wire bad_n) ;
  let bad_n = M.add "version" (Int Uint.(of_int_exn 23)) good_n in
  Alcotest.check (result team str_err) "couldn't parse team (wrong version)"
    (Error "") (Team.of_wire bad_n) ;
  let bad_m =
    M.add "members" (Data "bl") good_n
  in
  Alcotest.check (result team str_err) "couldn't parse team members"
    (Error "") (Team.of_wire bad_m) ;
  let bad_m =
    M.add "members" (List [ Int Uint.zero ])
      (M.add "name" (Identifier "foo") good_n)
  in
  Alcotest.check (result team str_err) "couldn't parse team members (not a set)"
    (Error "") (Team.of_wire bad_m) ;
  let good_t =
    M.add "members" (List [ Data "foo" ]) good_n
  in
  let t = Team.t ~members:(S.singleton "foo") Uint.zero "foo" in
  Alcotest.check (result team str_err) "could parse team"
    (Ok t) (Team.of_wire good_t) ;
  Alcotest.check (result team str_err) "could unparse/parse team"
    (Ok t) (Team.of_wire (Team.wire t)) ;
  Alcotest.check (result team str_err) "could unparse/parse team"
    (Ok t) (Conex_opam_encoding.decode (Conex_opam_encoding.encode (Team.wire t)) >>= Team.of_wire) ;
  let checksum =
    List [
      Data "foo" ;
      Data "sha256=3234c3275955af97d5def935a26098933c303f02813497d6b96d6aef9afe362f"
    ]
  in
  let css' =
    M.add "name" (Data "foo")
      (M.add "counter" (Int Uint.zero)
         (M.add "epoch" (Int Uint.zero)
            (M.add "created" (Int Uint.zero)
               (M.add "version" (Int Uint.zero)
                  (M.add "typ" (Identifier "checksums")
                     (M.add "files" (List [checksum]) M.empty))))))
  in
  let csum =
    let digest = V.digest Author.(wire (t Uint.zero "bar")) in
    { Checksums.filename = "foo" ; digest }
  in
  let csums = Checksums.t Uint.zero "foo" [csum] in
  Alcotest.check (result css str_err) "can parse checksum"
    (Ok csums) (Checksums.of_wire css') ;

  let s = List [ Int Uint.zero ; Identifier (Signature.alg_to_string `RSA_PSS_SHA256) ; Data "barf" ] in
  let pub = List [ Identifier "RSA" ; Data "data" ; Int Uint.zero ] in
  let pub' = (`RSA, "data", Uint.zero) in
  let s' = ((`RSA_PSS_SHA256, Uint.zero), "frab") in
  let idx =
    M.add "name" (Data "foo")
      (M.add "counter" (Int Uint.zero)
         (M.add "created" (Int Uint.zero)
            (M.add "epoch" (Int Uint.zero)
               (M.add "typ" (Identifier "author")
                  (M.add "version" (Int Uint.zero) M.empty)))))
  in
  let empty_idx =
    M.add "queued" (List [])
      (M.add "keys" (List [List [pub;s]]) M.empty)
  in
  let idxs =
    M.add "signed" (Map idx) empty_idx
  in

  let idx' = Author.t ~keys:[pub',s'] Uint.zero "foo" in
  Alcotest.check (result ji str_err) "can parse index"
    (Ok idx') (Author.of_wire idxs) ;
  Alcotest.check (result ji str_err) "can unparse/parse index"
    (Ok idx') (Author.of_wire (Author.wire idx')) ;
  let r =
    M.add "index" (Int Uint.zero)
      (M.add "name" (Data "foobar")
         (M.add "typ" (Identifier "team")
            (M.add "digest" (Data "sha256=01234567890123456789012345678901234567890123")
               M.empty)))
  in
  let r' = Author.r Uint.zero "foobar" `Team (`SHA256, "01234567890123456789012345678901234567890123") in
  let idx = M.add "resources" (List [ Map r ]) idx in
  let idxs =
    M.add "signed" (Map idx) empty_idx
  in
  let idx' = Author.t ~keys:[pub',s'] ~resources:[r'] Uint.zero "foo" in
  Alcotest.check (result ji str_err) "can parse index"
    (Ok idx') (Author.of_wire idxs) ;
  Alcotest.check (result ji str_err) "can unparse/parse index"
    (Ok idx') (Author.of_wire (Author.wire idx')) ;
  let bad_r = M.add "typ" (Identifier "teamfoo") r in
  let idx = M.add "resources" (List [ Map bad_r ]) idx in
  let idxs =
    M.add "signed" (Map idx) empty_idx
  in
  Alcotest.check (result ji str_err) "cannot parse index, bad resource type"
    (Error "") (Author.of_wire idxs)

let bad_id_r () =
  let io = Mem.mem_provider () in
  Alcotest.check (result ji re) "index foo not found"
    (Error (`NotFound (`Author, "foo"))) (Conex_io.read_author io "foo") ;
  Alcotest.check (result team re) "team foo not found"
    (Error (`NotFound (`Team, "foo"))) (Conex_io.read_team io "foo") ;
  Alcotest.check (result id re) "ID foo not found"
    (Error (`NotFound (`Author, "foo"))) (Conex_io.read_id io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'barf' to 'id/foo'"
    (Ok ()) (io.write ["id"; "foo"] "barf") ;
  Alcotest.check (result ji re) "parse error on index foo"
    (Error (`ParseError (`Author, "foo", ""))) (Conex_io.read_author io "foo") ;
  Alcotest.check (result team re) "parse error on team foo"
    (Error (`ParseError (`Team, "foo", ""))) (Conex_io.read_team io "foo") ;
  Alcotest.check (result id re) "parse error on id foo"
    (Error (`ParseError (`Author, "foo", ""))) (Conex_io.read_id io "foo") ;
  let idx = Author.t Uint.zero "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing index 'foo'"
    (Ok ()) (Conex_io.write_author io idx) ;
  Alcotest.check (result team re) "parse error on team foo"
    (Error (`ParseError (`Team, "foo", ""))) (Conex_io.read_team io "foo") ;
  Alcotest.check (result ji re) "author foo parses"
    (Ok idx) (Conex_io.read_author io "foo") ;
  Alcotest.check (result id re) "id foo parses"
    (Ok (`Author idx)) (Conex_io.read_id io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'id/foobar'"
    (Ok ()) (io.write ["id"; "foobar"] (Conex_opam_encoding.encode (Author.wire idx))) ;
  Alcotest.check (result team re) "parse error on team foobar"
    (Error (`ParseError (`Team, "foobar", ""))) (Conex_io.read_team io "foobar") ;
  Alcotest.check (result ji re) "id foobar namemismatch"
    (Error (`NameMismatch (`Author, "foobar", "foo"))) (Conex_io.read_author io "foobar") ;
  Alcotest.check (result id re) "namemismatch id foobar"
    (Error (`NameMismatch (`Author, "foobar", "foo"))) (Conex_io.read_id io "foobar") ;
  let t = Team.t Uint.zero "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing team 'foo'"
    (Ok ()) (Conex_io.write_team io t) ;
  Alcotest.check (result ji re) "parse error on index foo"
    (Error (`ParseError (`Author, "foo", ""))) (Conex_io.read_author io "foo") ;
  Alcotest.check (result team re) "team foo parses"
    (Ok t) (Conex_io.read_team io "foo") ;
  Alcotest.check (result id re) "id foo parses"
    (Ok (`Team t)) (Conex_io.read_id io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'id/foobar'"
    (Ok ()) (io.write ["id"; "foobar"] (Conex_opam_encoding.encode (Team.wire t))) ;
  Alcotest.check (result ji re) "parse error on index foobar"
    (Error (`ParseError (`Author, "foobar", ""))) (Conex_io.read_author io "foobar") ;
  Alcotest.check (result team re) "name mismatch on team foobar"
    (Error (`NameMismatch (`Team, "foobar", "foo"))) (Conex_io.read_team io "foobar") ;
  Alcotest.check (result id re) "name mismatch on id foo (parse error in author)"
    (Error (`ParseError (`Author, "foobar", ""))) (Conex_io.read_id io "foobar")

let bad_idx_r () =
  let io = Mem.mem_provider () in
  Alcotest.check (result ji re) "author foo not found"
    (Error (`NotFound (`Author, "foo"))) (Conex_io.read_author io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'bla' to 'id/foo'"
    (Ok ()) (io.write ["id"; "foo"] "bla") ;
  Alcotest.check (result ji re) "bad author foo"
    (Error (`ParseError (`Author, "foo", ""))) (Conex_io.read_author io "foo") ;
  let idx = Author.t Uint.zero "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing author 'foo'"
    (Ok ()) (Conex_io.write_author io idx) ;
  Alcotest.check (result ji re) "good author foo"
    (Ok idx) (Conex_io.read_author io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'id/foobar'"
    (Ok ()) (io.write ["id"; "foobar"] (Conex_opam_encoding.encode (Author.wire idx))) ;
  Alcotest.check (result ji re) "name mismatch in foobar"
    (Error (`NameMismatch (`Author, "foobar", "foo"))) (Conex_io.read_author io "foobar")

let bad_auth_r () =
  let io = Mem.mem_provider () in
  Alcotest.check (result auth re) "authorisation foo not found"
    (Error (`NotFound (`Authorisation, "foo"))) (Conex_io.read_authorisation io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foobar' to 'packages/foo/authorisation'"
    (Ok ()) (io.write ["packages"; "foo"; "authorisation"] "foobar") ;
  Alcotest.check (result auth re) "parse error on authorisation foo"
    (Error (`ParseError (`Authorisation, "foo", ""))) (Conex_io.read_authorisation io "foo") ;
  let a = Authorisation.t Uint.zero "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing authorisation 'foo'"
    (Ok ()) (Conex_io.write_authorisation io a) ;
  Alcotest.check (result auth re) "authorisation foo good"
    (Ok a) (Conex_io.read_authorisation io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'packages/foobar/authorisation'"
    (Ok ()) (io.write ["packages"; "foobar"; "authorisation"] (Conex_opam_encoding.encode (Authorisation.wire a))) ;
  Alcotest.check (result auth re) "name mismatch on authorisation foobar"
    (Error (`NameMismatch (`Authorisation, "foobar", "foo"))) (Conex_io.read_authorisation io "foobar")

let bad_rel_r () =
  let io = Mem.mem_provider () in
  Alcotest.check (result rels re) "releases of package foo not found"
    (Error (`NotFound (`Releases, "foo"))) (Conex_io.read_releases io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foobar' to 'packages/foo/releases'"
    (Ok ()) (io.write ["packages"; "foo"; "releases"] "foobar") ;
  Alcotest.check (result rels re) "parse error on package releases foo"
    (Error (`ParseError (`Releases, "foo", ""))) (Conex_io.read_releases io "foo") ;
  let rel = Releases.t Uint.zero "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing package releases 'foo'"
    (Ok ()) (Conex_io.write_releases io rel) ;
  Alcotest.check (result rels re) "package releases foo good"
    (Ok rel) (Conex_io.read_releases io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'packages/foobar/releases'"
    (Ok ()) (io.write ["packages"; "foobar"; "releases"] (Conex_opam_encoding.encode (Releases.wire rel))) ;
  Alcotest.check (result rels re) "name mismatch on pacakge foobar"
    (Error (`NameMismatch (`Releases, "foobar", "foo"))) (Conex_io.read_releases io "foobar")

let bad_cs_r () =
  let io = Mem.mem_provider () in
  Alcotest.check (result css re) "package foo not found"
    (Error (`NotFound (`Checksums, "foo"))) (Conex_io.read_checksums io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foobar' to 'packages/foo/foo.0/checksums'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "checksums"] "foobar") ;
  Alcotest.check (result css re) "parse error on release foo.0"
    (Error (`ParseError (`Checksums, "foo.0", ""))) (Conex_io.read_checksums io "foo.0") ;
  let c = Checksums.t Uint.zero "foo.0" [] in
  Alcotest.check (result Alcotest.unit str_err) "writing release foo.0"
    (Ok ()) (Conex_io.write_checksums io c) ;
  Alcotest.check (result css re) "release foo.0 good"
    (Ok c) (Conex_io.read_checksums io "foo.0") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'packages/foo/foo.1/checksums'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.1"; "checksums"] (Conex_opam_encoding.encode (Checksums.wire c))) ;
  Alcotest.check (result css re) "name mismatch on release foo.1"
    (Error (`NameMismatch (`Checksums, "foo.1", "foo.0"))) (Conex_io.read_checksums io "foo.1") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'blubb' to 'packages/foo/foo.2'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.2"] "blubb") ;
  Alcotest.check (result css ch_err) "foo.2 is a file, should be a directory"
    (Error (`NotADirectory "foo.2")) (Conex_io.compute_checksums V.raw_digest io Uint.zero "foo.2")

let basic_repo_tests = [
  "empty repo", `Quick, empty_r ;
  "key repo", `Quick, key_r ;
  "team", `Quick, team_r ;
  "checksum computation is sane", `Quick, checks_r ;
  "basic data persistency", `Quick, basic_persistency ;
  "bad id in repo", `Quick, bad_id_r ;
  "bad idx in repo", `Quick, bad_idx_r ;
  "bad auth in repo", `Quick, bad_auth_r ;
  "bad rel in repo", `Quick, bad_rel_r ;
  "bad cs in repo", `Quick, bad_cs_r ;
]

let conf =
  let module M = struct
    type t = Conex_repository.conflict
    let pp = Conex_repository.pp_conflict
    let equal a b = match a, b with
      | `NameConflict (n, _), `NameConflict (n', _) -> name_equal n n'
      | `TypConflict (n, _), `TypConflict (n', _) -> typ_equal n n'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let r_fake =
  let module M = struct
    type t = Conex_repository.t
    let pp ppf _ = Format.fprintf ppf "repository"
    let equal _ _ = true
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let a_err =
  let module M = struct
    type t = [ Conex_repository.base_error | Conex_repository.conflict | `InsufficientQuorum of name * typ * S.t * int | `AuthorWithoutKeys of identifier  ]
    let pp ppf = function
      | #Conex_repository.conflict as c -> Conex_repository.pp_conflict ppf c
      | e -> Conex_repository.pp_error ppf e
    let equal a b = match a, b with
      | `NameConflict (n, _), `NameConflict (n', _) -> name_equal n n'
      | `TypConflict (n, _), `TypConflict (n', _) -> typ_equal n n'
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && typ_equal w w' && typ_equal h h'
      | `NotApproved (n, r, js), `NotApproved (n', r', js') -> name_equal n n' && typ_equal r r' && S.equal js js'
      | `InsufficientQuorum (id, r, q, req), `InsufficientQuorum (id', r', q', req') -> id_equal id id' && S.equal q q' && typ_equal r r' && req = req'
      | `AuthorWithoutKeys id, `AuthorWithoutKeys id' -> id_equal id id'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let idx_sign () =
  let r = Conex_repository.repository ~quorum:0 V.digest () in
  let id = "foo" in
  let pub, priv = gen_pub () in
  let idx = Author.t Uint.zero id in
  Alcotest.check (result r_fake a_err)
    "empty index signed properly (no resources, quorum 0)"
    (Ok r) (Conex_repository.validate_author r idx) ;
  let pubenc = Key.wire id pub in
  let res = Author.r (Author.next_id idx) id `Key (V.digest pubenc) in
  let idx' = Author.queue idx res in
  let signed_idx = sign_idx idx' priv in
  Alcotest.check (result Alcotest.unit verr) "idx digitally signed properly"
    (Ok ()) (V.verify signed_idx) ;
  Alcotest.check (result r_fake a_err) "signed_idx signed properly (1 resource, quorum 0)"
    (Ok r) (Conex_repository.validate_author r signed_idx) ;
  Alcotest.check (result r_fake a_err) "idx signed properly (0 resources, 1 queued, quorum 0)"
    (Ok r) (Conex_repository.validate_author r idx) ;
  Alcotest.check (result r_fake a_err) "idx not signed properly (1 resource, quorum 0)"
    (Error (`AuthorWithoutKeys "foo"))
    (Conex_repository.validate_author r (Author.approve idx' res))

let idx_sign_verify () =
  let r = Conex_repository.repository ~quorum:0 V.digest () in
  let id = "foo" in
  let pub, priv = gen_pub () in
  let d = V.digest (Key.wire id pub) in
  let res = Author.r Uint.zero id `Key d in
  let resources = [ res ] in
  let idx = Author.t ~resources Uint.zero id in
  let signed_idx = sign_idx idx priv in
  Alcotest.check (result r_fake a_err) "idx signed properly"
    (Ok r) (Conex_repository.validate_author r signed_idx) ;
  Alcotest.check (result Alcotest.unit verr) "idx digitally signed properly"
    (Ok ()) (V.verify signed_idx) ;
  let queued = [ Author.r Uint.zero id `Team d ] in
  let idx'' = Author.t ~keys:signed_idx.Author.keys ~resources ~queued ~counter:Uint.(of_int_exn 1) Uint.zero id in
  Alcotest.check (result Alcotest.unit verr) "idx'' digitally signed properly"
    (Ok ()) (V.verify idx'') ;
  Alcotest.check (result r_fake a_err) "idx'' properly signed (queue)"
    (Ok r) (Conex_repository.validate_author r idx'') ;
  let idx' = Author.t ~resources Uint.zero id in
  let signed_idx' = sign_idx idx' priv in
  Alcotest.check (result Alcotest.unit verr) "signed_idx' digitally signed properly"
    (Ok ()) (V.verify signed_idx') ;
  Alcotest.check (result r_fake a_err) "signed_idx' signed properly"
    (Ok r) (Conex_repository.validate_author r signed_idx') ;
  match Conex_repository.add_valid_resource id r (List.hd resources) with
  | Ok r ->
    Alcotest.check (result r_fake conf) "add errors on wrong name"
      (Error (`NameConflict ("foo", res)))
      (Conex_repository.add_valid_resource id r (Author.r Uint.zero "barf" `Key d)) ;
    Alcotest.check (result r_fake conf) "add errors on wrong resource"
      (Error (`TypConflict (`Key, res)))
      (Conex_repository.add_valid_resource id r (Author.r Uint.zero id `Team d))
  | Error _ -> Alcotest.fail "wrong"

let idx_s_v_dupl () =
  let r = Conex_repository.repository ~quorum:0 V.digest () in
  let id = "foo" in
  let pub, priv = gen_pub () in
  let d = V.digest (Key.wire id pub) in
  let res = Author.r Uint.zero id `Key d in
  let resources = [ res ; Author.r (Uint.of_int_exn 1) id `Team d ] in
  let idx = Author.t ~resources Uint.zero id in
  let signed_idx = sign_idx idx priv in
  Alcotest.check (result r_fake a_err) "idx signed properly, but one resource ignored"
    (Error (`TypConflict (`Key, res)))
    (Conex_repository.validate_author r signed_idx)

let k_ok =
  let module M = struct
    type t = [ `Both of identifier * S.t ]
    let pp = Conex_repository.pp_ok
    let equal a b = match a, b with
      | `Both (a, js), `Both (b, is) -> id_equal a b && S.equal js is
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let k_err =
  let module M = struct
    type t = [ Conex_repository.base_error | `InsufficientQuorum of name * typ * S.t * int ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && typ_equal w w' && typ_equal h h'
      | `NotApproved (n, r, js), `NotApproved (n', r', js') -> name_equal n n' && typ_equal r r' && S.equal js js'
      | `InsufficientQuorum (id, r, q, req), `InsufficientQuorum (id', r', q', req') -> id_equal id id' && S.equal q q' && typ_equal r r' && req = req'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let add_rs repo id rs =
  List.fold_left (fun repo res ->
      match Conex_repository.add_valid_resource id repo res with
      | Ok r -> r
      | Error _ -> Alcotest.fail "should not fail")
    repo rs

let key_good () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _ = gen_pub () in
  let resources =
    let d = V.digest (Key.wire id pub) in
    [ Author.r Uint.zero id `Key d ]
  in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  Alcotest.check (result k_ok k_err) "not signed"
    (Error (`NotApproved (id, `Key, S.empty)))
    (Conex_repository.validate_key r id pub) ;
  let r' = add_rs r jid resources in
  Alcotest.check (result k_ok k_err) "publickey missing self-sig"
    (Error (`NotApproved (id, `Key, S.empty)))
    (Conex_repository.validate_key r' id pub) ;
  let r'' = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "publickey missing quorum"
    (Error (`InsufficientQuorum (id, `Key, S.empty, 1)))
    (Conex_repository.validate_key r'' id pub) ;
  let r''' = add_rs r' id resources in
  Alcotest.check (result k_ok k_err) "publickey is fine"
    (Ok (`Both (id, S.singleton jid)))
    (Conex_repository.validate_key r''' id pub)

let key_good_quorum () =
  let r = Conex_repository.repository ~quorum:3 V.digest () in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let resources =
    let d = V.digest (Key.wire id pub) in
    [ Author.r Uint.zero id `Key d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 0"
    (Error (`InsufficientQuorum (id, `Key, S.empty, 3)))
    (Conex_repository.validate_key r id pub) ;
  let jidx r jid =
    let r =
      let mems = match Conex_repository.find_team r "janitors" with None -> S.empty | Some s -> s in
      Conex_repository.add_team r (Team.t ~members:(S.add jid mems) Uint.zero "janitors")
    in
    add_rs r jid resources
  in
  let r = jidx r "jana" in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 1"
    (Error (`InsufficientQuorum (id, `Key, S.singleton "jana", 3)))
    (Conex_repository.validate_key r id pub) ;
  let r = jidx r "janb" in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 2"
    (Error (`InsufficientQuorum (id, `Key, S.add "janb" (S.singleton "jana"), 3)))
    (Conex_repository.validate_key r id pub) ;
  let r = jidx r "janc" in
  Alcotest.check (result k_ok k_err) "publickey is fine"
    (Ok (`Both (id, S.add "janc" (S.add "janb" (S.singleton "jana")))))
    (Conex_repository.validate_key r id pub)

let no_janitor () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let id' = "bar" in
  let resources =
    let d = V.digest (Key.wire id pub)
    and d' = V.digest (Key.wire id' pub)
    in
    [ Author.r Uint.zero id `Key d ; Author.r (Uint.of_int_exn 1) id' `Key d' ]
  in
  let r = add_rs r jid resources in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "missing quorum"
    (Error (`InsufficientQuorum (id, `Key, S.empty, 1)))
    (Conex_repository.validate_key r id pub) ;
  Alcotest.check (result k_ok k_err) "missing quorum for empty"
    (Error (`NotApproved (id', `Key, S.empty)))
    (Conex_repository.validate_key r id' pub)

let k_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let resources =
    let d = V.digest (Key.wire id pub) in
    [ Author.r Uint.zero id `Releases d ]
  in
  let r = add_rs r jid resources in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "wrong resource"
    (Error (`InvalidResource (id, `Key, `Releases)))
    (Conex_repository.validate_key r id pub)

let k_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let resources =
    let d = V.digest (Key.wire id pub) in
    [ Author.r Uint.zero jid `Key d ]
  in
  let r = add_rs r jid resources in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "wrong name"
    (Error (`InvalidName (id, jid)))
    (Conex_repository.validate_key r id pub)

let key_repo_tests = [
  "signed index", `Quick, idx_sign ;
  "signed and verified index", `Quick, idx_sign_verify ;
  "verify succeeds, but ignores resource", `Quick, idx_s_v_dupl ;
  "key good", `Quick, key_good ;
  "key good with quorum = 3", `Quick, key_good_quorum ;
  "no janitor", `Quick, no_janitor ;
  "wrong resource", `Quick, k_wrong_resource ;
  "wrong name", `Quick, k_wrong_name ;
]


let t_ok =
  let module M = struct
    type t = Conex_repository.t * [ `Quorum of S.t ]
    let pp ppf (_, ok) = Conex_repository.pp_ok ppf ok
    let equal (_, a) (_, b) = match a, b with
      | `Quorum js, `Quorum is -> S.equal js is
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let a_err =
  let module M = struct
    type t = [ Conex_repository.base_error
             | `InsufficientQuorum of name * typ * S.t * int
             | `IdNotPresent of name * S.t
             | `MemberNotPresent of identifier * S.t ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InsufficientQuorum (id, r, q, req), `InsufficientQuorum (id', r', q', req') -> id_equal id id' && S.equal q q' && typ_equal r r' && req = req'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && typ_equal w w' && typ_equal h h'
      | `NotApproved (n, r, js), `NotApproved (n', r', js') -> name_equal n n' && typ_equal r r' && S.equal js js'
      | `IdNotPresent (n, s), `IdNotPresent (n', s') -> name_equal n n' && S.equal s s'
      | `MemberNotPresent (n, s), `MemberNotPresent (n', s') -> id_equal n n' && S.equal s s'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let team () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  Alcotest.check (result t_ok a_err) "team missing quorum"
    (Error (`InsufficientQuorum (pname, `Team, S.empty, 1)))
    (Conex_repository.validate_team r team) ;
  let resources =
    let d = V.digest (Team.wire team) in
    [ Author.r Uint.zero pname `Team d ]
  in
  let j_sign r jid =
    let r =
      let mems = match Conex_repository.find_team r "janitors" with None -> S.empty | Some s -> s in
      Conex_repository.add_team r (Team.t ~members:(S.add jid mems) Uint.zero "janitors")
    in
    add_rs r jid resources
  in
  let r = j_sign r "janitor" in
  Alcotest.check (result t_ok a_err) "team properly signed"
    (Ok (r, `Quorum (S.singleton "janitor")))
    (Conex_repository.validate_team r team) ;
  let r = Conex_repository.repository ~quorum:2 V.digest () in
  let r = j_sign r "janitor" in
  Alcotest.check (result t_ok a_err) "team missing quorum of 2"
    (Error (`InsufficientQuorum (pname, `Team, S.singleton "janitor", 2)))
    (Conex_repository.validate_team r team) ;
  let r = j_sign r "janitor2" in
  Alcotest.check (result t_ok a_err) "team quorum of 2 good"
    (Ok (r, `Quorum (S.add "janitor2" (S.singleton "janitor"))))
    (Conex_repository.validate_team r team)

let team_self_signed () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let id = "foo" in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  let pub, _priv = gen_pub () in
  let resources =
    let d = V.digest (Team.wire team)
    and d' = V.digest (Key.wire id pub)
    in
    [ Author.r Uint.zero pname `Team d ; Author.r (Uint.of_int_exn 1) id `Key d' ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result t_ok a_err) "team missing quorum"
    (Error (`InsufficientQuorum (pname, `Team, S.empty, 1)))
    (Conex_repository.validate_team r team) ;
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "team ok"
    (Ok (r, `Quorum (S.singleton jid)))
    (Conex_repository.validate_team r team)

let team_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  let resources =
    let d = V.digest (Team.wire team) in
    [ Author.r Uint.zero pname `Releases d ]
  in
  let jid = "aaa" in
  let r = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "wrong resource"
    (Error (`InvalidResource (pname, `Team, `Releases)))
    (Conex_repository.validate_team r team)

let team_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  let jid = "aaa" in
  let resources =
    let d = V.digest (Team.wire team) in
    [ Author.r Uint.zero "barf" `Team d ]
  in
  let r = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "wrong name"
    (Error (`InvalidName (pname, "barf")))
    (Conex_repository.validate_team r team)

let team_dyn () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors")
  in
  let resources =
    let d = V.digest (Team.wire team) in
    [ Author.r Uint.zero pname `Team d ]
  in
  let r' = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "team properly signed"
    (Ok (r', `Quorum (S.singleton jid)))
    (Conex_repository.validate_team r' team) ;
  let team = Team.add team "foobar" in
  Alcotest.check (result t_ok a_err) "team not properly signed"
    (Error (`InsufficientQuorum (pname, `Team, S.empty, 1)))
    (Conex_repository.validate_team r' team) ;
  let resources =
    let d = V.digest (Team.wire team) in
    [ Author.r Uint.zero pname `Team d ]
  in
  let r' = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "team properly signed, but missing member"
    (Error (`MemberNotPresent (pname, S.singleton "foobar")))
    (Conex_repository.validate_team r' team) ;
  let pub, priv = gen_pub () in
  let resources =
    let d' = V.digest (Key.wire "foobar" pub) in
    [ Author.r Uint.zero "foobar" `Key d' ]
  in
  let idx = Author.t ~resources Uint.zero "foobar" in
  let r' = add_rs r' jid resources in
  let signed = sign_idx idx priv in
  let r' = match Conex_repository.validate_author r' signed with
    | Error _ -> Alcotest.fail "should not fail"
    | Ok r -> r
  in
  Alcotest.check (result t_ok a_err) "team properly signed"
    (Ok (r', `Quorum (S.singleton jid)))
    (Conex_repository.validate_team r' team) ;
  let team = Team.remove team "foo" in
  Alcotest.check (result t_ok a_err) "team properly signed (nothing changed)"
    (Ok (r', `Quorum (S.singleton jid)))
    (Conex_repository.validate_team r' team) ;
  let team = Team.add team "foobar" in
  Alcotest.check (result t_ok a_err) "team properly signed (nothing changed)"
    (Ok (r', `Quorum (S.singleton jid)))
    (Conex_repository.validate_team r' team) ;
  let team = Team.add team "foobar42" in
  let resources =
    let d = V.digest (Team.wire team) in
    [ Author.r Uint.zero pname `Team d ]
  in
  let r' = add_rs r' jid resources in
  Alcotest.check (result t_ok a_err) "team properly signed, but missing member"
    (Error (`MemberNotPresent ("foop", S.singleton "foobar42")))
    (Conex_repository.validate_team r' team) ;
  let r' = Conex_repository.add_id r' "foobar42" in
  Alcotest.check (result t_ok a_err) "team properly signed"
    (Ok (r', `Quorum (S.singleton jid)))
    (Conex_repository.validate_team r' team) ;
  let team = Team.remove team "foobar" in
  Alcotest.check (result t_ok a_err) "team not properly signed (rm'ed, counter incr)"
    (Error (`InsufficientQuorum (pname, `Team, S.empty, 1)))
    (Conex_repository.validate_team r' team)

let team_repo_tests = [
  "basic team", `Quick, team ;
  "also self signed", `Quick, team_self_signed ;
  "wrong resource", `Quick, team_wrong_resource ;
  "wrong name", `Quick, team_wrong_name ;
  "dynamic team", `Quick, team_dyn
]

let a_ok =
  let module M = struct
    type t = [ `Quorum of S.t ]
    let pp = Conex_repository.pp_ok
    let equal a b = match a, b with
      | `Quorum js, `Quorum is -> S.equal js is
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)


let auth () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  Alcotest.check (result a_ok a_err) "auth missing quorum"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.empty, 1)))
    (Conex_repository.validate_authorisation r auth) ;
  let resources =
    let d = V.digest (Authorisation.wire auth) in
    [ Author.r Uint.zero pname `Authorisation d ]
  in
  let j_sign r jid =
    let r =
      let mems = match Conex_repository.find_team r "janitors" with None -> S.empty | Some s -> s in
      Conex_repository.add_team r (Team.t ~members:(S.add jid mems) Uint.zero "janitors")
    in
    add_rs r jid resources
  in
  let r = j_sign r "janitor" in
  Alcotest.check (result a_ok a_err) "auth properly signed"
    (Ok (`Quorum (S.singleton "janitor")))
    (Conex_repository.validate_authorisation r auth) ;
  let r = Conex_repository.repository ~quorum:2 V.digest () in
  let r = j_sign r "janitor" in
  Alcotest.check (result a_ok a_err) "auth missing quorum of 2"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.singleton "janitor", 2)))
    (Conex_repository.validate_authorisation r auth) ;
  let r = j_sign r "janitor2" in
  Alcotest.check (result a_ok a_err) "auth quorum of 2 good"
    (Ok (`Quorum (S.add "janitor2" (S.singleton "janitor"))))
    (Conex_repository.validate_authorisation r auth)

let auth_self_signed () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let id = "foo" in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  let pub, _priv = gen_pub () in
  let resources =
    let d = V.digest (Authorisation.wire auth)
    and d' = V.digest (Key.wire id pub)
    in
    [ Author.r Uint.zero pname `Authorisation d ; Author.r (Uint.of_int_exn 1) id `Key d' ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result a_ok a_err) "auth missing quorum"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.empty, 1)))
    (Conex_repository.validate_authorisation r auth) ;
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "auth ok"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.validate_authorisation r auth)

let a_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  let resources =
    let d = V.digest (Authorisation.wire auth) in
    [ Author.r Uint.zero pname `Releases d ]
  in
  let jid = "aaa" in
  let r = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "wrong resource"
    (Error (`InvalidResource (pname, `Authorisation, `Releases)))
    (Conex_repository.validate_authorisation r auth)

let a_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  let jid = "aaa" in
  let resources =
    let d = V.digest (Authorisation.wire auth) in
    [ Author.r Uint.zero "barf" `Authorisation d ]
  in
  let r = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "wrong name"
    (Error (`InvalidName (pname, "barf")))
    (Conex_repository.validate_authorisation r auth)

let auth_dyn () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors")
  in
  let resources =
    let d = V.digest (Authorisation.wire auth) in
    [ Author.r Uint.zero pname `Authorisation d ]
  in
  let r' = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "authorisation properly signed"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.validate_authorisation r' auth) ;
  let auth = Authorisation.add auth "foobar" in
  Alcotest.check (result a_ok a_err) "authorisation not properly signed"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.empty, 1)))
    (Conex_repository.validate_authorisation r' auth) ;
  let resources =
    let d = V.digest (Authorisation.wire auth) in
    [ Author.r Uint.zero pname `Authorisation d ]
  in
  let r' = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "authorisation properly signed, but missing id"
    (Error (`IdNotPresent (pname, S.singleton "foobar")))
    (Conex_repository.validate_authorisation r' auth) ;
  let pub, priv = gen_pub () in
  let resources =
    let d' = V.digest (Key.wire "foobar" pub) in
    [ Author.r Uint.zero "foobar" `Key d' ]
  in
  let idx = Author.t ~resources Uint.zero "foobar" in
  let r' = add_rs r' jid resources in
  let signed = sign_idx idx priv in
  let r' = match Conex_repository.validate_author r' signed with
    | Error _ -> Alcotest.fail "should not fail"
    | Ok r -> r
  in
  let auth = Authorisation.remove auth "foo" in
  Alcotest.check (result a_ok a_err) "authorisation properly signed (nothing changed)"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.validate_authorisation r' auth) ;
  let auth = Authorisation.add auth "foobar" in
  Alcotest.check (result a_ok a_err) "authorisation properly signed (nothing changed)"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.validate_authorisation r' auth) ;
  let auth = Authorisation.add auth "foobar42" in
  let resources =
    let d = V.digest (Authorisation.wire auth) in
    [ Author.r Uint.zero pname `Authorisation d ]
  in
  let r' = add_rs r' jid resources in
  Alcotest.check (result a_ok a_err) "authorisation properly signed (nothing changed)"
    (Error (`IdNotPresent ("foop", S.singleton "foobar42")))
    (Conex_repository.validate_authorisation r' auth) ;
  let r' = Conex_repository.add_id r' "foobar" in
  let auth = Authorisation.remove auth "foobar" in
  Alcotest.check (result a_ok a_err) "authorisation not properly signed (rm'ed, counter incr)"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.empty, 1)))
    (Conex_repository.validate_authorisation r' auth)


let auth_repo_tests = [
  "basic auth", `Quick, auth ;
  "also self signed", `Quick, auth_self_signed ;
  "wrong resource", `Quick, a_wrong_resource ;
  "wrong name", `Quick, a_wrong_name ;
  "dynamic authorisation", `Quick, auth_dyn ;
]

let r_ok =
  let module M = struct
    type t = [ `Approved of identifier | `Quorum of S.t | `Both of identifier * S.t ]
    let pp = Conex_repository.pp_ok
    let equal a b = match a, b with
      | `Approved a, `Approved b -> id_equal a b
      | `Quorum js, `Quorum is -> S.equal js is
      | `Both (a, js), `Both (b, is) -> id_equal a b && S.equal js is
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let r_err =
  let module M = struct
    type t = [ Conex_repository.base_error | `AuthRelMismatch of name * name | `InvalidReleases of name * S.t * S.t | `NoSharedPrefix of name * S.t ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && typ_equal w w' && typ_equal h h'
      | `NotApproved (n, r, js), `NotApproved (n', r', js') -> name_equal n n' && typ_equal r r' && S.equal js js'
      | `AuthRelMismatch (a, r), `AuthRelMismatch (a', r') -> name_equal a a' && name_equal r r'
      | `InvalidReleases (n, h, w), `InvalidReleases (n', h', w') -> name_equal n n' && S.equal h h' && S.equal w w'
      | `NoSharedPrefix (n, s), `NoSharedPrefix (n', s') -> name_equal n n' && S.equal s s'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let rel_1 () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t Uint.zero pname in
  Alcotest.check (result r_ok r_err) "not signed"
    (Error (`NotApproved (pname, `Releases, S.empty)))
    (Conex_repository.validate_releases r auth rel) ;
  let resources =
    let d = V.digest (Releases.wire rel) in
    [ Author.r Uint.zero pname `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "properly signed"
    (Ok (`Approved id))
    (Conex_repository.validate_releases r auth rel)

let rel_quorum () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t Uint.zero pname in
  let jid = "janitor" in
  let resources =
    let d = V.digest (Releases.wire rel) in
    [ Author.r Uint.zero pname `Releases d ]
  in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok r_err) "properly signed (quorum)"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.validate_releases r auth rel) ;
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "properly signed (both)"
    (Ok (`Both (id, S.singleton jid)))
    (Conex_repository.validate_releases r auth rel)

let rel_not_authorised () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton "foo") Uint.zero pname in
  let rel = Releases.t Uint.zero pname in
  let resources =
    let d = V.digest (Releases.wire rel) in
    [ Author.r Uint.zero pname `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "not authorised"
    (Error (`NotApproved (pname, `Releases, S.empty)))
    (Conex_repository.validate_releases r auth rel)

let rel_missing_releases () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let v = pname ^ ".0" in
  let rel = Releases.t ~versions:(S.singleton v) Uint.zero pname in
  let resources =
    let d = V.digest (Releases.wire rel) in
    [ Author.r Uint.zero pname `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "missing on disk"
    (Error (`InvalidReleases (pname, S.empty, S.singleton v)))
    (Conex_repository.validate_releases r ~on_disk:(Releases.t Uint.zero pname) auth rel) ;
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/checksum'"
    (Ok ()) (io.write ["packages"; pname; v; "checksum"] "") ;
  match Conex_io.compute_releases io Uint.zero pname with
  | Error _ -> Alcotest.fail "should be able to compute releases0"
  | Ok on_disk ->
    Alcotest.check (result r_ok r_err) "all good"
      (Ok (`Approved id))
      (Conex_repository.validate_releases r ~on_disk auth rel) ;
    let v2 = pname ^ ".1" in
    Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v2/checksum'"
      (Ok ()) (io.write ["packages"; pname; v2; "checksum"] "") ;
    match Conex_io.compute_releases io Uint.zero pname with
    | Error _ -> Alcotest.fail "shold be able to compute releases1"
    | Ok on_disk ->
      Alcotest.check (result r_ok r_err) "missing in releases"
        (Error (`InvalidReleases (pname, S.singleton v2, S.empty)))
        (Conex_repository.validate_releases r ~on_disk auth rel) ;
      let v3 = pname ^ ".2" in
      Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$oname/$v3/checksum'"
        (Ok ()) (io.write ["packages"; pname; v3; "checksum"] "") ;
      match Conex_io.compute_releases io Uint.zero pname with
      | Error _ -> Alcotest.fail "shoul be able to compute releases2"
      | Ok on_disk ->
        Alcotest.check (result r_ok r_err) "missing in releases"
          (Error (`InvalidReleases (pname, S.add v3 (S.singleton v2), S.empty)))
          (Conex_repository.validate_releases r ~on_disk auth rel)

let bad_releases () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let v = pname ^ pname ^ ".0" in
  let rel = Releases.t ~versions:(S.singleton v) Uint.zero pname in
  let resources =
    let d = V.digest (Releases.wire rel) in
    [ Author.r Uint.zero pname `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/checksum'"
    (Ok ()) (io.write ["packages"; pname; v; "checksum"] "") ;
  match Conex_io.compute_releases io Uint.zero pname with
  | Error _ -> Alcotest.fail "should be able to compute releases0"
  | Ok on_disk ->
    Alcotest.check (result r_ok r_err) "releases contains bad prefix"
      (Error (`NoSharedPrefix (pname, S.singleton v)))
      (Conex_repository.validate_releases r ~on_disk auth rel)

let rel_name_mismatch () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero "foo" in
  let rel = Releases.t Uint.zero pname in
  let resources =
    let d = V.digest (Releases.wire rel) in
    [ Author.r Uint.zero pname `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "releases and authorisation names do not match"
    (Error (`AuthRelMismatch ("foo", pname)))
    (Conex_repository.validate_releases r auth rel)

let rel_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t Uint.zero pname in
  let resources =
    let d = V.digest (Releases.wire rel) in
    [ Author.r Uint.zero "foo" `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "wrong name in releases"
    (Error (`InvalidName (pname, "foo")))
    (Conex_repository.validate_releases r auth rel)

let rel_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t Uint.zero pname in
  let resources =
    let d = V.digest (Releases.wire rel) in
    [ Author.r Uint.zero pname `Authorisation d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "wrong resource for releases"
    (Error (`InvalidResource (pname, `Releases, `Authorisation)))
    (Conex_repository.validate_releases r auth rel)

let rel_repo_tests = [
  "basic release", `Quick, rel_1 ;
  "quorum on releases", `Quick, rel_quorum ;
  "not authorised", `Quick, rel_not_authorised ;
  "missing some releases", `Quick, rel_missing_releases ;
  "bad releases on disk", `Quick, bad_releases ;
  "name mismatch (releases and authorisation)", `Quick, rel_name_mismatch ;
  "wrong name", `Quick, rel_wrong_name ;
  "wrong resource", `Quick, rel_wrong_resource ;
]


let c_err =
  let module M = struct
    type t = [ Conex_repository.base_error  | `AuthRelMismatch of name * name | `NotInReleases of name * S.t | `ChecksumsDiff of name * name list * name list * (Checksums.c * Checksums.c) list ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && typ_equal w w' && typ_equal h h'
      | `NotApproved (n, r, js), `NotApproved (n', r', js') -> name_equal n n' && typ_equal r r' && S.equal js js'
      | `AuthRelMismatch (a, r), `AuthRelMismatch (a', r') -> name_equal a a' && name_equal r r'
      | `NotInReleases (c, r), `NotInReleases (c', r') -> name_equal c c' && S.equal r r'
      | `ChecksumsDiff (n, a, b, cs), `ChecksumsDiff (n', a', b', cs') ->
        name_equal n n' &&
        S.equal (S.of_list a) (S.of_list a') &&
        S.equal (S.of_list b) (S.of_list b') &&
        List.length cs = List.length cs' &&
        List.for_all (fun (c, d) -> List.exists (fun (c', d') -> Checksums.checksum_equal c c' && Checksums.checksum_equal d d') cs) cs'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let cs_base () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t ~versions:(S.singleton v) Uint.zero pname in
  let cs = Checksums.t Uint.zero v [] in
  Alcotest.check (result r_ok c_err) "release not signed"
    (Error (`NotApproved (v, `Checksums, S.empty)))
    (Conex_repository.validate_checksums r auth rel cs) ;
  let resources =
    let d = V.digest (Releases.wire rel)
    and d' = V.digest (Checksums.wire cs)
    in
    [ Author.r Uint.zero pname `Releases d ; Author.r (Uint.of_int_exn 1) v `Checksums d' ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/release'"
    (Ok ()) (io.write ["packages"; pname; v; "checksums"] "") ;
  match Conex_io.compute_checksums V.raw_digest io Uint.zero v with
  | Error _ -> Alcotest.fail "should be able to compute release"
  | Ok on_disk ->
    Alcotest.check (result r_ok c_err) "good release"
      (Ok (`Approved id))
      (Conex_repository.validate_checksums r ~on_disk auth rel cs) ;
    let jid = "janitor" in
    let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
    let r = add_rs r jid resources in
    Alcotest.check (result r_ok c_err) "good release (both)"
      (Ok (`Both (id, S.singleton jid)))
      (Conex_repository.validate_checksums r auth rel cs)

let cs_quorum () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t ~versions:(S.singleton v) Uint.zero pname in
  let cs = Checksums.t Uint.zero v [] in
  let resources =
    let d = V.digest (Releases.wire rel)
    and d' = V.digest (Checksums.wire cs)
    in
    [ Author.r Uint.zero pname `Releases d ; Author.r (Uint.of_int_exn 1) v `Checksums d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/release'"
    (Ok ()) (io.write ["packages"; pname; v; "checksums"] "") ;
  match Conex_io.compute_checksums V.raw_digest io Uint.zero v with
  | Error _ -> Alcotest.fail "should be able to compute release"
  | Ok on_disk ->
    Alcotest.check (result r_ok c_err) "good release (quorum)"
      (Ok (`Quorum (S.singleton jid)))
      (Conex_repository.validate_checksums r ~on_disk auth rel cs)

let cs_bad () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foo" in
  let v = pname ^ ".0" in
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar' to 'packages/$pname/$v/foo'"
    (Ok ()) (io.write ["packages"; pname; v; "foo"] "bar") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foo' to 'packages/$pname/$v/bar'"
    (Ok ()) (io.write ["packages"; pname; v; "bar"] "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'p1' to 'packages/$pname/$v/files/patch1'"
    (Ok ()) (io.write ["packages"; pname; v; "files"; "patch1"] "p1") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'p2' to 'packages/$pname/$v/files/patch2'"
    (Ok ()) (io.write ["packages"; pname; v; "files"; "patch2"] "p2") ;
  (* manually crafted using echo -n XXX | openssl dgst -sha256 -binary | b64encode -m - *)
  let csums = [
    { Checksums.filename = "bar" ; digest = (`SHA256, "2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae") } ;
    { Checksums.filename = "foo" ; digest = (`SHA256, "fcde2b2edba56bf408601fb721fe9b5c338d10ee429ea04fae5511b68fbf8fb9") } ;
    { Checksums.filename = "files/patch1" ; digest = (`SHA256, "f64551fcd6f07823cb87971cfb91446425da18286b3ab1ef935e0cbd7a69f68a") } ;
    { Checksums.filename = "files/patch2" ; digest = (`SHA256, "3946ca64ff78d93ca61090a437cbb6b3d2ca0d488f5f9ccf3059608368b27693") }
  ]
  in
  let acss = Checksums.t Uint.zero v csums in
  Alcotest.check (result css ch_err) "release computation works"
    (Ok acss) (Conex_io.compute_checksums V.raw_digest io Uint.zero "foo.0") ;
  let css' = Checksums.t Uint.zero v (List.tl csums) in
  let css'' = Checksums.t Uint.zero v ({ Checksums.filename = "foobar" ; digest = (`SHA256, "") } :: csums) in
  let other = { Checksums.filename = "bar" ; digest = (`SHA256, "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=") } in
  let css''' = Checksums.t Uint.zero v (other :: List.tl csums) in
  let id = "id" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t ~versions:(S.singleton v) Uint.zero pname in
  let resources =
    let d = V.digest (Releases.wire rel)
    and d1 = V.digest (Checksums.wire acss)
    and d2 = V.digest (Checksums.wire css')
    and d3 = V.digest (Checksums.wire css'')
    and d4 = V.digest (Checksums.wire css''')
    in
    [ Author.r Uint.zero pname `Releases d ;
      Author.r (Uint.of_int_exn 1) v `Checksums d1 ;
      Author.r (Uint.of_int_exn 2) v `Checksums d2 ;
      Author.r (Uint.of_int_exn 3) v `Checksums d3 ;
      Author.r (Uint.of_int_exn 4) v `Checksums d4 ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  match Conex_io.compute_checksums V.raw_digest io Uint.zero v with
  | Error _ -> Alcotest.fail "should be able to compute release"
  | Ok on_disk ->
    Alcotest.check (result r_ok c_err) "good release (quorum)"
      (Ok (`Quorum (S.singleton jid)))
      (Conex_repository.validate_checksums r ~on_disk auth rel acss) ;
    Alcotest.check (result r_ok c_err) "bad release (missing in cs file)"
      (Error (`ChecksumsDiff (v, [], ["bar"], [])))
      (Conex_repository.validate_checksums r ~on_disk auth rel css') ;
    Alcotest.check (result r_ok c_err) "bad release (missing on disk)"
      (Error (`ChecksumsDiff (v, ["foobar"], [], [])))
      (Conex_repository.validate_checksums r ~on_disk auth rel css'') ;
    Alcotest.check (result r_ok c_err) "bad release (differ)"
      (Error (`ChecksumsDiff (v, [], [], [(List.hd csums, other)])))
      (Conex_repository.validate_checksums r ~on_disk auth rel css''')

let cs_bad_name () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let reln = "foo" in
  let v = reln ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t ~versions:(S.singleton v) Uint.zero reln in
  let cs = Checksums.t Uint.zero v [] in
  let resources =
    let d = V.digest (Releases.wire rel)
    and d' = V.digest (Checksums.wire cs)
    in
    [ Author.r Uint.zero reln `Releases d ; Author.r (Uint.of_int_exn 1) v `Checksums d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "bad name (auth != rel)"
    (Error (`AuthRelMismatch (pname, reln)))
    (Conex_repository.validate_checksums r auth rel cs)

let cs_bad_name2 () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let reln = pname ^ ".0" in
  let v = reln ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t ~versions:(S.singleton reln) Uint.zero pname in
  let cs = Checksums.t Uint.zero v [] in
  let resources =
    let d = V.digest (Releases.wire rel)
    and d' = V.digest (Checksums.wire cs)
    in
    [ Author.r Uint.zero pname `Releases d ; Author.r (Uint.of_int_exn 1) v `Checksums d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "bad name (not member of releases)"
    (Error (`NotInReleases (v, rel.Releases.versions)))
    (Conex_repository.validate_checksums r auth rel cs)

let cs_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t ~versions:(S.singleton v) Uint.zero pname in
  let cs = Checksums.t Uint.zero v [] in
  let resources =
    let d = V.digest (Releases.wire rel)
    and d' = V.digest (Checksums.wire cs)
    in
    [ Author.r Uint.zero pname `Releases d ; Author.r (Uint.of_int_exn 1) pname `Checksums d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "wrong name"
    (Error (`InvalidName (v, pname)))
    (Conex_repository.validate_checksums r auth rel cs)

let cs_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 V.digest () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Releases.t ~versions:(S.singleton v) Uint.zero pname in
  let cs = Checksums.t Uint.zero v [] in
  let resources =
    let d = V.digest (Releases.wire rel)
    and d' = V.digest (Checksums.wire cs)
    in
    [ Author.r Uint.zero pname `Releases d ; Author.r (Uint.of_int_exn 1) v `Releases d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "wrong resource"
    (Error (`InvalidResource (v, `Checksums, `Releases)))
    (Conex_repository.validate_checksums r auth rel cs)

let cs_repo_tests = [
  "basic checksum", `Quick, cs_base ;
  "quorum checksum", `Quick, cs_quorum ;
  "bad checksum", `Quick, cs_bad ;
  "bad releases name", `Quick, cs_bad_name ;
  "checksum name not in releases", `Quick, cs_bad_name2 ;
  "wrong checksum name", `Quick, cs_wrong_name ;
  "wrong checksum resource", `Quick, cs_wrong_resource ;
]

let tests prefix = [
  prefix ^ "RepositoryBasics", basic_repo_tests ;
  prefix ^ "RepositoryKeys", key_repo_tests ;
  prefix ^ "RepositoryTeam", team_repo_tests ;
  prefix ^ "RepositoryAuthorisation", auth_repo_tests ;
  prefix ^ "RepositoryPackage", rel_repo_tests ;
  prefix ^ "RepositoryRelease", cs_repo_tests ;
]
end

module O = RTest (Conex_openssl.O_V)
module N = RTest (Conex_nocrypto.NC_V)

let tests = ("MemoryProvider", mem_provider_tests) :: N.tests "Nocrypto"
