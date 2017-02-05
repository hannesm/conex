open Conex_result
open Conex_utils
open Conex_core
open Conex_resource

open Common

let res d =
  let data = Wire.to_string d in
  (Uint.of_int_exn (String.length data), Conex_nocrypto.digest data)

module Mem = struct
  open Conex_provider
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

  let mem_provider () : Conex_provider.t =
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
      | Node (_, ch) -> Ok (List.map (function Node (x, _) -> `Dir x | Leaf (x, _) -> `File x) ch)
      | Leaf _ -> Error "reading a directory, but found files"
    and exists path =
      match find_f !root path with Ok _ -> true | Error _ -> false
    in
    { name = "mem" ; description = "Memory provider" ; file_type ; read ; write ;read_dir ; exists }
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
    type t = Conex_provider.item
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
  let open Conex_provider in
  let p = Mem.mem_provider () in
  Alcotest.check Alcotest.bool "no 'foo' in empty mem store" false (p.exists ["foo"]) ;
  Alcotest.check (result Alcotest.string str_err) "reading 'foo' in empty mem store" (Error "") (p.read ["foo"]) ;
  Alcotest.check (result ft str_err) "file-type of 'foo' in empty mem store" (Error "") (p.file_type ["foo"]) ;
  Alcotest.check (result (Alcotest.list it) str_err) "read_dir of 'foo' in empty mem store" (Error "") (p.read_dir ["foo"])

let basic_p () =
  let open Conex_provider in
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
  let open Conex_provider in
  let p = Mem.mem_provider () in
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar' to 'packages/foo'"
    (Ok ()) (p.write ["packages"; "foo"] "bar") ;
  Alcotest.check Alcotest.bool "packages in more store" true (p.exists ["packages"]) ;
  Alcotest.check Alcotest.bool "packages/foo in more store" true (p.exists ["packages"; "foo"]) ;
  Alcotest.check Alcotest.bool "packages/foo/bar not in more store" false (p.exists ["packages"; "foo"; "bar"]) ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar2' to 'packages/foo2'"
    (Ok ()) (p.write ["packages"; "foo2"] "bar2") ;
  Alcotest.check (result (Alcotest.list it) str_err) "read_dir of 'packages' in more mem store"
    (Ok [ `File "foo2" ; `File "foo" ]) (p.read_dir ["packages"]) ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar3' to 'packages/foo3'"
    (Ok ()) (p.write ["packages"; "foo3"] "bar3") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar4' to 'packages/foo4'"
    (Ok ()) (p.write ["packages"; "foo4"] "bar4") ;
  Alcotest.check (result (Alcotest.list it) str_err) "read_dir of 'packages' in even more mem store"
    (Ok [ `File "foo4" ; `File "foo3" ; `File "foo2" ; `File "foo" ]) (p.read_dir ["packages"]) ;
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

(* basic operations work, and we have an in-memory data provider!  let the games begin *)
let empty_r () =
  let io = Mem.mem_provider () in
  Alcotest.check (result sset str_err) "empty repo errors on ids"
    (Error "") (Conex_io.ids io) ;
  Alcotest.check (result sset str_err) "empty repo errors on authorisations"
    (Error "") (Conex_io.items io) ;
  Alcotest.check (result sset str_err) "empty repo errors on subitems"
    (Error "") (Conex_io.subitems io "foo") ;
  Alcotest.check (result auth re) "reading authorisation foo in empty repo fails"
    (Error (`NotFound (`Authorisation, "foo"))) (Conex_io.read_authorisation io "foo") ;
  Alcotest.check (result package re) "reading releases foo in empty repo fails"
    (Error (`NotFound (`Package, "foo"))) (Conex_io.read_package io "foo") ;
  Alcotest.check (result ji re) "reading janitorindex foo in empty repo fails"
    (Error (`NotFound (`Author, "foo"))) (Conex_io.read_author io "foo") ;
  Alcotest.check (result rel re) "reading checksum foo.0 in empty repo fails"
    (Error (`NotFound (`Release, "foo.0"))) (Conex_io.read_release io "foo.0") ;
  Alcotest.check (result rel ch_err) "computing checksum foo.0 in empty repo fails"
    (Error (`FileNotFound "foo.0")) (Conex_io.compute_release io Uint.zero "foo.0")

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
    (io_ex (Conex_io.ids io))


let team_r () =
  let r = Conex_repository.repository ~quorum:10 () in
  Alcotest.(check int "quorum is correct" 10 (Conex_repository.quorum r)) ;
  Alcotest.(check (option sset) "team foo is empty" None (Conex_repository.find_team r "foo")) ;
  let t = Team.t ~members:(S.singleton "bar") Uint.zero "foo" in
  let r = Conex_repository.add_team r t in
  Alcotest.(check (option sset) "team foo has one member"
              (Some (S.singleton "bar")) (Conex_repository.find_team r "foo"))

let checks_r () =
  let open Conex_provider in
  let io = Mem.mem_provider () in
  Alcotest.check (result Alcotest.unit str_err) "writing 'bar' to 'packages/foo/foo.0/foo'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "foo"] "bar") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foo' to 'packages/foo/foo.0/bar'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "bar"] "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'p1' to 'packages/foo/foo.0/files/patch1'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "files"; "patch1"] "p1") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'p2' to 'packages/foo/foo.0/files/patch2'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "files"; "patch2"] "p2") ;
  (* manually crafted using echo -n XXX | openssl dgst -sha256 -binary | b64encode -m - *)
  let csums = [
    { Release.filename = "bar" ; size = Uint.of_int_exn 3 ; digest = (`SHA256, "LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm564=") } ;
    { Release.filename = "foo" ; size = Uint.of_int_exn 3 ; digest = (`SHA256, "/N4rLtula/QIYB+3If6bXDONEO5CnqBPrlURto+/j7k=") } ;
    { Release.filename = "files/patch1" ; size = Uint.of_int_exn 2 ; digest = (`SHA256, "9kVR/NbweCPLh5cc+5FEZCXaGChrOrHvk14MvXpp9oo=") } ;
    { Release.filename = "files/patch2" ; size = Uint.of_int_exn 2 ; digest = (`SHA256, "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=") }
  ]
  in
  let css = Release.t Uint.zero "foo.0" csums in
  Alcotest.check (result rel ch_err) "checksum computation works"
    (Ok css) (Conex_io.compute_release io Uint.zero "foo.0")

let basic_persistency () =
  let open Wire in
  Alcotest.check (result team str_err) "couldn't parse team"
    (Error "") (Team.of_wire M.empty) ;
  let bad_c = M.add "counter" (String "foo") M.empty in
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
    M.add "name" (String "foo") bad_n
  in
  Alcotest.check (result team str_err) "couldn't parse team (type, wraps missing)"
    (Error "") (Team.of_wire bad_n) ;
  let bad_n =
    M.add "typ" (Int Uint.zero) bad_n
  in
  Alcotest.check (result team str_err) "couldn't parse team (bad type, wraps missing)"
    (Error "") (Team.of_wire bad_n) ;
  let bad_n =
    M.add "typ" (String "team") bad_n
  in
  Alcotest.check (result team str_err) "couldn't parse team (wraps missing)"
    (Error "") (Team.of_wire bad_n) ;
  let good_n =
    M.add "wraps" (Int Uint.zero) (M.add "created" (Int Uint.zero) bad_n)
  in
  let t = Team.t Uint.zero "foo" in
  Alcotest.check (result team str_err) "could parse team"
    (Ok t) (Team.of_wire good_n) ;
  let bad_m =
    M.add "members" (String "bl") good_n
  in
  Alcotest.check (result team str_err) "couldn't parse team members"
    (Error "") (Team.of_wire bad_m) ;
  let bad_m =
    M.add "members" (List [ Int Uint.zero ])
      (M.add "name" (String "foo") good_n)
  in
  Alcotest.check (result team str_err) "couldn't parse team members (not a set)"
    (Error "") (Team.of_wire bad_m) ;
  let good_t =
    M.add "members" (List [ String "foo" ]) good_n
  in
  let t = Team.t ~members:(S.singleton "foo") Uint.zero "foo" in
  Alcotest.check (result team str_err) "could parse team"
    (Ok t) (Team.of_wire good_t) ;
  Alcotest.check (result team str_err) "could unparse/parse team"
    (Ok t) (Team.of_wire (Team.wire t)) ;
  Alcotest.check (result team str_err) "could unparse/parse team"
    (Ok t) (Conex_opam_encoding.decode (Conex_opam_encoding.encode (Team.wire t)) >>= Team.of_wire) ;
  let checksum =
    M.add "filename" (String "foo")
      (M.add "size" (Int (Uint.of_int_exn 0xA9))
         (M.add "digest" (List [ String "SHA256" ; String "dsAK05qC6v7WGDQ3z3D6bGqmNPFvbYR4hMkli9WS0IY=" ])
            M.empty))
  in
  let css =
    M.add "name" (String "foo")
      (M.add "counter" (Int Uint.zero)
         (M.add "wraps" (Int Uint.zero)
            (M.add "created" (Int Uint.zero)
               (M.add "version" (Int Uint.zero)
                  (M.add "typ" (String "release")
                     (M.add "files" (List [Map checksum]) M.empty))))))
  in
  let csum =
    let size, digest = res Author.(wire (t Uint.zero "bar")) in
    { Release.filename = "foo" ; size ; digest }
  in
  let csums = Release.t Uint.zero "foo" [csum] in
  Alcotest.check (result rel str_err) "can parse checksum"
    (Ok csums) (Release.of_wire css) ;
  let s = List [ Int Uint.zero ; String (Signature.alg_to_string `RSA_PSS_SHA256) ; String "barf" ] in
  let s' = ((`RSA_PSS_SHA256, Uint.zero), "frab") in
  let idx =
    M.add "name" (String "foo")
      (M.add "counter" (Int Uint.zero)
         (M.add "created" (Int Uint.zero)
            (M.add "wraps" (Int Uint.zero)
               (M.add "typ" (String "author")
                  (M.add "version" (Int Uint.zero) M.empty)))))
  in
  let empty_idx =
    M.add "queued" (List [])
      (M.add "signatures" (List [s]) M.empty)
  in
  let idxs =
    M.add "signed" (Map idx) empty_idx
  in
  let idx' = Author.t ~signatures:[s'] Uint.zero "foo" in
  Alcotest.check (result ji str_err) "can parse index"
    (Ok idx') (Author.of_wire idxs) ;
  Alcotest.check (result ji str_err) "can unparse/parse index"
    (Ok idx') (Author.of_wire (Author.wire idx')) ;
  let r =
    M.add "index" (Int Uint.zero)
      (M.add "name" (String "foobar")
         (M.add "size" (Int Uint.zero)
            (M.add "typ" (String "team")
               (M.add "digest" (List [ String "SHA256" ; String "01234567890123456789012345678901234567890123" ]) M.empty))))
  in
  let r' = Author.r Uint.zero "foobar" Uint.zero `Team (`SHA256, "01234567890123456789012345678901234567890123") in
  let idx = M.add "resources" (List [ Map r ]) idx in
  let idxs =
    M.add "signed" (Map idx) empty_idx
  in
  let idx' = Author.t ~signatures:[s'] ~resources:[r'] Uint.zero "foo" in
  Alcotest.check (result ji str_err) "can parse index"
    (Ok idx') (Author.of_wire idxs) ;
  Alcotest.check (result ji str_err) "can unparse/parse index"
    (Ok idx') (Author.of_wire (Author.wire idx')) ;
  let bad_r = M.add "typ" (String "teamfoo") r in
  let idx = M.add "resources" (List [ Map bad_r ]) idx in
  let idxs =
    M.add "signed" (Map idx) empty_idx
  in
  Alcotest.check (result ji str_err) "cannot parse index, bad resource type"
    (Error "") (Author.of_wire idxs)

let bad_id_r () =
  let open Conex_provider in
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
  let open Conex_provider in
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
  let open Conex_provider in
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
  let open Conex_provider in
  let io = Mem.mem_provider () in
  Alcotest.check (result package re) "package foo not found"
    (Error (`NotFound (`Package, "foo"))) (Conex_io.read_package io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foobar' to 'packages/foo/releases'"
    (Ok ()) (io.write ["packages"; "foo"; "releases"] "foobar") ;
  Alcotest.check (result package re) "parse error on package foo"
    (Error (`ParseError (`Package, "foo", ""))) (Conex_io.read_package io "foo") ;
  let rel = Package.t Uint.zero "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing package 'foo'"
    (Ok ()) (Conex_io.write_package io rel) ;
  Alcotest.check (result package re) "package foo good"
    (Ok rel) (Conex_io.read_package io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'packages/foobar/releases'"
    (Ok ()) (io.write ["packages"; "foobar"; "releases"] (Conex_opam_encoding.encode (Package.wire rel))) ;
  Alcotest.check (result package re) "name mismatch on releases foobar"
    (Error (`NameMismatch (`Package, "foobar", "foo"))) (Conex_io.read_package io "foobar")

let bad_cs_r () =
  let open Conex_provider in
  let io = Mem.mem_provider () in
  Alcotest.check (result rel re) "release foo not found"
    (Error (`NotFound (`Release, "foo"))) (Conex_io.read_release io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foobar' to 'packages/foo/foo.0/checksum'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "checksum"] "foobar") ;
  Alcotest.check (result rel re) "parse error on release foo.0"
    (Error (`ParseError (`Release, "foo.0", ""))) (Conex_io.read_release io "foo.0") ;
  let c = Release.t Uint.zero "foo.0" [] in
  Alcotest.check (result Alcotest.unit str_err) "writing release foo.0"
    (Ok ()) (Conex_io.write_release io c) ;
  Alcotest.check (result rel re) "release foo.0 good"
    (Ok c) (Conex_io.read_release io "foo.0") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'packages/foo/foo.1/checksum'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.1"; "checksum"] (Conex_opam_encoding.encode (Release.wire c))) ;
  Alcotest.check (result rel re) "name mismatch on checksum foo.1"
    (Error (`NameMismatch (`Release, "foo.1", "foo.0"))) (Conex_io.read_release io "foo.1") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'blubb' to 'packages/foo/foo.2'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.2"] "blubb") ;
  Alcotest.check (result rel ch_err) "checksum is a file, should be a directory"
    (Error (`NotADirectory "foo.2")) (Conex_io.compute_release io Uint.zero "foo.2")

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

let r_ok =
  let module M = struct
    type t = (Conex_repository.t * string list * identifier)
    let pp ppf (_, ws, id) = Format.fprintf ppf "%s (%d warnings)" id (List.length ws)
    let equal (_, w, id) (_, w', id') = id_equal id id' && List.length w = List.length w'
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let idx_sign () =
  let r = Conex_repository.repository ~quorum:0 () in
  let id = "foo" in
  let pub, priv = gen_pub () in
  let idx = Author.t ~keys:[pub] Uint.zero id in
  Alcotest.check (result r_ok verr) "empty index signed properly (no resources, quorum 0)"
    (Ok (r, [], id)) (Conex_repository.verify_author r idx) ;
  let pubenc = Wire.to_string (Key.wire id pub) in
  let idx = Author.(add_resource idx (r (next_id idx) id (Uint.of_int_exn (String.length pubenc)) `Key (Conex_nocrypto.digest pubenc))) in
  let signed_idx = sign_idx idx priv in
  Alcotest.check (result r_ok verr) "signed_idx signed properly (1 resource, quorum 0)"
    (Ok (r, [], id)) (Conex_repository.verify_author r signed_idx) ;
  Alcotest.check (result r_ok verr) "idx signed properly (0 resources, 1 queued, quorum 0)"
    (Ok (r, [], id)) (Conex_repository.verify_author r idx) ;
  let idx, _ = Author.prep_sig idx in
  Alcotest.check (result r_ok verr) "idx not signed properly (1 resource, quorum 0)"
    (Error `NoSignature) (Conex_repository.verify_author r idx)

let r_fake =
  let module M = struct
    type t = Conex_repository.t
    let pp ppf _ = Format.fprintf ppf "repository"
    let equal _ _ = true
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let idx_sign_verify () =
  let r = Conex_repository.repository ~quorum:0 () in
  let id = "foo" in
  let pub, priv = gen_pub () in
  let idx = Author.t ~keys:[pub] Uint.zero id in
  let s, d = res (Key.wire id pub) in
  let resources = [ Author.r Uint.zero id s `Key d ] in
  let signed_idx = sign_idx idx priv in
  Alcotest.check (result r_ok verr) "idx signed properly"
    (Ok (r, [], id)) (Conex_repository.verify_author r signed_idx) ;
  let signatures = signed_idx.Author.signatures in
  let idx' = Author.t ~keys:[pub] ~resources ~signatures Uint.zero id in
  Alcotest.check (result r_ok verr) "idx' not properly signed"
    (Error `NoSignature) (Conex_repository.verify_author r idx') ;
  let idx'' = Author.t ~keys:[pub] ~queued:resources ~signatures ~counter:Uint.(of_int_exn 1) Uint.zero id in
  Alcotest.check (result r_ok verr) "idx'' properly signed (queue)"
    (Ok (r, [], id)) (Conex_repository.verify_author r idx'') ;
  let signed_idx' = sign_idx idx' priv in
  Alcotest.check (result r_ok verr) "signed_idx' signed properly"
    (Ok (r, [], id)) (Conex_repository.verify_author r signed_idx') ;
  match Conex_repository.add_valid_resource r id (List.hd resources) with
  | Ok r ->
    Alcotest.check (result r_fake str_err) "add errors on wrong name"
      (Error "") (Conex_repository.add_valid_resource r id
                    (Author.r Uint.zero "barf" s `Key d)) ;
    Alcotest.check (result r_fake str_err) "add errors on wrong resource"
      (Error "") (Conex_repository.add_valid_resource r id
                    (Author.r Uint.zero id s `Team d)) ;
    Alcotest.check (result r_fake str_err) "add errors on wrong size"
      (Error "") (Conex_repository.add_valid_resource r id
                    (Author.r Uint.zero id (Uint.of_int_exn 23) `Key d))
  | Error e -> Alcotest.fail e

let idx_s_v_dupl () =
  let r = Conex_repository.repository ~quorum:0 () in
  let id = "foo" in
  let pub, priv = gen_pub () in
  let s, d = res (Key.wire id pub) in
  let resources = [
    Author.r Uint.zero id s `Key d ;
    Author.r (Uint.of_int_exn 1) id s `Team d ;
  ] in
  let idx = Author.t ~keys:[pub] ~resources Uint.zero id in
  let signed_idx = sign_idx idx priv in
  Alcotest.check (result r_ok verr) "idx signed properly, but one resource ignored"
    (Ok (r, [""], id)) (Conex_repository.verify_author r signed_idx) ;
  let signatures =
    ((`RSA_PSS_SHA256, Uint.zero), "foobar") ::
    signed_idx.Author.signatures @
    [ ((`RSA_PSS_SHA256, Uint.zero), "foobar") ]
  in
  let signed_idx = Author.t ~keys:[pub] ~counter:(Uint.of_int_exn 1) ~resources ~signatures Uint.zero id in
  Alcotest.check (result r_ok verr) "idx signed properly (second sig), but one resource ignored"
    (Ok (r, [""], id)) (Conex_repository.verify_author r signed_idx)

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
    type t = [ Conex_repository.base_error | `InsufficientQuorum of name * typ * S.t | `MissingSignature of identifier ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && typ_equal w w' && typ_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && typ_equal r r' && S.equal js js'
      | `InsufficientQuorum (id, r, q), `InsufficientQuorum (id', r', q') -> id_equal id id' && S.equal q q' && typ_equal r r'
      | `MissingSignature id, `MissingSignature id' -> id_equal id id'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let add_rs repo id rs =
  List.fold_left (fun repo res ->
      match Conex_repository.add_valid_resource repo id res with
      | Ok r -> r
      | Error e -> Alcotest.fail e)
    repo rs

let key_good () =
  let r = Conex_repository.repository ~quorum:1 () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _ = gen_pub () in
  let resources =
    let s, d = res (Key.wire id pub) in
    [ Author.r Uint.zero id s `Key d ]
  in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  Alcotest.check (result k_ok k_err) "not signed"
    (Error (`NotSigned (id, `Key, S.empty)))
    (Conex_repository.verify_key r id pub) ;
  let r' = add_rs r jid resources in
  Alcotest.check (result k_ok k_err) "publickey missing self-sig"
    (Error (`MissingSignature id))
    (Conex_repository.verify_key r' id pub) ;
  let r'' = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "publickey missing quorum"
    (Error (`InsufficientQuorum (id, `Key, S.empty)))
    (Conex_repository.verify_key r'' id pub) ;
  let r''' = add_rs r' id resources in
  Alcotest.check (result k_ok k_err) "publickey is fine"
    (Ok (`Both (id, S.singleton jid)))
    (Conex_repository.verify_key r''' id pub)

let key_good_quorum () =
  let r = Conex_repository.repository ~quorum:3 () in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let resources =
    let s, d = res (Key.wire id pub) in
    [ Author.r Uint.zero id s `Key d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 0"
    (Error (`InsufficientQuorum (id, `Key, S.empty)))
    (Conex_repository.verify_key r id pub) ;
  let jidx r jid =
    let r =
      let mems = match Conex_repository.find_team r "janitors" with None -> S.empty | Some s -> s in
      Conex_repository.add_team r (Team.t ~members:(S.add jid mems) Uint.zero "janitors")
    in
    add_rs r jid resources
  in
  let r = jidx r "jana" in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 1"
    (Error (`InsufficientQuorum (id, `Key, S.singleton "jana")))
    (Conex_repository.verify_key r id pub) ;
  let r = jidx r "janb" in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 2"
    (Error (`InsufficientQuorum (id, `Key, S.add "janb" (S.singleton "jana"))))
    (Conex_repository.verify_key r id pub) ;
  let r = jidx r "janc" in
  Alcotest.check (result k_ok k_err) "publickey is fine"
    (Ok (`Both (id, S.add "janc" (S.add "janb" (S.singleton "jana")))))
    (Conex_repository.verify_key r id pub)

let no_janitor () =
  let r = Conex_repository.repository ~quorum:1 () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let id' = "bar" in
  let resources =
    let s, d = res (Key.wire id pub)
    and s', d' = res (Key.wire id' pub)
    in
    [ Author.r Uint.zero id s `Key d ; Author.r (Uint.of_int_exn 1) id' s' `Key d' ]
  in
  let r = add_rs r jid resources in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "missing quorum"
    (Error (`InsufficientQuorum (id, `Key, S.empty)))
    (Conex_repository.verify_key r id pub) ;
  Alcotest.check (result k_ok k_err) "missing quorum for empty"
    (Error (`NotSigned (id', `Key, S.empty)))
    (Conex_repository.verify_key r id' pub)

let k_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let resources =
    let s, d = res (Key.wire id pub) in
    [ Author.r Uint.zero id s `Package d ]
  in
  let r = add_rs r jid resources in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "wrong resource"
    (Error (`InvalidResource (id, `Key, `Package)))
    (Conex_repository.verify_key r id pub)

let k_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let resources =
    let s, d = res (Key.wire id pub) in
    [ Author.r Uint.zero jid s `Key d ]
  in
  let r = add_rs r jid resources in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "wrong name"
    (Error (`InvalidName (id, jid)))
    (Conex_repository.verify_key r id pub)

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
             | `InsufficientQuorum of name * typ * S.t
             | `IdNotPresent of name * S.t
             | `MemberNotPresent of identifier * S.t ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InsufficientQuorum (id, r, q), `InsufficientQuorum (id', r', q') -> id_equal id id' && S.equal q q' && typ_equal r r'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && typ_equal w w' && typ_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && typ_equal r r' && S.equal js js'
      | `IdNotPresent (n, s), `IdNotPresent (n', s') -> name_equal n n' && S.equal s s'
      | `MemberNotPresent (n, s), `MemberNotPresent (n', s') -> id_equal n n' && S.equal s s'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let team () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  Alcotest.check (result t_ok a_err) "team missing quorum"
    (Error (`InsufficientQuorum (pname, `Team, S.empty)))
    (Conex_repository.verify_team r team) ;
  let resources =
    let s, d = res (Team.wire team) in
    [ Author.r Uint.zero pname s `Team d ]
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
    (Conex_repository.verify_team r team) ;
  let r = Conex_repository.repository ~quorum:2 () in
  let r = j_sign r "janitor" in
  Alcotest.check (result t_ok a_err) "team missing quorum of 2"
    (Error (`InsufficientQuorum (pname, `Team, S.singleton "janitor")))
    (Conex_repository.verify_team r team) ;
  let r = j_sign r "janitor2" in
  Alcotest.check (result t_ok a_err) "team quorum of 2 good"
    (Ok (r, `Quorum (S.add "janitor2" (S.singleton "janitor"))))
    (Conex_repository.verify_team r team)

let team_self_signed () =
  let r = Conex_repository.repository ~quorum:1 () in
  let id = "foo" in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  let pub, _priv = gen_pub () in
  let resources =
    let s, d = res (Team.wire team)
    and s', d' = res (Key.wire id pub)
    in
    [ Author.r Uint.zero pname s `Team d ; Author.r (Uint.of_int_exn 1) id s' `Key d' ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result t_ok a_err) "team missing quorum"
    (Error (`InsufficientQuorum (pname, `Team, S.empty)))
    (Conex_repository.verify_team r team) ;
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "team ok"
    (Ok (r, `Quorum (S.singleton jid)))
    (Conex_repository.verify_team r team)

let team_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  let resources =
    let s, d = res (Team.wire team) in
    [ Author.r Uint.zero pname s `Package d ]
  in
  let jid = "aaa" in
  let r = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "wrong resource"
    (Error (`InvalidResource (pname, `Team, `Package)))
    (Conex_repository.verify_team r team)

let team_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  let jid = "aaa" in
  let resources =
    let s, d = res (Team.wire team) in
    [ Author.r Uint.zero "barf" s `Team d ]
  in
  let r = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "wrong name"
    (Error (`InvalidName (pname, "barf")))
    (Conex_repository.verify_team r team)

let team_dyn () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let team = Team.t Uint.zero pname in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors")
  in
  let resources =
    let s, d = res (Team.wire team) in
    [ Author.r Uint.zero pname s `Team d ]
  in
  let r' = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "team properly signed"
    (Ok (r', `Quorum (S.singleton jid)))
    (Conex_repository.verify_team r' team) ;
  let team = Team.add team "foobar" in
  Alcotest.check (result t_ok a_err) "team not properly signed"
    (Error (`InsufficientQuorum (pname, `Team, S.empty)))
    (Conex_repository.verify_team r' team) ;
  let resources =
    let s, d = res (Team.wire team) in
    [ Author.r Uint.zero pname s `Team d ]
  in
  let r' = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "team properly signed, but missing member"
    (Error (`MemberNotPresent (pname, S.singleton "foobar")))
    (Conex_repository.verify_team r' team) ;
  let pub, priv = gen_pub () in
  let resources =
    let s', d' = res (Key.wire "foobar" pub) in
    [ Author.r Uint.zero "foobar" s' `Key d' ]
  in
  let idx = Author.t ~keys:[pub] ~resources Uint.zero "foobar" in
  let r' = add_rs r' jid resources in
  match Conex_sign.sign Uint.zero idx priv with
  | Error e -> Alcotest.fail e
  | Ok signed ->
    let r' = match Conex_repository.verify_author r' signed with
      | Error _ -> Alcotest.fail "should not fail"
      | Ok (r, _, _) -> r
    in
    Alcotest.check (result t_ok a_err) "team properly signed"
      (Ok (r', `Quorum (S.singleton jid)))
      (Conex_repository.verify_team r' team) ;
    let team = Team.remove team "foo" in
    Alcotest.check (result t_ok a_err) "team properly signed (nothing changed)"
      (Ok (r', `Quorum (S.singleton jid)))
      (Conex_repository.verify_team r' team) ;
    let team = Team.add team "foobar" in
    Alcotest.check (result t_ok a_err) "team properly signed (nothing changed)"
      (Ok (r', `Quorum (S.singleton jid)))
      (Conex_repository.verify_team r' team) ;
    let team = Team.remove team "foobar" in
    Alcotest.check (result t_ok a_err) "team not properly signed (rm'ed, counter incr)"
      (Error (`InsufficientQuorum (pname, `Team, S.empty)))
      (Conex_repository.verify_team r' team)

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
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  Alcotest.check (result a_ok a_err) "auth missing quorum"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.empty)))
    (Conex_repository.verify_authorisation r auth) ;
  let resources =
    let s, d = res (Authorisation.wire auth) in
    [ Author.r Uint.zero pname s `Authorisation d ]
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
    (Conex_repository.verify_authorisation r auth) ;
  let r = Conex_repository.repository ~quorum:2 () in
  let r = j_sign r "janitor" in
  Alcotest.check (result a_ok a_err) "auth missing quorum of 2"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.singleton "janitor")))
    (Conex_repository.verify_authorisation r auth) ;
  let r = j_sign r "janitor2" in
  Alcotest.check (result a_ok a_err) "auth quorum of 2 good"
    (Ok (`Quorum (S.add "janitor2" (S.singleton "janitor"))))
    (Conex_repository.verify_authorisation r auth)

let auth_self_signed () =
  let r = Conex_repository.repository ~quorum:1 () in
  let id = "foo" in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  let pub, _priv = gen_pub () in
  let resources =
    let s, d = res (Authorisation.wire auth)
    and s', d' = res (Key.wire id pub)
    in
    [ Author.r Uint.zero pname s `Authorisation d ; Author.r (Uint.of_int_exn 1) id s' `Key d' ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result a_ok a_err) "auth missing quorum"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.empty)))
    (Conex_repository.verify_authorisation r auth) ;
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "auth ok"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.verify_authorisation r auth)

let a_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  let resources =
    let s, d = res (Authorisation.wire auth) in
    [ Author.r Uint.zero pname s `Package d ]
  in
  let jid = "aaa" in
  let r = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "wrong resource"
    (Error (`InvalidResource (pname, `Authorisation, `Package)))
    (Conex_repository.verify_authorisation r auth)

let a_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  let jid = "aaa" in
  let resources =
    let s, d = res (Authorisation.wire auth) in
    [ Author.r Uint.zero "barf" s `Authorisation d ]
  in
  let r = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "wrong name"
    (Error (`InvalidName (pname, "barf")))
    (Conex_repository.verify_authorisation r auth)

let auth_dyn () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let auth = Authorisation.t Uint.zero pname in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors")
  in
  let resources =
    let s, d = res (Authorisation.wire auth) in
    [ Author.r Uint.zero pname s `Authorisation d ]
  in
  let r' = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "authorisation properly signed"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.verify_authorisation r' auth) ;
  let auth = Authorisation.add auth "foobar" in
  Alcotest.check (result a_ok a_err) "authorisation not properly signed"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.empty)))
    (Conex_repository.verify_authorisation r' auth) ;
  let resources =
    let s, d = res (Authorisation.wire auth) in
    [ Author.r Uint.zero pname s `Authorisation d ]
  in
  let r' = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "authorisation properly signed, but missing id"
    (Error (`IdNotPresent (pname, S.singleton "foobar")))
    (Conex_repository.verify_authorisation r' auth) ;
  let pub, priv = gen_pub () in
  let resources =
    let s', d' = res (Key.wire "foobar" pub) in
    [ Author.r Uint.zero "foobar" s' `Key d' ]
  in
  let idx = Author.t ~keys:[pub] ~resources Uint.zero "foobar" in
  let r' = add_rs r' jid resources in
  match Conex_sign.sign Uint.zero idx priv with
  | Error e -> Alcotest.fail e
  | Ok signed ->
    let r' = match Conex_repository.verify_author r' signed with
      | Error _ -> Alcotest.fail "should not fail"
      | Ok (r, _, _) -> r
    in
    let auth = Authorisation.remove auth "foo" in
    Alcotest.check (result a_ok a_err) "authorisation properly signed (nothing changed)"
      (Ok (`Quorum (S.singleton jid)))
      (Conex_repository.verify_authorisation r' auth) ;
    let auth = Authorisation.add auth "foobar" in
    Alcotest.check (result a_ok a_err) "authorisation properly signed (nothing changed)"
      (Ok (`Quorum (S.singleton jid)))
      (Conex_repository.verify_authorisation r' auth) ;
    let auth = Authorisation.remove auth "foobar" in
    Alcotest.check (result a_ok a_err) "authorisation not properly signed (rm'ed, counter incr)"
      (Error (`InsufficientQuorum (pname, `Authorisation, S.empty)))
      (Conex_repository.verify_authorisation r' auth)


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
    let pp = Conex_repository.pp_ok
    let equal a b = match a, b with
      | `Signed a, `Signed b -> id_equal a b
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
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && typ_equal r r' && S.equal js js'
      | `AuthRelMismatch (a, r), `AuthRelMismatch (a', r') -> name_equal a a' && name_equal r r'
      | `InvalidReleases (n, h, w), `InvalidReleases (n', h', w') -> name_equal n n' && S.equal h h' && S.equal w w'
      | `NoSharedPrefix (n, s), `NoSharedPrefix (n', s') -> name_equal n n' && S.equal s s'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let rel_1 () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t Uint.zero pname in
  Alcotest.check (result r_ok r_err) "not signed"
    (Error (`NotSigned (pname, `Package, S.empty)))
    (Conex_repository.verify_package r auth rel) ;
  let resources =
    let s, d = res (Package.wire rel) in
    [ Author.r Uint.zero pname s `Package d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "properly signed"
    (Ok (`Signed id))
    (Conex_repository.verify_package r auth rel)

let rel_quorum () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t Uint.zero pname in
  let jid = "janitor" in
  let resources =
    let s, d = res (Package.wire rel) in
    [ Author.r Uint.zero pname s `Package d ]
  in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok r_err) "properly signed (quorum)"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.verify_package r auth rel) ;
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "properly signed (both)"
    (Ok (`Both (id, S.singleton jid)))
    (Conex_repository.verify_package r auth rel)

let rel_not_authorised () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton "foo") Uint.zero pname in
  let rel = Package.t Uint.zero pname in
  let resources =
    let s, d = res (Package.wire rel) in
    [ Author.r Uint.zero pname s `Package d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "not authorised"
    (Error (`NotSigned (pname, `Package, S.empty)))
    (Conex_repository.verify_package r auth rel)

let rel_missing_releases () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let v = pname ^ ".0" in
  let rel = Package.t ~releases:(S.singleton v) Uint.zero pname in
  let resources =
    let s, d = res (Package.wire rel) in
    [ Author.r Uint.zero pname s `Package d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "missing on disk"
    (Error (`InvalidReleases (pname, S.empty, S.singleton v)))
    (Conex_repository.verify_package r ~on_disk:(Package.t Uint.zero pname) auth rel) ;
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/checksum'"
    (Ok ()) (io.Conex_provider.write ["packages"; pname; v; "checksum"] "") ;
  match Conex_io.compute_package io Uint.zero pname with
  | Error _ -> Alcotest.fail "should be able to compute releases0"
  | Ok on_disk ->
    Alcotest.check (result r_ok r_err) "all good"
      (Ok (`Signed id))
      (Conex_repository.verify_package r ~on_disk auth rel) ;
    let v2 = pname ^ ".1" in
    Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v2/checksum'"
      (Ok ()) (io.Conex_provider.write ["packages"; pname; v2; "checksum"] "") ;
    match Conex_io.compute_package io Uint.zero pname with
    | Error _ -> Alcotest.fail "shold be able to compute releases1"
    | Ok on_disk ->
      Alcotest.check (result r_ok r_err) "missing in releases"
        (Error (`InvalidReleases (pname, S.singleton v2, S.empty)))
        (Conex_repository.verify_package r ~on_disk auth rel) ;
      let v3 = pname ^ ".2" in
      Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$oname/$v3/checksum'"
        (Ok ()) (io.Conex_provider.write ["packages"; pname; v3; "checksum"] "") ;
      match Conex_io.compute_package io Uint.zero pname with
      | Error _ -> Alcotest.fail "shoul be able to compute releases2"
      | Ok on_disk ->
        Alcotest.check (result r_ok r_err) "missing in releases"
          (Error (`InvalidReleases (pname, S.add v3 (S.singleton v2), S.empty)))
          (Conex_repository.verify_package r ~on_disk auth rel)

let bad_releases () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let v = pname ^ pname ^ ".0" in
  let rel = Package.t ~releases:(S.singleton v) Uint.zero pname in
  let resources =
    let s, d = res (Package.wire rel) in
    [ Author.r Uint.zero pname s `Package d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/checksum'"
    (Ok ()) (io.Conex_provider.write ["packages"; pname; v; "checksum"] "") ;
  match Conex_io.compute_package io Uint.zero pname with
  | Error _ -> Alcotest.fail "should be able to compute releases0"
  | Ok on_disk ->
    Alcotest.check (result r_ok r_err) "releases contains bad prefix"
      (Error (`NoSharedPrefix (pname, S.singleton v)))
      (Conex_repository.verify_package r ~on_disk auth rel)

let rel_name_mismatch () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero "foo" in
  let rel = Package.t Uint.zero pname in
  let resources =
    let s, d = res (Package.wire rel) in
    [ Author.r Uint.zero pname s `Package d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "releases and authorisation names do not match"
    (Error (`AuthRelMismatch ("foo", pname)))
    (Conex_repository.verify_package r auth rel)

let rel_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t Uint.zero pname in
  let resources =
    let s, d = res (Package.wire rel) in
    [ Author.r Uint.zero "foo" s `Package d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "wrong name in releases"
    (Error (`InvalidName (pname, "foo")))
    (Conex_repository.verify_package r auth rel)

let rel_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t Uint.zero pname in
  let resources =
    let s, d = res (Package.wire rel) in
    [ Author.r Uint.zero pname s `Authorisation d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "wrong resource for releases"
    (Error (`InvalidResource (pname, `Package, `Authorisation)))
    (Conex_repository.verify_package r auth rel)

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
    type t = [ Conex_repository.base_error  | `AuthRelMismatch of name * name | `NotInReleases of name * S.t | `ChecksumsDiff of name * name list * name list * (Release.c * Release.c) list ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && typ_equal w w' && typ_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && typ_equal r r' && S.equal js js'
      | `AuthRelMismatch (a, r), `AuthRelMismatch (a', r') -> name_equal a a' && name_equal r r'
      | `NotInReleases (c, r), `NotInReleases (c', r') -> name_equal c c' && S.equal r r'
      | `ChecksumsDiff (n, a, b, cs), `ChecksumsDiff (n', a', b', cs') ->
        name_equal n n' &&
        S.equal (S.of_list a) (S.of_list a') &&
        S.equal (S.of_list b) (S.of_list b') &&
        List.length cs = List.length cs' &&
        List.for_all (fun (c, d) -> List.exists (fun (c', d') -> Release.checksum_equal c c' && Release.checksum_equal d d') cs) cs'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let cs_base () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t ~releases:(S.singleton v) Uint.zero pname in
  let cs = Release.t Uint.zero v [] in
  Alcotest.check (result r_ok c_err) "checksum not signed"
    (Error (`NotSigned (v, `Release, S.empty)))
    (Conex_repository.verify_release r auth rel cs) ;
  let resources =
    let s, d = res (Package.wire rel)
    and s', d' = res (Release.wire cs)
    in
    [ Author.r Uint.zero pname s `Package d ; Author.r (Uint.of_int_exn 1) v s' `Release d' ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/checksum'"
    (Ok ()) (io.Conex_provider.write ["packages"; pname; v; "checksum"] "") ;
  match Conex_io.compute_release io Uint.zero v with
  | Error _ -> Alcotest.fail "should be able to compute checksum"
  | Ok on_disk ->
    Alcotest.check (result r_ok c_err) "good checksum"
      (Ok (`Signed id))
      (Conex_repository.verify_release r ~on_disk auth rel cs) ;
    let jid = "janitor" in
    let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
    let r = add_rs r jid resources in
    Alcotest.check (result r_ok c_err) "good checksum (both)"
      (Ok (`Both (id, S.singleton jid)))
      (Conex_repository.verify_release r auth rel cs)

let cs_quorum () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t ~releases:(S.singleton v) Uint.zero pname in
  let cs = Release.t Uint.zero v [] in
  let resources =
    let s, d = res (Package.wire rel)
    and s', d' = res (Release.wire cs)
    in
    [ Author.r Uint.zero pname s `Package d ; Author.r (Uint.of_int_exn 1) v s' `Release d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/checksum'"
    (Ok ()) (io.Conex_provider.write ["packages"; pname; v; "checksum"] "") ;
  match Conex_io.compute_release io Uint.zero v with
  | Error _ -> Alcotest.fail "should be able to compute checksum"
  | Ok on_disk ->
    Alcotest.check (result r_ok c_err) "good checksum (quorum)"
      (Ok (`Quorum (S.singleton jid)))
      (Conex_repository.verify_release r ~on_disk auth rel cs)

let cs_bad () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 () in
  let open Conex_provider in
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
    { Release.filename = "bar" ; size = Uint.of_int_exn 3 ; digest = (`SHA256, "LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm564=") } ;
    { Release.filename = "foo" ; size = Uint.of_int_exn 3 ; digest = (`SHA256, "/N4rLtula/QIYB+3If6bXDONEO5CnqBPrlURto+/j7k=") } ;
    { Release.filename = "files/patch1" ; size = Uint.of_int_exn 2 ; digest = (`SHA256, "9kVR/NbweCPLh5cc+5FEZCXaGChrOrHvk14MvXpp9oo=") } ;
    { Release.filename = "files/patch2" ; size = Uint.of_int_exn 2 ; digest = (`SHA256, "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=") }
  ]
  in
  let css = Release.t Uint.zero v csums in
  Alcotest.check (result rel ch_err) "checksum computation works"
    (Ok css) (Conex_io.compute_release io Uint.zero "foo.0") ;
  let css' = Release.t Uint.zero v (List.tl csums) in
  let css'' = Release.t Uint.zero v ({ Release.filename = "foobar" ; size = Uint.of_int_exn 3 ; digest = (`SHA256, "") } :: csums) in
  let other = { Release.filename = "bar" ; size = Uint.of_int_exn 3 ; digest = (`SHA256, "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=") } in
  let css''' = Release.t Uint.zero v (other :: List.tl csums) in
  let id = "id" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t ~releases:(S.singleton v) Uint.zero pname in
  let resources =
    let s, d = res (Package.wire rel)
    and s1, d1 = res (Release.wire css)
    and s2, d2 = res (Release.wire css')
    and s3, d3 = res (Release.wire css'')
    and s4, d4 = res (Release.wire css''')
    in
    [ Author.r Uint.zero pname s `Package d ;
      Author.r (Uint.of_int_exn 1) v s1 `Release d1 ;
      Author.r (Uint.of_int_exn 2) v s2 `Release d2 ;
      Author.r (Uint.of_int_exn 3) v s3 `Release d3 ;
      Author.r (Uint.of_int_exn 4) v s4 `Release d4 ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  match Conex_io.compute_release io Uint.zero v with
  | Error _ -> Alcotest.fail "should be able to compute checksum"
  | Ok on_disk ->
    Alcotest.check (result r_ok c_err) "good checksum (quorum)"
      (Ok (`Quorum (S.singleton jid)))
      (Conex_repository.verify_release r ~on_disk auth rel css) ;
    Alcotest.check (result r_ok c_err) "bad checksum (missing in cs file)"
      (Error (`ChecksumsDiff (v, [], ["bar"], [])))
      (Conex_repository.verify_release r ~on_disk auth rel css') ;
    Alcotest.check (result r_ok c_err) "bad checksum (missing on disk)"
      (Error (`ChecksumsDiff (v, ["foobar"], [], [])))
      (Conex_repository.verify_release r ~on_disk auth rel css'') ;
    Alcotest.check (result r_ok c_err) "bad checksum (differ)"
      (Error (`ChecksumsDiff (v, [], [], [(List.hd csums, other)])))
      (Conex_repository.verify_release r ~on_disk auth rel css''')

let cs_bad_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let reln = "foo" in
  let v = reln ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t ~releases:(S.singleton v) Uint.zero reln in
  let cs = Release.t Uint.zero v [] in
  let resources =
    let s, d = res (Package.wire rel)
    and s', d' = res (Release.wire cs)
    in
    [ Author.r Uint.zero reln s `Package d ; Author.r (Uint.of_int_exn 1) v s' `Release d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "bad name (auth != rel)"
    (Error (`AuthRelMismatch (pname, reln)))
    (Conex_repository.verify_release r auth rel cs)

let cs_bad_name2 () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let reln = pname ^ ".0" in
  let v = reln ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t ~releases:(S.singleton reln) Uint.zero pname in
  let cs = Release.t Uint.zero v [] in
  let resources =
    let s, d = res (Package.wire rel)
    and s', d' = res (Release.wire cs)
    in
    [ Author.r Uint.zero pname s `Package d ; Author.r (Uint.of_int_exn 1) v s' `Release d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "bad name (not member of releases)"
    (Error (`NotInReleases (v, rel.Package.releases)))
    (Conex_repository.verify_release r auth rel cs)

let cs_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t ~releases:(S.singleton v) Uint.zero pname in
  let cs = Release.t Uint.zero v [] in
  let resources =
    let s, d = res (Package.wire rel)
    and s', d' = res (Release.wire cs)
    in
    [ Author.r Uint.zero pname s `Package d ; Author.r (Uint.of_int_exn 1) pname s' `Release d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "wrong name"
    (Error (`InvalidName (v, pname)))
    (Conex_repository.verify_release r auth rel cs)

let cs_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.t ~authorised:(S.singleton id) Uint.zero pname in
  let rel = Package.t ~releases:(S.singleton v) Uint.zero pname in
  let cs = Release.t Uint.zero v [] in
  let resources =
    let s, d = res (Package.wire rel)
    and s', d' = res (Release.wire cs)
    in
    [ Author.r Uint.zero pname s `Package d ; Author.r (Uint.of_int_exn 1) v s' `Package d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.t ~members:(S.singleton jid) Uint.zero "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "wrong resource"
    (Error (`InvalidResource (v, `Release, `Package)))
    (Conex_repository.verify_release r auth rel cs)

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
  "RepositoryPackage", rel_repo_tests ;
  "RepositoryRelease", cs_repo_tests ;
]
