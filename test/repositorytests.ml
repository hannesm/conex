open Conex_result
open Conex_core
open Conex_resource

open Common

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
      | `NotFound (a, a'), `NotFound (b, b') -> name_equal a b && name_equal a' b'
      | `NameMismatch (a, b), `NameMismatch (a', b') -> name_equal a a' && name_equal b b'
      | `ParseError (n, _), `ParseError (n', _) -> name_equal n n'
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

(* basic operations work, and we have an in-memory data provider!  let the games begin *)
let empty_r () =
  let io = Mem.mem_provider () in
  Alcotest.check sset "empty repo has no keys" S.empty (Conex_io.ids io) ;
  Alcotest.check sset "empty repo has no authorisations" S.empty (Conex_io.items io) ;
  Alcotest.check sset "empty repo has no subitems" S.empty (Conex_io.subitems io "foo") ;
  Alcotest.check (result auth re) "reading authorisation foo in empty repo fails"
    (Error (`NotFound ("authorisation", "foo"))) (Conex_io.read_authorisation io "foo") ;
  Alcotest.check (result releases re) "reading releases foo in empty repo fails"
    (Error (`NotFound ("releases", "foo"))) (Conex_io.read_releases io "foo") ;
  Alcotest.check (result ji re) "reading janitorindex foo in empty repo fails"
    (Error (`NotFound ("index", "foo"))) (Conex_io.read_index io "foo") ;
  Alcotest.check (result cs re) "reading checksum foo.0 in empty repo fails"
    (Error (`NotFound ("checksum", "foo.0"))) (Conex_io.read_checksum io "foo.0") ;
  Alcotest.check (result cs ch_err) "computing checksum foo.0 in empty repo fails"
    (Error (`FileNotFound "foo.0")) (Conex_io.compute_checksum io "foo.0")

let key_r () =
  let io = Mem.mem_provider () in
  let k = Index.index "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing index 'foo'"
    (Ok ()) (Conex_io.write_index io k) ;
  Alcotest.check sset "key repo has one key"
    (S.singleton "foo") (Conex_io.ids io) ;
  Alcotest.check (result Alcotest.unit str_err) "writing index 'foo' again"
    (Ok ()) (Conex_io.write_index io k) ;
  Alcotest.check sset "key repo+ has one key"
    (S.singleton "foo") (Conex_io.ids io) ;
  let k2 = Index.index "foobar" in
  Alcotest.check (result Alcotest.unit str_err) "writing index 'foobar'"
    (Ok ()) (Conex_io.write_index io k2) ;
  Alcotest.check sset "key repo has two keys"
    (S.add "foobar" (S.singleton "foo")) (Conex_io.ids io) ;
  let jk = Index.index "janitor" in
  Alcotest.check (result Alcotest.unit str_err) "writing index 'janitor'"
    (Ok ()) (Conex_io.write_index io jk) ;
  Alcotest.check sset "key repo has three keys"
    (S.add "janitor" (S.add "foobar" (S.singleton "foo")))
    (Conex_io.ids io)


let team_r () =
  let r = Conex_repository.repository ~quorum:10 () in
  Alcotest.(check int "quorum is correct" 10 (Conex_repository.quorum r)) ;
  Alcotest.(check (option sset) "team foo is empty" None (Conex_repository.find_team r "foo")) ;
  let t = Team.team ~members:(S.singleton "bar") "foo" in
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
    { Checksum.filename = "bar" ; size = Uint.of_int 3 ; digest = (`SHA256, "LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm564=") } ;
    { Checksum.filename = "foo" ; size = Uint.of_int 3 ; digest = (`SHA256, "/N4rLtula/QIYB+3If6bXDONEO5CnqBPrlURto+/j7k=") } ;
    { Checksum.filename = "files/patch1" ; size = Uint.of_int 2 ; digest = (`SHA256, "9kVR/NbweCPLh5cc+5FEZCXaGChrOrHvk14MvXpp9oo=") } ;
    { Checksum.filename = "files/patch2" ; size = Uint.of_int 2 ; digest = (`SHA256, "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=") }
  ]
  in
  let css = Checksum.checksums "foo.0" csums in
  Alcotest.check (result cs ch_err) "checksum computation works"
    (Ok css) (Conex_io.compute_checksum io "foo.0")

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
  let bad_m =
    M.add "members" (String "bl")
      (M.add "name" (String "foo") bad_n)
  in
  Alcotest.check (result team str_err) "couldn't parse team members"
    (Error "") (Team.of_wire bad_m) ;
  let bad_m =
    M.add "members" (List [ Int Uint.zero ])
      (M.add "name" (String "foo") bad_n)
  in
  Alcotest.check (result team str_err) "couldn't parse team members (not a set)"
    (Error "") (Team.of_wire bad_m) ;
  let good_t =
    M.add "members" (List [ String "foo" ])
      (M.add "name" (String "foo") bad_n)
  in
  let t = Team.team ~members:(S.singleton "foo") "foo" in
  Alcotest.check (result team str_err) "could parse team"
    (Ok t) (Team.of_wire good_t) ;
  Alcotest.check (result team str_err) "could unparse/parse team"
    (Ok t) (Team.of_wire (Team.wire t)) ;
  Alcotest.check (result team str_err) "could unparse/parse team"
    (Ok t) (Conex_data.decode (Conex_data.encode (Team.wire t)) >>= Team.of_wire) ;
  let checksum =
    M.add "filename" (String "foo")
      (M.add "size" (Int (Uint.of_int 3))
         (M.add "digest" (List [ String "SHA256" ; String "/N4rLtula/QIYB+3If6bXDONEO5CnqBPrlURto+/j7k=" ]) M.empty))
  in
  let css = M.add "name" (String "foo")
      (M.add "counter" (Int Uint.zero)
         (M.add "version" (Int Uint.zero)
            (M.add "files" (List [Map checksum]) M.empty)))
  in
  let csum = Checksum.checksum "foo" "bar" in
  let csums = Checksum.checksums "foo" [csum] in
  Alcotest.check (result cs str_err) "can parse checksum"
    (Ok csums) (Checksum.of_wire css) ;
  let s = List [ Int Uint.zero ; String (sigtype_to_string `RSA_PSS_SHA256) ; String "foobar" ; String "barf" ] in
  let s' = ({ created = Uint.zero ; sigtyp = `RSA_PSS_SHA256 ; signame = "barf" }, "frab") in
  let idx =
    M.add "name" (String "foo")
      (M.add "counter" (Int Uint.zero)
         (M.add "version" (Int Uint.zero) M.empty))
  in
  let empty_idx =
    M.add "queued" (List [])
      (M.add "signatures" (List [s]) M.empty)
  in
  let idxs =
    M.add "signed" (Map idx) empty_idx
  in
  let idx' = Index.index ~signatures:[s'] "foo" in
  Alcotest.check (result ji str_err) "can parse index"
    (Ok idx') (Index.of_wire idxs) ;
  Alcotest.check (result ji str_err) "can unparse/parse index"
    (Ok idx') (Index.of_wire (Index.wire idx')) ;
  let r =
    M.add "index" (Int Uint.zero)
      (M.add "name" (String "foobar")
         (M.add "size" (Int Uint.zero)
            (M.add "resource" (String "team")
               (M.add "digest" (List [ String "SHA256" ; String "01234567890123456789012345678901234567890123" ]) M.empty))))
  in
  let r' = Index.r Uint.zero "foobar" Uint.zero `Team (`SHA256, "01234567890123456789012345678901234567890123") in
  let idx = M.add "resources" (List [ Map r ]) idx in
  let idxs =
    M.add "signed" (Map idx) empty_idx
  in
  let idx' = Index.index ~signatures:[s'] ~resources:[r'] "foo" in
  Alcotest.check (result ji str_err) "can parse index"
    (Ok idx') (Index.of_wire idxs) ;
  Alcotest.check (result ji str_err) "can unparse/parse index"
    (Ok idx') (Index.of_wire (Index.wire idx')) ;
  let bad_r = M.add "resource" (String "teamfoo") r in
  let idx = M.add "resources" (List [ Map bad_r ]) idx in
  let idxs =
    M.add "signed" (Map idx) empty_idx
  in
  Alcotest.check (result ji str_err) "cannot parse index, bad resource type"
    (Error "") (Index.of_wire idxs)

let bad_id_r () =
  let open Conex_provider in
  let io = Mem.mem_provider () in
  Alcotest.check (result ji re) "index foo not found"
    (Error (`NotFound ("index", "foo"))) (Conex_io.read_index io "foo") ;
  Alcotest.check (result team re) "team foo not found"
    (Error (`NotFound ("team", "foo"))) (Conex_io.read_team io "foo") ;
  Alcotest.check (result id re) "ID foo not found"
    (Error (`NotFound ("index", "foo"))) (Conex_io.read_id io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'barf' to 'id/foo'"
    (Ok ()) (io.write ["id"; "foo"] "barf") ;
  Alcotest.check (result ji re) "parse error on index foo"
    (Error (`ParseError ("foo", ""))) (Conex_io.read_index io "foo") ;
  Alcotest.check (result team re) "parse error on team foo"
    (Error (`ParseError ("foo", ""))) (Conex_io.read_team io "foo") ;
  Alcotest.check (result id re) "parse error on id foo"
    (Error (`ParseError ("foo", ""))) (Conex_io.read_id io "foo") ;
  let idx = Index.index "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing index 'foo'"
    (Ok ()) (Conex_io.write_index io idx) ;
  Alcotest.check (result team re) "parse error on team foo"
    (Error (`ParseError ("foo", ""))) (Conex_io.read_team io "foo") ;
  Alcotest.check (result ji re) "index foo parses"
    (Ok idx) (Conex_io.read_index io "foo") ;
  Alcotest.check (result id re) "id foo parses"
    (Ok (`Id idx)) (Conex_io.read_id io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'id/foobar'"
    (Ok ()) (io.write ["id"; "foobar"] (Conex_data.encode (Index.wire idx))) ;
  Alcotest.check (result team re) "parse error on team foobar"
    (Error (`ParseError ("foobar", ""))) (Conex_io.read_team io "foobar") ;
  Alcotest.check (result ji re) "id foobar namemismatch"
    (Error (`NameMismatch ("foobar", "foo"))) (Conex_io.read_index io "foobar") ;
  Alcotest.check (result id re) "namemismatch id foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Conex_io.read_id io "foobar") ;
  let t = Team.team "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing team 'foo'"
    (Ok ()) (Conex_io.write_team io t) ;
  Alcotest.check (result ji re) "parse error on index foo"
    (Error (`ParseError ("foo", ""))) (Conex_io.read_index io "foo") ;
  Alcotest.check (result team re) "team foo parses"
    (Ok t) (Conex_io.read_team io "foo") ;
  Alcotest.check (result id re) "id foo parses"
    (Ok (`Team t)) (Conex_io.read_id io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'id/foobar'"
    (Ok ()) (io.write ["id"; "foobar"] (Conex_data.encode (Team.wire t))) ;
  Alcotest.check (result ji re) "parse error on index foobar"
    (Error (`ParseError ("foobar", ""))) (Conex_io.read_index io "foobar") ;
  Alcotest.check (result team re) "name mismatch on team foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Conex_io.read_team io "foobar") ;
  Alcotest.check (result id re) "name mismatch on id foo"
    (Error (`NameMismatch ("foobar", "foo"))) (Conex_io.read_id io "foobar")

let bad_idx_r () =
  let open Conex_provider in
  let io = Mem.mem_provider () in
  Alcotest.check (result ji re) "index foo not found"
    (Error (`NotFound ("index", "foo"))) (Conex_io.read_index io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'bla' to 'id/foo'"
    (Ok ()) (io.write ["id"; "foo"] "bla") ;
  Alcotest.check (result ji re) "bad index foo"
    (Error (`ParseError ("foo", ""))) (Conex_io.read_index io "foo") ;
  let idx = Index.index "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing index 'foo'"
    (Ok ()) (Conex_io.write_index io idx) ;
  Alcotest.check (result ji re) "good index foo"
    (Ok idx) (Conex_io.read_index io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'id/foobar'"
    (Ok ()) (io.write ["id"; "foobar"] (Conex_data.encode (Index.wire idx))) ;
  Alcotest.check (result ji re) "name mismatch in foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Conex_io.read_index io "foobar")

let bad_auth_r () =
  let open Conex_provider in
  let io = Mem.mem_provider () in
  Alcotest.check (result auth re) "authorisation foo not found"
    (Error (`NotFound ("authorisation", "foo"))) (Conex_io.read_authorisation io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foobar' to 'packages/foo/authorisation'"
    (Ok ()) (io.write ["packages"; "foo"; "authorisation"] "foobar") ;
  Alcotest.check (result auth re) "parse error on authorisation foo"
    (Error (`ParseError ("foo", ""))) (Conex_io.read_authorisation io "foo") ;
  let a = Authorisation.authorisation "foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing authorisation 'foo'"
    (Ok ()) (Conex_io.write_authorisation io a) ;
  Alcotest.check (result auth re) "authorisation foo good"
    (Ok a) (Conex_io.read_authorisation io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'packages/foobar/authorisation'"
    (Ok ()) (io.write ["packages"; "foobar"; "authorisation"] (Conex_data.encode (Authorisation.wire a))) ;
  Alcotest.check (result auth re) "name mismatch on authorisation foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Conex_io.read_authorisation io "foobar")

let bad_rel_r () =
  let open Conex_provider in
  let io = Mem.mem_provider () in
  Alcotest.check (result releases re) "releases foo not found"
    (Error (`NotFound ("releases", "foo"))) (Conex_io.read_releases io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foobar' to 'packages/foo/releases'"
    (Ok ()) (io.write ["packages"; "foo"; "releases"] "foobar") ;
  Alcotest.check (result releases re) "parse error on releases foo"
    (Error (`ParseError ("foo", ""))) (Conex_io.read_releases io "foo") ;
  let rel = match Releases.releases "foo" with Ok r -> r | Error _ -> Alcotest.fail "parsing releases foo" in
  Alcotest.check (result Alcotest.unit str_err) "writing releases 'foo'"
    (Ok ()) (Conex_io.write_releases io rel) ;
  Alcotest.check (result releases re) "releases foo good"
    (Ok rel) (Conex_io.read_releases io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'packages/foobar/releases'"
    (Ok ()) (io.write ["packages"; "foobar"; "releases"] (Conex_data.encode (Releases.wire rel))) ;
  Alcotest.check (result releases re) "name mismatch on releases foobar"
    (Error (`NameMismatch ("foobar", "foo"))) (Conex_io.read_releases io "foobar")

let bad_cs_r () =
  let open Conex_provider in
  let io = Mem.mem_provider () in
  Alcotest.check (result cs re) "checksum foo not found"
    (Error (`NotFound ("checksum", "foo"))) (Conex_io.read_checksum io "foo") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'foobar' to 'packages/foo/foo.0/checksum'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.0"; "checksum"] "foobar") ;
  Alcotest.check (result cs re) "parse error on checksum foo.0"
    (Error (`ParseError ("foo.0", ""))) (Conex_io.read_checksum io "foo.0") ;
  let c = Checksum.checksums "foo.0" [] in
  Alcotest.check (result Alcotest.unit str_err) "writing checksum foo.0"
    (Ok ()) (Conex_io.write_checksum io c) ;
  Alcotest.check (result cs re) "checksum foo.0 good"
    (Ok c) (Conex_io.read_checksum io "foo.0") ;
  Alcotest.check (result Alcotest.unit str_err) "writing sth to 'packages/foo/foo.1/checksum'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.1"; "checksum"] (Conex_data.encode (Checksum.wire c))) ;
  Alcotest.check (result cs re) "name mismatch on checksum foo.1"
    (Error (`NameMismatch ("foo.1", "foo.0"))) (Conex_io.read_checksum io "foo.1") ;
  Alcotest.check (result Alcotest.unit str_err) "writing 'blubb' to 'packages/foo/foo.2'"
    (Ok ()) (io.write ["packages"; "foo"; "foo.2"] "blubb") ;
  Alcotest.check (result cs ch_err) "checksum is a file, should be a directory"
    (Error (`NotADirectory "foo.2")) (Conex_io.compute_checksum io "foo.2")

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

let res d =
  let data = Conex_data.encode d in
  (Uint.of_int (String.length data), Conex_nocrypto.digest data)

let idx_sign () =
  let r = Conex_repository.repository ~quorum:0 () in
  let id = "foo" in
  let pub, priv = gen_pub () in
  let idx = Index.index ~keys:[pub] id in
  Alcotest.check (result r_ok verr) "empty index signed properly (no resources, quorum 0)"
    (Ok (r, [], id)) (Conex_repository.verify_index r idx) ;
  let pubenc = Conex_data.encode (Wire.wire_pub id pub) in
  let idx = Index.(add_resource idx (r (next_id idx) id (Uint.of_int (String.length pubenc)) `PublicKey (Conex_nocrypto.digest pubenc))) in
  let signed_idx = sign_idx idx priv in
  Alcotest.check (result r_ok verr) "signed_idx signed properly (1 resource, quorum 0)"
    (Ok (r, [], id)) (Conex_repository.verify_index r signed_idx) ;
  Alcotest.check (result r_ok verr) "idx signed properly (0 resources, 1 queued, quorum 0)"
    (Ok (r, [], id)) (Conex_repository.verify_index r idx) ;
  let idx, _ = Index.prep_sig idx in
  Alcotest.check (result r_ok verr) "idx not signed properly (1 resource, quorum 0)"
    (Error `NoSignature) (Conex_repository.verify_index r idx)

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
  let idx = Index.index ~keys:[pub] id in
  let s, d = res (Wire.wire_pub id pub) in
  let resources = [ Index.r Uint.zero id s `PublicKey d ] in
  let signed_idx = sign_idx idx priv in
  Alcotest.check (result r_ok verr) "idx signed properly"
    (Ok (r, [], id)) (Conex_repository.verify_index r signed_idx) ;
  let signatures = signed_idx.Index.signatures in
  let idx' = Index.index ~keys:[pub] ~resources ~signatures id in
  Alcotest.check (result r_ok verr) "idx' not properly signed"
    (Error `NoSignature) (Conex_repository.verify_index r idx') ;
  let idx'' = Index.index ~keys:[pub] ~queued:resources ~signatures ~counter:Uint.(of_int 1) id in
  Alcotest.check (result r_ok verr) "idx'' properly signed (queue)"
    (Ok (r, [], id)) (Conex_repository.verify_index r idx'') ;
  let signed_idx' = sign_idx idx' priv in
  Alcotest.check (result r_ok verr) "signed_idx' signed properly"
    (Ok (r, [], id)) (Conex_repository.verify_index r signed_idx') ;
  match Conex_repository.add_valid_resource r id (List.hd resources) with
  | Ok r ->
    Alcotest.check (result r_fake str_err) "add errors on wrong name"
      (Error "") (Conex_repository.add_valid_resource r id
                    (Index.r Uint.zero "barf" s `PublicKey d)) ;
    Alcotest.check (result r_fake str_err) "add errors on wrong resource"
      (Error "") (Conex_repository.add_valid_resource r id
                    (Index.r Uint.zero id s `Team d)) ;
    Alcotest.check (result r_fake str_err) "add errors on wrong size"
      (Error "") (Conex_repository.add_valid_resource r id
                    (Index.r Uint.zero id (Uint.of_int 23) `PublicKey d))
  | Error e -> Alcotest.fail e

let idx_s_v_dupl () =
  let r = Conex_repository.repository ~quorum:0 () in
  let id = "foo" in
  let pub, priv = gen_pub () in
  let s, d = res (Wire.wire_pub id pub) in
  let resources = [
    Index.r Uint.zero id s `PublicKey d ;
    Index.r (Uint.of_int 1) id s `Team d ;
  ] in
  let idx = Index.index ~keys:[pub] ~resources id in
  let signed_idx = sign_idx idx priv in
  Alcotest.check (result r_ok verr) "idx signed properly, but one resource ignored"
    (Ok (r, [""], id)) (Conex_repository.verify_index r signed_idx) ;
  let signatures =
    ({ created = Uint.zero ; sigtyp = `RSA_PSS_SHA256 ; signame = "foobar" }, "foobar") ::
    signed_idx.Index.signatures @
    [ ({ created = Uint.zero ; sigtyp = `RSA_PSS_SHA256 ; signame = "foobar" }, "foobar") ]
  in
  let signed_idx = Index.index ~keys:[pub] ~counter:(Uint.of_int 1) ~resources ~signatures id in
  Alcotest.check (result r_ok verr) "idx signed properly (second sig), but one resource ignored"
    (Ok (r, [""], id)) (Conex_repository.verify_index r signed_idx)

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
    type t = [ Conex_repository.base_error | `InsufficientQuorum of name * resource * S.t | `MissingSignature of identifier ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && resource_equal w w' && resource_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && resource_equal r r' && S.equal js js'
      | `InsufficientQuorum (id, r, q), `InsufficientQuorum (id', r', q') -> id_equal id id' && S.equal q q' && resource_equal r r'
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
    let s, d = res (Wire.wire_pub id pub) in
    [ Index.r Uint.zero id s `PublicKey d ]
  in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  Alcotest.check (result k_ok k_err) "not signed"
    (Error (`NotSigned (id, `PublicKey, S.empty)))
    (Conex_repository.verify_key r id pub) ;
  let r' = add_rs r jid resources in
  Alcotest.check (result k_ok k_err) "publickey missing self-sig"
    (Error (`MissingSignature id))
    (Conex_repository.verify_key r' id pub) ;
  let r'' = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "publickey missing quorum"
    (Error (`InsufficientQuorum (id, `PublicKey, S.empty)))
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
    let s, d = res (Wire.wire_pub id pub) in
    [ Index.r Uint.zero id s `PublicKey d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 0"
    (Error (`InsufficientQuorum (id, `PublicKey, S.empty)))
    (Conex_repository.verify_key r id pub) ;
  let jidx r jid =
    let r =
      let mems = match Conex_repository.find_team r "janitors" with None -> S.empty | Some s -> s in
      Conex_repository.add_team r (Team.team ~members:(S.add jid mems) "janitors")
    in
    add_rs r jid resources
  in
  let r = jidx r "jana" in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 1"
    (Error (`InsufficientQuorum (id, `PublicKey, S.singleton "jana")))
    (Conex_repository.verify_key r id pub) ;
  let r = jidx r "janb" in
  Alcotest.check (result k_ok k_err) "publickey missing quorum 2"
    (Error (`InsufficientQuorum (id, `PublicKey, S.add "janb" (S.singleton "jana"))))
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
    let s, d = res (Wire.wire_pub id pub)
    and s', d' = res (Wire.wire_pub id' pub)
    in
    [ Index.r Uint.zero id s `PublicKey d ; Index.r (Uint.of_int 1) id' s' `PublicKey d' ]
  in
  let r = add_rs r jid resources in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "missing quorum"
    (Error (`InsufficientQuorum (id, `PublicKey, S.empty)))
    (Conex_repository.verify_key r id pub) ;
  Alcotest.check (result k_ok k_err) "missing quorum for empty"
    (Error (`NotSigned (id', `PublicKey, S.empty)))
    (Conex_repository.verify_key r id' pub)

let k_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let resources =
    let s, d = res (Wire.wire_pub id pub) in
    [ Index.r Uint.zero id s `Checksums d ]
  in
  let r = add_rs r jid resources in
  let r = add_rs r id resources in
  Alcotest.check (result k_ok k_err) "wrong resource"
    (Error (`InvalidResource (id, `PublicKey, `Checksums)))
    (Conex_repository.verify_key r id pub)

let k_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let jid = "aaa" in
  let id = "foo" in
  let pub, _priv = gen_pub () in
  let resources =
    let s, d = res (Wire.wire_pub id pub) in
    [ Index.r Uint.zero jid s `PublicKey d ]
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
    type t = [ Conex_repository.base_error | `InsufficientQuorum of name * resource * S.t ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InsufficientQuorum (id, r, q), `InsufficientQuorum (id', r', q') -> id_equal id id' && S.equal q q' && resource_equal r r'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && resource_equal w w' && resource_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && resource_equal r r' && S.equal js js'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let team () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let team = Team.team pname in
  Alcotest.check (result t_ok a_err) "team missing quorum"
    (Error (`InsufficientQuorum (pname, `Team, S.empty)))
    (Conex_repository.verify_team r team) ;
  let resources =
    let s, d = res (Team.wire team) in
    [ Index.r Uint.zero pname s `Team d ]
  in
  let j_sign r jid =
    let r =
      let mems = match Conex_repository.find_team r "janitors" with None -> S.empty | Some s -> s in
      Conex_repository.add_team r (Team.team ~members:(S.add jid mems) "janitors")
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
  let team = Team.team pname in
  let pub, _priv = gen_pub () in
  let resources =
    let s, d = res (Team.wire team)
    and s', d' = res (Wire.wire_pub id pub)
    in
    [ Index.r Uint.zero pname s `Team d ; Index.r (Uint.of_int 1) id s' `PublicKey d' ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result t_ok a_err) "team missing quorum"
    (Error (`InsufficientQuorum (pname, `Team, S.empty)))
    (Conex_repository.verify_team r team) ;
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "team ok"
    (Ok (r, `Quorum (S.singleton jid)))
    (Conex_repository.verify_team r team)

let team_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let team = Team.team pname in
  let resources =
    let s, d = res (Team.wire team) in
    [ Index.r Uint.zero pname s `Checksums d ]
  in
  let jid = "aaa" in
  let r = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "wrong resource"
    (Error (`InvalidResource (pname, `Team, `Checksums)))
    (Conex_repository.verify_team r team)

let team_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let team = Team.team pname in
  let jid = "aaa" in
  let resources =
    let s, d = res (Team.wire team) in
    [ Index.r Uint.zero "barf" s `Team d ]
  in
  let r = add_rs r jid resources in
  Alcotest.check (result t_ok a_err) "wrong name"
    (Error (`InvalidName (pname, "barf")))
    (Conex_repository.verify_team r team)

let team_dyn () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let team = Team.team pname in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors")
  in
  let resources =
    let s, d = res (Team.wire team) in
    [ Index.r Uint.zero pname s `Team d ]
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
    [ Index.r Uint.zero pname s `Team d ]
  in
  let r' = add_rs r jid resources in
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
  let auth = Authorisation.authorisation pname in
  Alcotest.check (result a_ok a_err) "auth missing quorum"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.empty)))
    (Conex_repository.verify_authorisation r auth) ;
  let resources =
    let s, d = res (Authorisation.wire auth) in
    [ Index.r Uint.zero pname s `Authorisation d ]
  in
  let j_sign r jid =
    let r =
      let mems = match Conex_repository.find_team r "janitors" with None -> S.empty | Some s -> s in
      Conex_repository.add_team r (Team.team ~members:(S.add jid mems) "janitors")
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
  let auth = Authorisation.authorisation pname in
  let pub, _priv = gen_pub () in
  let resources =
    let s, d = res (Authorisation.wire auth)
    and s', d' = res (Wire.wire_pub id pub)
    in
    [ Index.r Uint.zero pname s `Authorisation d ; Index.r (Uint.of_int 1) id s' `PublicKey d' ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result a_ok a_err) "auth missing quorum"
    (Error (`InsufficientQuorum (pname, `Authorisation, S.empty)))
    (Conex_repository.verify_authorisation r auth) ;
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "auth ok"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.verify_authorisation r auth)

let a_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let auth = Authorisation.authorisation pname in
  let resources =
    let s, d = res (Authorisation.wire auth) in
    [ Index.r Uint.zero pname s `Checksums d ]
  in
  let jid = "aaa" in
  let r = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "wrong resource"
    (Error (`InvalidResource (pname, `Authorisation, `Checksums)))
    (Conex_repository.verify_authorisation r auth)

let a_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let auth = Authorisation.authorisation pname in
  let jid = "aaa" in
  let resources =
    let s, d = res (Authorisation.wire auth) in
    [ Index.r Uint.zero "barf" s `Authorisation d ]
  in
  let r = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "wrong name"
    (Error (`InvalidName (pname, "barf")))
    (Conex_repository.verify_authorisation r auth)

let auth_dyn () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop" in
  let auth = Authorisation.authorisation pname in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors")
  in
  let resources =
    let s, d = res (Authorisation.wire auth) in
    [ Index.r Uint.zero pname s `Authorisation d ]
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
    [ Index.r Uint.zero pname s `Authorisation d ]
  in
  let r' = add_rs r jid resources in
  Alcotest.check (result a_ok a_err) "authorisation properly signed"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.verify_authorisation r' auth) ;
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
    type t = [ Conex_repository.base_error | `AuthRelMismatch of name * name | `InvalidReleases of name * S.t * S.t ]
    let pp = Conex_repository.pp_error
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
  | Error _ -> Alcotest.fail "shouldn't happen"

let rel () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel pname in
  Alcotest.check (result r_ok r_err) "not signed"
    (Error (`NotSigned (pname, `Releases, S.empty)))
    (Conex_repository.verify_releases r auth rel) ;
  let resources =
    let s, d = res (Releases.wire rel) in
    [ Index.r Uint.zero pname s `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "properly signed"
    (Ok (`Signed id))
    (Conex_repository.verify_releases r auth rel)

let rel_quorum () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel pname in
  let jid = "janitor" in
  let resources =
    let s, d = res (Releases.wire rel) in
    [ Index.r Uint.zero pname s `Releases d ]
  in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok r_err) "properly signed (quorum)"
    (Ok (`Quorum (S.singleton jid)))
    (Conex_repository.verify_releases r auth rel) ;
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "properly signed (both)"
    (Ok (`Both (id, S.singleton jid)))
    (Conex_repository.verify_releases r auth rel)

let rel_not_authorised () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton "foo") pname in
  let rel = safe_rel pname in
  let resources =
    let s, d = res (Releases.wire rel) in
    [ Index.r Uint.zero pname s `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "not authorised"
    (Error (`NotSigned (pname, `Releases, S.empty)))
    (Conex_repository.verify_releases r auth rel)

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
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let v = pname ^ ".0" in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let resources =
    let s, d = res (Releases.wire rel) in
    [ Index.r Uint.zero pname s `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "missing on disk"
    (Error (`InvalidReleases (pname, S.empty, S.singleton v)))
    (Conex_repository.verify_releases r ~on_disk:(safe_rel pname) auth rel) ;
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/checksum'"
    (Ok ()) (io.Conex_provider.write ["packages"; pname; v; "checksum"] "") ;
  match Conex_io.compute_releases io pname with
  | Error _ -> Alcotest.fail "should be able to compute releases0"
  | Ok on_disk ->
    Alcotest.check (result r_ok r_err) "all good"
      (Ok (`Signed id))
      (Conex_repository.verify_releases r ~on_disk auth rel) ;
    let v2 = pname ^ ".1" in
    Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v2/checksum'"
      (Ok ()) (io.Conex_provider.write ["packages"; pname; v2; "checksum"] "") ;
    match Conex_io.compute_releases io pname with
    | Error _ -> Alcotest.fail "shold be able to compute releases1"
    | Ok on_disk ->
      Alcotest.check (result r_ok r_err) "missing in releases"
        (Error (`InvalidReleases (pname, S.singleton v2, S.empty)))
        (Conex_repository.verify_releases r ~on_disk auth rel) ;
      let v3 = pname ^ ".2" in
      Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$oname/$v3/checksum'"
        (Ok ()) (io.Conex_provider.write ["packages"; pname; v3; "checksum"] "") ;
      match Conex_io.compute_releases io pname with
      | Error _ -> Alcotest.fail "shoul be able to compute releases2"
      | Ok on_disk ->
        Alcotest.check (result r_ok r_err) "missing in releases"
          (Error (`InvalidReleases (pname, S.add v3 (S.singleton v2), S.empty)))
          (Conex_repository.verify_releases r ~on_disk auth rel)


let rel_name_mismatch () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) "foo" in
  let rel = safe_rel pname in
  let resources =
    let s, d = res (Releases.wire rel) in
    [ Index.r Uint.zero pname s `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "releases and authorisation names do not match"
    (Error (`AuthRelMismatch ("foo", pname)))
    (Conex_repository.verify_releases r auth rel)

let rel_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel pname in
  let resources =
    let s, d = res (Releases.wire rel) in
    [ Index.r Uint.zero "foo" s `Releases d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "wrong name in releases"
    (Error (`InvalidName (pname, "foo")))
    (Conex_repository.verify_releases r auth rel)

let rel_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel pname in
  let resources =
    let s, d = res (Releases.wire rel) in
    [ Index.r Uint.zero pname s `Authorisation d ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result r_ok r_err) "wrong resource for releases"
    (Error (`InvalidResource (pname, `Releases, `Authorisation)))
    (Conex_repository.verify_releases r auth rel)

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
    type t = [ Conex_repository.base_error  | `AuthRelMismatch of name * name | `NotInReleases of name * S.t | `ChecksumsDiff of name * name list * name list * (Checksum.c * Checksum.c) list ]
    let pp = Conex_repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InvalidResource (n, w, h), `InvalidResource (n', w', h') -> name_equal n n' && resource_equal w w' && resource_equal h h'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && resource_equal r r' && S.equal js js'
      | `AuthRelMismatch (a, r), `AuthRelMismatch (a', r') -> name_equal a a' && name_equal r r'
      | `NotInReleases (c, r), `NotInReleases (c', r') -> name_equal c c' && S.equal r r'
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
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let cs = Checksum.checksums v [] in
  Alcotest.check (result r_ok c_err) "checksum not signed"
    (Error (`NotSigned (v, `Checksums, S.empty)))
    (Conex_repository.verify_checksum r auth rel cs) ;
  let resources =
    let s, d = res (Releases.wire rel)
    and s', d' = res (Checksum.wire cs)
    in
    [ Index.r Uint.zero pname s `Releases d ; Index.r (Uint.of_int 1) v s' `Checksums d' ]
  in
  let r = add_rs r id resources in
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/checksum'"
    (Ok ()) (io.Conex_provider.write ["packages"; pname; v; "checksum"] "") ;
  match Conex_io.compute_checksum io v with
  | Error _ -> Alcotest.fail "should be able to compute checksum"
  | Ok on_disk ->
    Alcotest.check (result r_ok c_err) "good checksum"
      (Ok (`Signed id))
      (Conex_repository.verify_checksum r ~on_disk auth rel cs) ;
    let jid = "janitor" in
    let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
    let r = add_rs r jid resources in
    Alcotest.check (result r_ok c_err) "good checksum (both)"
      (Ok (`Both (id, S.singleton jid)))
      (Conex_repository.verify_checksum r auth rel cs)

let cs_quorum () =
  let io = Mem.mem_provider () in
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (Releases.wire rel)
    and s', d' = res (Checksum.wire cs)
    in
    [ Index.r Uint.zero pname s `Releases d ; Index.r (Uint.of_int 1) v s' `Checksums d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result Alcotest.unit str_err) "writing nothing to 'packages/$pname/$v/checksum'"
    (Ok ()) (io.Conex_provider.write ["packages"; pname; v; "checksum"] "") ;
  match Conex_io.compute_checksum io v with
  | Error _ -> Alcotest.fail "should be able to compute checksum"
  | Ok on_disk ->
    Alcotest.check (result r_ok c_err) "good checksum (quorum)"
      (Ok (`Quorum (S.singleton jid)))
      (Conex_repository.verify_checksum r ~on_disk auth rel cs)

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
    { Checksum.filename = "bar" ; size = Uint.of_int 3 ; digest = (`SHA256, "LCa0a2j/xo/5m0U8HTBBNBNCLXBkg7+g+YpeiGJm564=") } ;
    { Checksum.filename = "foo" ; size = Uint.of_int 3 ; digest = (`SHA256, "/N4rLtula/QIYB+3If6bXDONEO5CnqBPrlURto+/j7k=") } ;
    { Checksum.filename = "files/patch1" ; size = Uint.of_int 2 ; digest = (`SHA256, "9kVR/NbweCPLh5cc+5FEZCXaGChrOrHvk14MvXpp9oo=") } ;
    { Checksum.filename = "files/patch2" ; size = Uint.of_int 2 ; digest = (`SHA256, "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=") }
  ]
  in
  let css = Checksum.checksums v csums in
  Alcotest.check (result cs ch_err) "checksum computation works"
    (Ok css) (Conex_io.compute_checksum io "foo.0") ;
  let css' = Checksum.checksums v (List.tl csums) in
  let css'' = Checksum.checksums v ({ Checksum.filename = "foobar" ; size = Uint.of_int 3 ; digest = (`SHA256, "") } :: csums) in
  let other = { Checksum.filename = "bar" ; size = Uint.of_int 3 ; digest = (`SHA256, "OUbKZP942TymEJCkN8u2s9LKDUiPX5zPMFlgg2iydpM=") } in
  let css''' = Checksum.checksums v (other :: List.tl csums) in
  let id = "id" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let resources =
    let s, d = res (Releases.wire rel)
    and s1, d1 = res (Checksum.wire css)
    and s2, d2 = res (Checksum.wire css')
    and s3, d3 = res (Checksum.wire css'')
    and s4, d4 = res (Checksum.wire css''')
    in
    [ Index.r Uint.zero pname s `Releases d ;
      Index.r (Uint.of_int 1) v s1 `Checksums d1 ;
      Index.r (Uint.of_int 2) v s2 `Checksums d2 ;
      Index.r (Uint.of_int 3) v s3 `Checksums d3 ;
      Index.r (Uint.of_int 4) v s4 `Checksums d4 ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = add_rs r jid resources in
  match Conex_io.compute_checksum io v with
  | Error _ -> Alcotest.fail "should be able to compute checksum"
  | Ok on_disk ->
    Alcotest.check (result r_ok c_err) "good checksum (quorum)"
      (Ok (`Quorum (S.singleton jid)))
      (Conex_repository.verify_checksum r ~on_disk auth rel css) ;
    Alcotest.check (result r_ok c_err) "bad checksum (missing in cs file)"
      (Error (`ChecksumsDiff (v, [], ["bar"], [])))
      (Conex_repository.verify_checksum r ~on_disk auth rel css') ;
    Alcotest.check (result r_ok c_err) "bad checksum (missing on disk)"
      (Error (`ChecksumsDiff (v, ["foobar"], [], [])))
      (Conex_repository.verify_checksum r ~on_disk auth rel css'') ;
    Alcotest.check (result r_ok c_err) "bad checksum (differ)"
      (Error (`ChecksumsDiff (v, [], [], [(List.hd csums, other)])))
      (Conex_repository.verify_checksum r ~on_disk auth rel css''')

let cs_bad_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let reln = "foo" in
  let v = reln ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) reln in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (Releases.wire rel)
    and s', d' = res (Checksum.wire cs)
    in
    [ Index.r Uint.zero reln s `Releases d ; Index.r (Uint.of_int 1) v s' `Checksums d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "bad name (auth != rel)"
    (Error (`AuthRelMismatch (pname, reln)))
    (Conex_repository.verify_checksum r auth rel cs)

let cs_bad_name2 () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let reln = pname ^ ".0" in
  let v = reln ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton reln) pname in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (Releases.wire rel)
    and s', d' = res (Checksum.wire cs)
    in
    [ Index.r Uint.zero pname s `Releases d ; Index.r (Uint.of_int 1) v s' `Checksums d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "bad name (not member of releases)"
    (Error (`NotInReleases (v, rel.Releases.releases)))
    (Conex_repository.verify_checksum r auth rel cs)

let cs_wrong_name () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (Releases.wire rel)
    and s', d' = res (Checksum.wire cs)
    in
    [ Index.r Uint.zero pname s `Releases d ; Index.r (Uint.of_int 1) pname s' `Checksums d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "wrong name"
    (Error (`InvalidName (v, pname)))
    (Conex_repository.verify_checksum r auth rel cs)

let cs_wrong_resource () =
  let r = Conex_repository.repository ~quorum:1 () in
  let pname = "foop"
  and id = "id"
  in
  let v = pname ^ ".0" in
  let auth = Authorisation.authorisation ~authorised:(S.singleton id) pname in
  let rel = safe_rel ~releases:(S.singleton v) pname in
  let cs = Checksum.checksums v [] in
  let resources =
    let s, d = res (Releases.wire rel)
    and s', d' = res (Checksum.wire cs)
    in
    [ Index.r Uint.zero pname s `Releases d ; Index.r (Uint.of_int 1) v s' `Releases d' ]
  in
  let jid = "janitor" in
  let r = Conex_repository.add_team r (Team.team ~members:(S.singleton jid) "janitors") in
  let r = add_rs r jid resources in
  Alcotest.check (result r_ok c_err) "wrong resource"
    (Error (`InvalidResource (v, `Checksums, `Releases)))
    (Conex_repository.verify_checksum r auth rel cs)

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
