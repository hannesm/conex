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
    (Error (`NotFound "foo")) (Repository.read_janitorindex r "foo") ;
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

let ok =
  let module M = struct
    type t = Repository.ok
    let pp = Repository.pp_ok
    let equal a b = match a, b with
      | `Identifier a, `Identifier b -> a = b
      | `Quorum js, `Quorum is -> List.length js = List.length is && List.for_all (fun a -> List.mem a is) js
      | `Both (a, js), `Both (b, is) -> a = b && List.length js = List.length is && List.for_all (fun a -> List.mem a is) js
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let key_sign_r () =
  let p = Mem.mem_provider () in
  let r = Repository.repository ~quorum:0 p in
  let sign pub priv =
    let raw = Data.publickey_raw pub in
    let s = Private.sign "foo" priv `PublicKey raw in
    Publickey.add_sig pub s
  in
  let k, pk = gen_pub ~role:`Janitor "foo" in
  let k' = sign k pk in
  Alcotest.check (result ok err) "key signed properly"
    (Ok (`Both ("foo", []))) (Repository.verify_key r k') ;
  let r' = Repository.repository ~quorum:1 p in
  Alcotest.check (result ok err) "key requires quorum"
    (Error (`InsufficientQuorum ("foo", []))) (Repository.verify_key r' k')

let repo_tests = [
  "empty repo", `Quick, empty_r ;
  "key repo", `Quick, key_r ;
  "signed key repo", `Quick, key_sign_r ;
]

let tests = [
  "Memory provider", mem_provider_tests ;
  "Repository", repo_tests ;
]
