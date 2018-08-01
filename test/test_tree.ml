open Conex_utils

let t =
  let module M = struct
    type t = string Tree.t
    let pp = Tree.pp Format.pp_print_string
    let equal = Tree.equal (fun a b -> String.compare a b = 0)
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let lookp = Alcotest.(list string)
let look = Alcotest.option lookp

let basic_test () =
  Alcotest.check t "empty is really empty"
    Tree.empty Tree.empty ;
  Alcotest.check look "lookup for root in empty is Some []"
    (Some []) (Tree.lookup [] Tree.empty) ;
  Alcotest.check lookp "lookup_prefix for root in empty is []"
    [] (Tree.lookup_prefix [] Tree.empty) ;
  Alcotest.check look "lookup for foo in empty is None"
    None (Tree.lookup ["foo"] Tree.empty) ;
  Alcotest.check lookp "lookup_prefix for foo in empty is []"
    [] (Tree.lookup_prefix ["foo"] Tree.empty) ;
  let t = Tree.insert ["foo"] "abc" Tree.empty in
  Alcotest.check look "lookup for foo in tree with foo is abc"
    (Some ["abc"]) (Tree.lookup ["foo"] t) ;
  Alcotest.check lookp "lookup_prefix for foo in tree with foo is abc"
    ["abc"] (Tree.lookup_prefix ["foo"] t) ;
  Alcotest.check lookp "lookup_prefix for foo/bar in tree with foo is abc"
    ["abc"] (Tree.lookup_prefix ["foo";"bar"] t) ;
  Alcotest.check look "lookup for bar in tree with foo is None"
    None (Tree.lookup ["bar"] t) ;
  Alcotest.check lookp "lookup_prefix for bar in tree with foo is []"
    [] (Tree.lookup_prefix ["bar"] t) ;
  let t = Tree.insert ["foo"] "def" t in
  Alcotest.check look "lookup for foo in tree with foo is Some ['abc';'def']"
    (Some ["abc";"def"]) (Tree.lookup ["foo"] t) ;
  Alcotest.check look "lookup for bar in tree with foo is None"
    None (Tree.lookup ["bar"] t) ;
  let t = Tree.insert ["foo" ; "bar"] "ghi" t in
  Alcotest.check look "lookup for foo in tree with foo is Some ['abc';'def']"
    (Some ["abc";"def"]) (Tree.lookup ["foo"] t) ;
  Alcotest.check look "lookup for bar in tree with foo is None"
    None (Tree.lookup ["bar"] t) ;
  Alcotest.check look "lookup for foo/bar in tree with foo/bar is Some 'ghi'"
    (Some ["ghi"]) (Tree.lookup ["foo" ; "bar"] t) ;
  Alcotest.check lookp "lookup_prefix for foo/bar in tree with foo/bar is ['ghi']"
    ["ghi"] (Tree.lookup_prefix ["foo" ; "bar"] t) ;
  Alcotest.check lookp "lookup_prefix for foo in tree with foo/bar is ['abc';'def']"
    ["abc";"def"] (Tree.lookup_prefix ["foo"] t) ;
  let t = Tree.insert ["foo"] "jkl" t in
  Alcotest.check look "lookup for foo in tree with foo is Some ['abc';'def';'jkl']"
    (Some ["abc";"def";"jkl"]) (Tree.lookup ["foo"] t) ;
  Alcotest.check lookp "lookup_prefix for foo/bar in tree with foo is ['ghi']"
    ["ghi"] (Tree.lookup_prefix ["foo" ; "bar"] t) ;
  Alcotest.check look "lookup for bar in tree with foo is None"
    None (Tree.lookup ["bar"] t) ;
  Alcotest.check look "lookup for foo/bar in tree with foo/bar is Some 'ghi'"
    (Some ["ghi"]) (Tree.lookup ["foo" ; "bar"] t)


let tests = [
  "basic", `Quick, basic_test
]
