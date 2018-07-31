open Conex_utils

open Common

let p =
  let module M = struct
    type t = path
    let pp = pp_path
    let equal = path_equal
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let of_string () =
  Alcotest.check (result p str_err) "of_string foo/bar works"
    (Ok [ "foo" ; "bar" ]) (string_to_path "foo/bar") ;
  Alcotest.check (result p str_err) "of_string /foo/bar works"
    (Ok [ "foo" ; "bar" ]) (string_to_path "/foo/bar") ;
  Alcotest.check (result p str_err) "of_string foo//bar results in error"
    (Error "") (string_to_path "foo//bar") ;
  Alcotest.check (result p str_err) "of_string foo/./bar results in error"
    (Error "") (string_to_path "foo/./bar") ;
  Alcotest.check (result p str_err) "of_string foo/../bar results in error"
    (Error "") (string_to_path "foo/../bar")

let subpath () =
  Alcotest.(check bool "subpath ~parent:[] ['a';'b'] works"
              true (subpath ~parent:[] ["a";"b"])) ;
  Alcotest.(check bool "subpath ~parent:['a'] ['a';'b'] works"
              true (subpath ~parent:["a"] ["a";"b"])) ;
  Alcotest.(check bool "subpath ~parent:['a';'b'] ['a';'b'] is false"
              false (subpath ~parent:["a";"b"] ["a";"b"])) ;
  Alcotest.(check bool "subpath ~parent:['a';'b'] ['a'] is false"
              false (subpath ~parent:["a";"b"] ["a"])) ;
  Alcotest.(check bool "subpath ~parent:['a'] ['b'] is false"
              false (subpath ~parent:["a"] ["b"])) ;
  Alcotest.(check bool "subpath ~parent:['a';'b'] ['a';'c'] is false"
              false (subpath ~parent:["a";"b"] ["a";"c"]))

let tests = [
  "of_string path", `Quick, of_string ;
  "subpath", `Quick, subpath ;
]
