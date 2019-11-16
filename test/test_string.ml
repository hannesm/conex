
(* String.concat "/" (String.cuts '/' xs) == xs *)
let basic_cuts () =
  let sep = '/'
  and j = "/"
  and data = ""
  in
  Alcotest.(check string __LOC__ data
              (String.concat j (Conex_utils.String.cuts sep data)));
  let data = "foo" in
  Alcotest.(check string __LOC__ data
              (String.concat j (Conex_utils.String.cuts sep data)));
  let data = "///" in
  Alcotest.(check string __LOC__ data
              (String.concat j (Conex_utils.String.cuts sep data)));
  let data = "foo/bar/baz" in
  Alcotest.(check string __LOC__ data
              (String.concat j (Conex_utils.String.cuts sep data)));
  let data = "foo//bar///baz" in
  Alcotest.(check string __LOC__ data
              (String.concat j (Conex_utils.String.cuts sep data)));
  let data = "/foo/bar/baz/" in
  Alcotest.(check string __LOC__ data
              (String.concat j (Conex_utils.String.cuts sep data)));
  let data = "/foo//bar//baz/" in
  Alcotest.(check string __LOC__ data
              (String.concat j (Conex_utils.String.cuts sep data)))

let tests = [
  "basic cuts is good", `Quick, basic_cuts ;
]
