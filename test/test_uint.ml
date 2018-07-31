open Conex_utils

let ui =
  let module M = struct
    type t = Uint.t
    let pp ppf v = Format.pp_print_string ppf (Uint.to_string v)
    let equal a b = Uint.compare a b = 0
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let the x = match Uint.of_string x with Some x -> x | None -> Alcotest.fail "cannot parse"
let max = the "FFFFFFFFFFFFFFFF"
let max_int64 = the "7FFFFFFFFFFFFFFF"
let min_int64 = the "8000000000000000"

let compare_initial () =
  Alcotest.(check int "compare 0 0 is 0"
              0 Uint.(compare zero zero)) ;
  Alcotest.(check int "compare 0 max is -1"
              (-1) Uint.(compare zero max)) ;
  Alcotest.(check int "compare max 0 is 1"
              1 Uint.(compare max zero)) ;
  Alcotest.(check int "compare 0 max_int64 is -1"
              (-1) Uint.(compare zero max_int64)) ;
  Alcotest.(check int "compare max_int64 0 is 1"
              1 Uint.(compare max_int64 zero)) ;
  Alcotest.(check int "compare 0 min_int64 is -1"
              (-1) Uint.(compare zero min_int64)) ;
  Alcotest.(check int "compare min_int64 0 is 1"
              1 Uint.(compare min_int64 zero))

let succ_initial () =
  Alcotest.(check bool "succ max overflows"
              true (fst (Uint.succ max))) ;
  let one = snd Uint.(succ zero) in
  Alcotest.(check (pair bool ui) "succ 0 no overflow and one"
              (false, one) Uint.(succ zero)) ;
  Alcotest.(check (pair bool ui) "succ max_int64 no overflow and min_int64"
              (false, min_int64) Uint.(succ max_int64)) ;
  Alcotest.(check bool "succ min_int64 no overflow"
              false (fst Uint.(succ min_int64)))

let r () =
  let buf = Bytes.create 16 in
  let one i =
    let r = Random.int 16 in
    let ascii = r + (if r < 10 then 0x30 else 0x37) in
    Bytes.set buf i (char_of_int ascii)
  in
  for i = 0 to 15 do one i done ;
  Bytes.to_string buf

let rec trim0 s =
  if String.get s 0 = '0' then
    trim0 (String.slice ~start:1 s)
  else
    s

let to_of_str_id n () =
  for _i = 0 to n do
    let r = r () in
    Alcotest.(check string "to_of_string is identity"
                (trim0 r) Uint.(to_string (the r)))
  done

let compare_random n () =
  for _i = 0 to n do
    let r = the (r ()) in
    let r = if r = Uint.zero then snd (Uint.succ r) else r in
    Alcotest.(check int ("compare " ^ Uint.to_string r ^ " 0 is 1")
                1 Uint.(compare r zero)) ;
    Alcotest.(check int ("compare 0 " ^ Uint.to_string r ^ " is -1")
                (-1) Uint.(compare zero r)) ;
    Alcotest.(check int ("compare " ^ Uint.to_string r ^ " with itself is 0")
                0 Uint.(compare r r)) ;
    if r = max then begin
      Alcotest.(check int ("compare " ^ Uint.to_string r ^ " max is 0")
                  0 Uint.(compare r max)) ;
      Alcotest.(check int ("compare max " ^ Uint.to_string r ^ " is 0")
                  0 Uint.(compare max r))
    end else begin
      Alcotest.(check int ("compare " ^ Uint.to_string r ^ " max is -1")
                  (-1) Uint.(compare r max)) ;
      Alcotest.(check int ("compare max " ^ Uint.to_string r ^ " is 1")
                  1 Uint.(compare max r)) ;
    end
  done

let tests = [
  "basic compare is good", `Quick, compare_initial ;
  "succ is good", `Quick, succ_initial ;
  "to/of_string is identity", `Quick, to_of_str_id 10000 ;
  "compare r zero is good", `Quick, compare_random 1000 ;
]
