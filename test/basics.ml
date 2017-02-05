open Conex_result
open Conex_utils
open Conex_resource

module Ui = struct
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
end

open Common

let raw_sign p d = match Conex_nocrypto.sign_rsa_pss ~key:p d with
  | Ok s -> s
  | Error e -> Alcotest.fail e

let sig_good () =
  let pid = "foobar" in
  let pub, p = gen_pub () in
  let priv = match p with `Priv (`RSA, k, _) -> k in
  let pu = match pub with `RSA, k, _ -> k in
  let d = Wire.to_string (Signature.wire pid (`RSA_PSS_SHA256, Uint.zero) pid) in
  let signature = raw_sign priv d in
  Alcotest.check (result Alcotest.unit verr)
    "signature is good" (Ok ())
    (Conex_nocrypto.verify_rsa_pss ~key:pu ~data:d ~signature)

let sign_single () =
  let idx = Author.t Uint.zero "a" in
  let pub, priv = gen_pub () in
  let pri = match priv with `Priv (`RSA, k, _) -> k in
  let key = match pub with `RSA, k, _ -> k in
  let raw = Wire.to_string (Author.wire idx) in
  let sv = raw_sign pri raw in
  Alcotest.check (result Alcotest.unit verr)
    "signature can be verified"
    (Ok ())
    (Conex_nocrypto.verify_rsa_pss ~key ~data:raw ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "signature empty"
    (Error `InvalidSignature)
    (Conex_nocrypto.verify_rsa_pss ~key ~data:raw ~signature:"") ;
  Alcotest.check (result Alcotest.unit verr)
    "signature is bad (b64prefix)"
    (Error `InvalidBase64Encoding)
    (Conex_nocrypto.verify_rsa_pss ~key ~data:raw ~signature:("\000" ^ sv)) ;
  Alcotest.check (result Alcotest.unit verr)
    "signature is bad (prefix)"
    (Error `InvalidSignature)
    (Conex_nocrypto.verify_rsa_pss ~key ~data:raw ~signature:("abcd" ^ sv)) ;
  Alcotest.check (result Alcotest.unit verr)
    "signature is bad (postfix)"
    (* should be invalid_b64 once nocrypto >0.5.3 is released *)
    (Error `InvalidSignature)
    (Conex_nocrypto.verify_rsa_pss ~key ~data:raw ~signature:(sv ^ "abcd")) ;
  Alcotest.check (result Alcotest.unit verr)
    "signature is bad (raw)"
    (Error `InvalidSignature)
    (Conex_nocrypto.verify_rsa_pss ~key ~data:"" ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "public key is bad (empty)"
    (Error `InvalidPublicKey)
    (Conex_nocrypto.verify_rsa_pss ~key:"" ~data:raw ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "public key is bad (prepended)"
    (Error `InvalidPublicKey)
    (Conex_nocrypto.verify_rsa_pss ~key:("\000" ^ key) ~data:raw ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "public key is bad (prepended)"
    (Error `InvalidPublicKey)
    (Conex_nocrypto.verify_rsa_pss ~key:("abcd" ^ key) ~data:raw ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "public key is bad (appended)"
    (Error `InvalidPublicKey)
    (Conex_nocrypto.verify_rsa_pss ~key:(key ^ "abcd") ~data:raw ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "public key is bad (appended)"
    (Error `InvalidPublicKey)
    (Conex_nocrypto.verify_rsa_pss ~key:(key ^ "\000") ~data:raw ~signature:sv)

let bad_priv () =
  let idx = Author.t Uint.zero "a" in
  let pub, priv = gen_pub () in
  let raw = Wire.to_string (Author.wire idx) in
  Alcotest.check (result Alcotest.string Alcotest.string)
    "sign with broken key is broken"
    (Error "couldn't decode private key")
    (Conex_nocrypto.sign_rsa_pss ~key:"" raw) ;
  let get_pub p = Conex_nocrypto.pub_of_priv_rsa p in
  Alcotest.check (result Alcotest.string Alcotest.string)
    "pub_of_priv with broken key is broken"
    (Error "couldn't decode private key")
    (get_pub "") ;
  let mypriv = match priv with `Priv (`RSA, p, _) -> p
  and _, mypub, _ = pub
  in
  Alcotest.check (result Alcotest.string Alcotest.string)
    "pub_of_priv works fine"
    (Ok mypub)
    (get_pub mypriv)

let verify_fail () =
  let idx = Author.t Uint.zero "a" in
  let k, p = gen_pub () in
  let signed = sign_idx idx p in
  let single_sig = match signed.Author.signatures with
    | [x] -> x
    | _ -> Alcotest.fail "expected a single signature"
  in
  let raw = Wire.to_string (Author.wire_raw signed) in
  Alcotest.check (result Alcotest.unit verr) "signed index verifies" (Ok ())
    (Conex_repository.verify "a" k raw single_sig) ;
  let (hdr, _) = single_sig in
  Alcotest.check (result Alcotest.unit verr) "bad signature does not verify"
    (Error `InvalidSignature)
    (Conex_repository.verify "foo" k raw single_sig) ;
  let pub = match Conex_sign.(pub_of_priv (generate ~bits:20 Uint.zero ())) with
    | Ok p -> p
    | Error _ -> Alcotest.fail "couldn't pub_of_priv"
  in
  Alcotest.check (result Alcotest.unit verr) "too small key"
    (Error `InvalidPublicKey)
    (Conex_repository.verify "a" pub raw single_sig) ;
  Alcotest.check (result Alcotest.unit verr) "invalid b64 sig"
    (Error `InvalidBase64Encoding)
    (Conex_repository.verify "a" k
       (Wire.to_string (Author.wire_raw signed))
       (hdr, "bad"))


let sign_tests = [
  "sign and verify is good", `Quick, sig_good ;
  "self-sign is good", `Quick, sign_single ;
  "bad priv is bad", `Quick, bad_priv ;
  "verify failures", `Quick, verify_fail ;
]

let idx_sign () =
  let k, p = gen_pub () in
  let idx = Author.t Uint.zero "a" in
  let signed = sign_idx idx p in
  let single_sig = match signed.Author.signatures with
    | [x] -> x
    | _ -> Alcotest.fail "expected a single signature"
  in
  Alcotest.check (result Alcotest.unit verr) "signed index verifies"
    (Ok ())
    (Conex_repository.verify "a" k
       (Wire.to_string (Author.wire_raw signed))
       single_sig) ;
  let r = Author.r (Author.next_id idx) "foo" (Uint.of_int_exn 4) `Key (`SHA256, "2342") in
  let idx' = Author.add_resource signed r in
  Alcotest.check (result Alcotest.unit verr) "signed modified index does verify (no commit)"
    (Ok ())
    (Conex_repository.verify "a" k
       (Wire.to_string (Author.wire_raw idx')) single_sig) ;
  let idx', _ = Author.prep_sig idx' in
  Alcotest.check (result Alcotest.unit verr) "signed modified index does verify (no commit)"
    (Error `InvalidSignature)
    (Conex_repository.verify "a" k
       (Wire.to_string (Author.wire_raw idx')) single_sig)

let idx_sign_other () =
  (* this shows that Publickey.verify does not check
       index.identifier == fst signature
     cause it does not get the index *)
  let k, p = gen_pub () in
  let idx = Author.t Uint.zero "b" in
  let signed = sign_idx idx p in
  let signature = match signed.Author.signatures with
    | [x] -> x
    | _ -> Alcotest.fail "expected a single signature"
  in
  Alcotest.check (result Alcotest.unit verr) "signed index verifies"
    (Ok ())
    (Conex_repository.verify "b" k
       (Wire.to_string (Author.wire_raw signed))
       signature)

let idx_sign_bad () =
  let k, p = gen_pub () in
  let idx = Author.t Uint.zero "b" in
  let signed = sign_idx idx p in
  let single_sig = match signed.Author.signatures with
    | [x] -> x
    | _ -> Alcotest.fail "expected a single signature"
  in
  let idx' = Author.t Uint.zero "c" in
  let raw = Wire.to_string (Author.wire_raw idx') in
  Alcotest.check (result Alcotest.unit verr) "signed index does not verify (wrong id)"
    (Error `InvalidSignature)
    (Conex_repository.verify "c" k raw single_sig)

let idx_sign_bad2 () =
  let k, p = gen_pub () in
  let idx = Author.t Uint.zero "a" in
  let signed = sign_idx idx p in
  let single_sig = match signed.Author.signatures with
    | [x] -> x
    | _ -> Alcotest.fail "expected a single signature"
  in
  let idx' = Author.t ~counter:(Uint.of_int_exn 23) Uint.zero "b" in
  let raw = Wire.to_string (Author.wire_raw idx') in
  Alcotest.check (result Alcotest.unit verr) "signed index does not verify (wrong data)"
    (Error `InvalidSignature)
    (Conex_repository.verify "b" k raw single_sig)

let idx_tests = [
  "good index", `Quick, idx_sign ;
  "good index other", `Quick, idx_sign_other ;
  "bad index", `Quick, idx_sign_bad ;
  "bad index 2", `Quick, idx_sign_bad2 ;
]

let tests = [
  ("Uint", Ui.tests) ;
  ("Signature", sign_tests) ;
  ("Index", idx_tests)
]
