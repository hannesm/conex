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
module CS = Conex_nocrypto.NC_S

module BasicTests (V : Conex_crypto.VERIFY) (R : Conex_crypto.VERIFY_BACK) = struct

let raw_sign p d = match Conex_nocrypto.C.sign_rsa_pss ~key:p d with
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
    (R.verify_rsa_pss ~key:pu ~data:d ~signature)

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
    (R.verify_rsa_pss ~key ~data:raw ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "signature empty"
    (Error `InvalidSignature)
    (R.verify_rsa_pss ~key ~data:raw ~signature:"") ;
  Alcotest.check (result Alcotest.unit verr)
    "signature is bad (b64prefix)"
    (Error `InvalidBase64Encoding)
    (R.verify_rsa_pss ~key ~data:raw ~signature:("\000" ^ sv)) ;
  Alcotest.check (result Alcotest.unit verr)
    "signature is bad (prefix)"
    (Error `InvalidSignature)
    (R.verify_rsa_pss ~key ~data:raw ~signature:("abcd" ^ sv)) ;
  Alcotest.check (result Alcotest.unit verr)
    "signature is bad (postfix)"
    (Error `InvalidBase64Encoding)
    (R.verify_rsa_pss ~key ~data:raw ~signature:(sv ^ "abcd")) ;
  Alcotest.check (result Alcotest.unit verr)
    "signature is bad (raw)"
    (Error `InvalidSignature)
    (R.verify_rsa_pss ~key ~data:"" ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "public key is bad (empty)"
    (Error `InvalidPublicKey)
    (R.verify_rsa_pss ~key:"" ~data:raw ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "public key is bad (prepended)"
    (Error `InvalidPublicKey)
    (R.verify_rsa_pss ~key:("\000" ^ key) ~data:raw ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "public key is bad (prepended)"
    (Error `InvalidPublicKey)
    (R.verify_rsa_pss ~key:("abcd" ^ key) ~data:raw ~signature:sv)
(* TODO: re-enable this test... openssl doesn't care about extra data, should we? *)
(* Alcotest.check (result Alcotest.unit verr)
    "public key is bad (appended)"
    (Error `InvalidPublicKey)
    (R.verify_rsa_pss ~key:(key ^ "abcd") ~data:raw ~signature:sv) ;
  Alcotest.check (result Alcotest.unit verr)
    "public key is bad (appended)"
    (Error `InvalidPublicKey)
      (R.verify_rsa_pss ~key:(key ^ "\000") ~data:raw ~signature:sv) *)

let bad_priv () =
  let idx = Author.t Uint.zero "a" in
  let pub, priv = gen_pub () in
  let raw = Wire.to_string (Author.wire idx) in
  Alcotest.check (result Alcotest.string Alcotest.string)
    "sign with broken key is broken"
    (Error "couldn't decode private key")
    (Conex_nocrypto.C.sign_rsa_pss ~key:"" raw) ;
  let get_pub p = Conex_nocrypto.C.pub_of_priv_rsa p in
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
  let single_sig = match signed.Author.keys with
    | [(_,x)] -> x
    | _ -> Alcotest.fail "expected a single signature"
  in
  Alcotest.check (result Alcotest.unit verr) "signed index verifies" (Ok ())
    (V.verify signed) ;
  let (hdr, v) = single_sig in
  let v' = Bytes.(of_string v) in
  Bytes.set v' 3 'f' ;
  Bytes.set v' 2 'f' ;
  let idx = Author.replace_sig signed (k, (hdr, (Bytes.to_string v'))) in
  Alcotest.check (result Alcotest.unit verr) "bad signature does not verify"
    (Error `InvalidSignature)
    (V.verify idx) ;
  let pub = match CS.(pub_of_priv (generate ~bits:20 Uint.zero ())) with
    | Ok p -> p
    | Error _ -> Alcotest.fail "couldn't pub_of_priv"
  in
  let idx = Author.t ~keys:[pub, single_sig] Uint.zero "a" in
  Alcotest.check (result Alcotest.unit verr) "too small key"
    (Error `InvalidPublicKey)
    (V.verify idx) ;
  let idx = Author.replace_sig signed (k, (hdr, Wire.to_string (Author.wire_raw signed))) in
  Alcotest.check (result Alcotest.unit verr) "invalid b64 sig"
    (Error `InvalidBase64Encoding)
    (V.verify idx)


let sign_tests = [
  "sign and verify is good", `Quick, sig_good ;
  "self-sign is good", `Quick, sign_single ;
  "bad priv is bad", `Quick, bad_priv ;
  "verify failures", `Quick, verify_fail ;
]

let idx_sign () =
  let _, p = gen_pub () in
  let idx = Author.t Uint.zero "a" in
  let signed = sign_idx idx p in
  Alcotest.check (result Alcotest.unit verr) "signed index verifies"
    (Ok ()) (V.verify signed) ;
  let r = Author.r (Author.next_id idx) "foo" `Key (`SHA256, "2342") in
  let idx' = Author.queue signed r in
  Alcotest.check (result Alcotest.unit verr) "signed modified index does verify (no commit)"
    (Ok ())
    (V.verify idx') ;
  let idx', _ = Author.prep_sig idx' in
  Alcotest.check (result Alcotest.unit verr) "signed modified index does verify (no commit)"
    (Error `InvalidSignature)
    (V.verify idx')

let idx_sign_bad () =
  let _, p = gen_pub () in
  let idx = Author.t Uint.zero "b" in
  let signed = sign_idx idx p in
  let idx' = Author.t ~keys:signed.Author.keys Uint.zero "c" in
  Alcotest.check (result Alcotest.unit verr) "signed index does not verify (wrong id)"
    (Error `InvalidSignature) (V.verify idx')

let idx_sign_bad2 () =
  let _, p = gen_pub () in
  let idx = Author.t Uint.zero "a" in
  let signed = sign_idx idx p in
  let idx' = Author.t ~keys:signed.Author.keys ~counter:(Uint.of_int_exn 23) Uint.zero "a" in
  Alcotest.check (result Alcotest.unit verr) "signed index does not verify (wrong data)"
    (Error `InvalidSignature) (V.verify idx')

let idx_tests = [
  "good index", `Quick, idx_sign ;
  "bad index", `Quick, idx_sign_bad ;
  "bad index 2", `Quick, idx_sign_bad2 ;
]

let tests prefix =
  [ (prefix ^ "Signature", sign_tests) ; (prefix ^ "Index", idx_tests) ]
end

module NC = BasicTests (Conex_nocrypto.NC_V) (Conex_nocrypto.V)

module OC = BasicTests (Conex_openssl.O_V) (Conex_openssl.V)

let tests = ("Uint", Ui.tests) :: NC.tests "Nocrypto"
