open Conex_result
open Conex_core
open Conex_resource

module Ui = struct
  let ui =
    let module M = struct
      type t = Uint.t
      let pp ppf v = Format.pp_print_string ppf (Uint.to_string v)
      let equal a b = Uint.compare a b = 0
    end in
    (module M: Alcotest.TESTABLE with type t = M.t)

  let max = Uint.of_string "FFFFFFFFFFFFFFFF"
  let max_int64 = Uint.of_string "7FFFFFFFFFFFFFFF"
  let min_int64 = Uint.of_string "8000000000000000"

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
      let l = pred (String.length s) in
      trim0 (String.sub s 1 l)
    else
      s

  let to_of_str_id n () =
    for _i = 0 to n do
      let r = r () in
      Alcotest.(check string "to_of_string is identity"
                  (trim0 r) Uint.(to_string (of_string r)))
    done

  let compare_random n () =
    for _i = 0 to n do
      let r = Uint.of_string (r ()) in
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

let raw_sign p d = match Conex_nocrypto.sign p d with
  | Ok s -> s
  | Error e -> Alcotest.fail e

let base_v_err =
  let module M = struct
    type t = base_v_err
    let pp ppf x =
      let str = match x with
        | `InvalidBase64 -> "base64"
        | `InvalidPubKey -> "pubkey"
        | `InvalidSig -> "signature"
      in
      Format.pp_print_string ppf str
    let equal a b = match a, b with
      | `InvalidBase64, `InvalidBase64 -> true
      | `InvalidSig, `InvalidSig -> true
      | `InvalidPubKey, `InvalidPubKey -> true
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)


let sig_good () =
  let pid = "foobar" in
  let pub, p = gen_pub pid in
  let d = Signature.extend_data "bla" pid Uint.zero in
  let sigval = raw_sign p d in
  let public = match pub.Publickey.key with None -> Alcotest.fail "expected some key" | Some x -> x in
  Alcotest.check (result Alcotest.unit base_v_err)
    "signature is good" (Ok ())
    (Conex_nocrypto.verify public d sigval)

let sign_single () =
  let id = "a" in
  let pub, priv = gen_pub id in
  let raw = Conex_data.encode (Conex_data_persistency.publickey_to_t pub) in
  let sv = raw_sign priv raw in
  let pub = match pub.Publickey.key with None -> Alcotest.fail "expected some key" | Some x -> x
  in
  Alcotest.check (result Alcotest.unit base_v_err)
    "signature can be verified"
    (Ok ())
    (Conex_nocrypto.verify pub raw sv) ;
  Alcotest.check (result Alcotest.unit base_v_err)
    "signature empty"
    (Error `InvalidSig)
    (Conex_nocrypto.verify pub raw "") ;
  Alcotest.check (result Alcotest.unit base_v_err)
    "signature is bad (b64prefix)"
    (Error `InvalidBase64)
    (Conex_nocrypto.verify pub raw ("\000" ^ sv)) ;
  Alcotest.check (result Alcotest.unit base_v_err)
    "signature is bad (prefix)"
    (Error `InvalidSig)
    (Conex_nocrypto.verify pub raw ("abcd" ^ sv)) ;
  Alcotest.check (result Alcotest.unit base_v_err)
    "signature is bad (postfix)"
    (* should be invalid_b64 once nocrypto >0.5.3 is released *)
    (Error `InvalidSig)
    (Conex_nocrypto.verify pub raw (sv ^ "abcd")) ;
  Alcotest.check (result Alcotest.unit base_v_err)
    "signature is bad (raw)"
    (Error `InvalidSig)
    (Conex_nocrypto.verify pub "" sv) ;
  Alcotest.check (result Alcotest.unit base_v_err)
    "public key is bad (empty)"
    (Error `InvalidPubKey)
    (Conex_nocrypto.verify (`Pub "") raw sv) ;
  let pub = match pub with `Pub p -> p in
  Alcotest.check (result Alcotest.unit base_v_err)
    "public key is bad (prepended)"
    (Error `InvalidPubKey)
    (Conex_nocrypto.verify (`Pub ("\000" ^ pub)) raw sv) ;
  Alcotest.check (result Alcotest.unit base_v_err)
    "public key is bad (prepended)"
    (Error `InvalidPubKey)
    (Conex_nocrypto.verify (`Pub ("abcd" ^ pub)) raw sv) ;
  Alcotest.check (result Alcotest.unit base_v_err)
    "public key is bad (appended)"
    (Error `InvalidPubKey)
    (Conex_nocrypto.verify (`Pub (pub ^ "abcd")) raw sv) ;
  Alcotest.check (result Alcotest.unit base_v_err)
    "public key is bad (appended)"
    (Error `InvalidPubKey)
    (Conex_nocrypto.verify (`Pub (pub ^ "\000")) raw sv)

let bad_priv () =
  let id = "a" in
  let pub, priv = gen_pub id in
  let raw = Conex_data.encode (Conex_data_persistency.publickey_to_t pub) in
  Alcotest.check (result Alcotest.string Alcotest.string)
    "sign with broken key is broken"
    (Error "couldn't decode private key")
    (Conex_nocrypto.sign (`Priv "") raw) ;
  let get_pub p = match Conex_nocrypto.pub_of_priv (`Priv p) with
    | Ok (`Pub p) -> Ok p
    | Error e -> Error e
  in
  Alcotest.check (result Alcotest.string Alcotest.string)
    "pub_of_priv with broken key is broken"
    (Error "couldn't decode private key")
    (get_pub "") ;
  let mypriv = match priv with `Priv p -> p in
  let mypub = match pub.Publickey.key with Some (`Pub p) -> p | _ -> Alcotest.fail "expected key" in
  Alcotest.check (result Alcotest.string Alcotest.string)
    "pub_of_priv works fine"
    (Ok mypub)
    (get_pub mypriv)

let sign_tests = [
  "sign and verify is good", `Quick, sig_good ;
  "self-sign is good", `Quick, sign_single ;
  "bad priv is bad", `Quick, bad_priv ;
]

let check_ver = Alcotest.check (result Alcotest.string verr)

let idx_sign () =
  let k, p = gen_pub "a" in
  let idx = Index.index "a" in
  let signed = sign_idx idx p in
  let (sid, ts, sigval) = match signed.Index.signatures with
    | [x] -> x
    | _ -> assert false
  in
  check_ver "signed index verifies" (Ok "a")
    (Repository.verify k
       (Conex_data.encode (Conex_data_persistency.index_to_t signed))
       (sid, ts, sigval)) ;
  let r = Index.r (Index.next_id idx) "foo" (Uint.of_int 4) `PublicKey "2342" in
  let idx' = Index.add_resource signed r in
  check_ver "signed modified index does not verify"
    (Error (`InvalidSignature "a"))
    (Repository.verify k
       (Conex_data.encode (Conex_data_persistency.index_to_t idx'))
       (sid, ts, sigval))

let idx_sign_other () =
  (* this shows that Publickey.verify does not check
       index.identifier == fst signature
     cause it does not get the index *)
  let k, p = gen_pub "a" in
  let idx = Index.index "b" in
  let signed = sign_idx idx p in
  let signature = match signed.Index.signatures with
    | [x] -> x
    | _ -> assert false
  in
  check_ver "signed index verifies" (Ok "b")
    (Repository.verify k
       (Conex_data.encode (Conex_data_persistency.index_to_t signed))
       signature)

let idx_sign_bad () =
  let k, p = gen_pub "a" in
  let idx = Index.index "b" in
  let signed = sign_idx idx p in
  let (sid, ts, sigval) = match signed.Index.signatures with
    | [x] -> x
    | _ -> assert false
  in
  let idx' = Index.index "c" in
  let raw = Conex_data.encode (Conex_data_persistency.index_to_t idx') in
  check_ver "signed index does not verify (wrong id)"
    (Error (`InvalidSignature "b"))
    (Repository.verify k raw (sid, ts, sigval))

let idx_sign_bad2 () =
  let k, p = gen_pub "a" in
  let idx = Index.index "a" in
  let signed = sign_idx idx p in
  let (sid, ts, sigval) = match signed.Index.signatures with
    | [x] -> x
    | _ -> assert false
  in
  let idx' = Index.index ~counter:(Uint.of_int 23) "b" in
  let raw = Conex_data.encode (Conex_data_persistency.index_to_t idx') in
  check_ver "signed index does not verify (wrong data)"
    (Error (`InvalidSignature "a"))
    (Repository.verify k raw (sid, ts, sigval))

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
