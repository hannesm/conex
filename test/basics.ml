open Conex_core
open Conex_resource

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
  let d = Signature.extend_data "bla" pid 0L in
  let sigval = raw_sign p d in
  let public = match pub.Publickey.key with None -> Alcotest.fail "expected some key" | Some x -> x in
  Alcotest.check (result Alcotest.unit base_v_err)
    "signature is good" (Ok ())
    (Conex_nocrypto.verify public d sigval)

let sign_single () =
  let id = "a" in
  let pub, priv = gen_pub id in
  let raw = Data.publickey_to_string pub in
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
  let raw = Data.publickey_to_string pub in
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

let empty () =
  let store = Keystore.empty in
  Alcotest.(check int "Empty keystore is empty" 0 (Keystore.size store))

let single () =
  let s = Keystore.empty in
  let k1, _ = gen_pub "a" in
  let s' = Keystore.add s k1 in
  Alcotest.(check int "single: inserting doesn't affect original" 0 (Keystore.size s)) ;
  Alcotest.(check int "Inserting into keystore worked" 1 (Keystore.size s')) ;
  Alcotest.(check bool "Mem of keystore works" true (Keystore.mem s' "a")) ;
  Alcotest.(check bool "Mem of non-existing key works" false (Keystore.mem s' "b")) ;
  let k1' = Keystore.find s' "a" in
  Alcotest.(check bool "Insert and find works " true (k1 = k1')) ;
  let s'' = Keystore.remove s' "a" in
  Alcotest.(check int "single: remove works" 0 (Keystore.size s''))

let multiple () =
  let s = Keystore.empty in
  let k1, _ = gen_pub "a"
  and k2, _ = gen_pub "b"
  and k3, _ = gen_pub ~priv:(Conex_nocrypto.generate ()) "a"
  in
  let s' = Keystore.add s k1 in
  Alcotest.(check int "multiple: inserting doesn't affect original" 0 (Keystore.size s)) ;
  Alcotest.(check int "Inserting into keystore worked" 1 (Keystore.size s')) ;
  let s' = Keystore.add s' k2 in
  Alcotest.(check int "Multiple insertion worked" 2 (Keystore.size s')) ;
  let s' = Keystore.add s' k3 in
  Alcotest.(check int "Overwriting worked" 2 (Keystore.size s')) ;
  let k3' = Keystore.find s' "a" in
  Alcotest.(check bool "Overwriting worked (compare)" true (k3 = k3')) ;
  Alcotest.(check bool "Overwriting worked (compare, false)" false (k1 = k3')) ;
  let s'' = Keystore.remove s' "a" in
  Alcotest.(check int "Removing of a key worked" 1 (Keystore.size s'')) ;
  let s''' = Keystore.remove s' "b" in
  Alcotest.(check int "Removing of b key worked" 1 (Keystore.size s''')) ;
  let s'' = Keystore.remove s'' "b" in
  Alcotest.(check int "Removing of another key worked" 0 (Keystore.size s''))

let ks_tests = [
  "empty keystore", `Quick, empty ;
  "single keystore insert and remove", `Quick, single ;
  "multiple keys", `Quick, multiple ;
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
    (Repository.verify k (Data.index_to_string signed) (sid, ts, sigval)) ;
  let idx' = Index.add_resource signed ("foo", `PublicKey, "2342") in
  check_ver "signed modified index does not verify"
    (Error (`InvalidSignature "a"))
    (Repository.verify k (Data.index_to_string idx') (sid, ts, sigval))

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
    (Repository.verify k (Data.index_to_string signed) signature)

let idx_sign_bad () =
  let k, p = gen_pub "a" in
  let idx = Index.index "b" in
  let signed = sign_idx idx p in
  let (sid, ts, sigval) = match signed.Index.signatures with
    | [x] -> x
    | _ -> assert false
  in
  let idx' = Index.index "c" in
  let raw = Data.index_to_string idx' in
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
  let idx' = Index.index ~counter:23L "b" in
  let raw = Data.index_to_string idx' in
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
  ("Signature", sign_tests) ;
  ("Keystore", ks_tests) ;
  ("Index", idx_tests)
]
