open Core
open Common

(* since Publickey.t is private, we've no way to properly test Publickey.publickey *)
let good_publickey () =
  let apriv = Private.generate () in
  let pk, _ = gen_pub ~counter:0L ~priv:apriv "a" in
  Alcotest.check
    (result publickey Alcotest.string)
    "good key is good"
    (Ok pk)
    (Publickey.publickey "a" (Some (Private.pub_of_priv apriv)))

let good_publickey_2 () =
  let apriv = Private.generate ~bits:4096 () in
  let pk, _ = gen_pub ~counter:100L ~priv:apriv "a" in
  Alcotest.check
    (result publickey Alcotest.string)
    "good key is good"
    (Ok pk)
    (Publickey.publickey ~counter:100L "a" (Some (Private.pub_of_priv apriv)))

let bad_publickey () =
  let apriv = Private.generate ~bits:1024 () in
  Alcotest.check
    (result publickey Alcotest.string)
    "small key is rejected"
    (Error "RSA key too small")
    (Publickey.publickey "a" (Some (Private.pub_of_priv apriv)))

let pubkey_enc_dec () =
  let apriv = Private.generate () in
  let apub = Private.pub_of_priv apriv in
  let enc = Publickey.encode_key apub in
  Alcotest.(check string "encoding and decoding works"
              enc
              (match Publickey.decode_key enc with
               | None -> invalid_arg "invalid data"
               | Some x -> Publickey.encode_key x))

let public_tests = [
  "good publickey", `Quick, good_publickey ;
  "good publickey explicit", `Slow, good_publickey_2 ;
  "bad publickey", `Quick, bad_publickey ;
  "encode/decode publickey", `Quick, pubkey_enc_dec ;
]

let check_ver = Alcotest.check (result Alcotest.string verr)

let sig_good () =
  let pid = "foobar" in
  let pub, p = gen_pub pid in
  let (id, sigval) = Private.sign pid p "bla" in
  Alcotest.(check string "id of sign is same as given" pid id) ;
  check_ver "signature is good" (Ok id) (Publickey.verify pub "bla" (id, sigval))

let check_sig prefix pub raw (id, sv) =
  check_ver (prefix ^ " signature can be verified") (Ok id)
    (Publickey.verify pub raw (id, sv)) ;
  check_ver (prefix ^ " signature of other id")
    (Error (`InvalidSignature ("foo", Signature.extend_data raw "foo")))
    (Publickey.verify pub raw ("foo", sv)) ;
  let sigdata = Signature.extend_data raw id in
  check_ver (prefix ^ " signature empty")
    (Error (`InvalidSignature (id, sigdata)))
    (Publickey.verify pub raw (id, "")) ;
  check_ver (prefix ^ " signature is bad (b64prefix)")
    (Error (`InvalidBase64Encoding (id, "")))
    (Publickey.verify pub raw (id, "\000" ^ sv)) ;
  check_ver (prefix ^ " signature is bad (prefix)")
    (Error (`InvalidSignature (id, sigdata)))
    (Publickey.verify pub raw (id, "abcd" ^ sv)) ;
  check_ver (prefix ^ " signature is bad (postfix)")
    (* should be invalid_b64 once nocrypto >0.5.3 is released *)
    (Error (`InvalidSignature (id, sigdata)))
    (Publickey.verify pub raw (id, sv ^ "abcd")) ;
  check_ver (prefix ^ " signature is bad (raw)")
    (Error (`InvalidSignature (id, Signature.extend_data "" id)))
    (Publickey.verify pub "" (id, sv))

let sign_single () =
  let id = "a" in
  let pub, priv = gen_pub id in
  let raw = Data.publickey_to_string pub in
  let s = Private.sign id priv raw in
  check_sig "common" pub raw s

let sig_good_role () =
  let pid = "foobar" in
  let pub, p = gen_pub pid in
  let id, sigval = Private.sign pid p "bla" in
  Alcotest.(check string "id of sign is same as given" pid id) ;
  check_ver "signature is good" (Ok id) (Publickey.verify pub "bla" (id, sigval))

let sig_bad_data () =
  let pid = "foobar" in
  let pub, p = gen_pub pid in
  let id, sigval = Private.sign pid p "bla" in
  Alcotest.(check string "id of sign is same as given" pid id) ;
  let raw = "blabb" in
  check_ver
    "verify fails (data)"
    (Error (`InvalidSignature (id, Signature.extend_data raw id)))
    (Publickey.verify pub raw (id, sigval))

let sign_tests = [
  "sign and verify is good", `Quick, sig_good ;
  "sign and verify is good", `Quick, sig_good_role ;
  "self-sign is good", `Quick, sign_single ;
  "bad signature (data)", `Quick, sig_bad_data ;
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
  and k3, _ = gen_pub ~priv:(Private.generate ()) "a"
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

let idx_sign () =
  let k, p = gen_pub "a" in
  let idx = Index.index "a" in
  let signed = Private.sign_index idx p in
  let signature = match signed.Index.signature with
    | None -> assert false
    | Some x -> x
  in
  check_ver "signed index verifies" (Ok "a")
    (Publickey.verify k (Data.index_to_string signed) signature)

let idx_sign_other () =
  (* this shows that Publickey.verify does not check
       index.identifier == fst signature
     cause it does not get the index *)
  let k, p = gen_pub "a" in
  let idx = Index.index "b" in
  let signed = Private.sign_index idx p in
  let signature = match signed.Index.signature with
    | None -> assert false
    | Some x -> x
  in
  check_ver "signed index verifies" (Ok "b")
    (Publickey.verify k (Data.index_to_string signed) signature)

let idx_sign_bad () =
  let k, p = gen_pub "a" in
  let idx = Index.index "b" in
  let signed = Private.sign_index idx p in
  let signature = match signed.Index.signature with
    | None -> assert false
    | Some x -> x
  in
  let idx' = Index.index "c" in
  let raw = Data.index_to_string idx' in
  check_ver "signed index does not verify (wrong id)"
    (Error (`InvalidSignature ("b", Signature.extend_data raw "b")))
    (Publickey.verify k raw signature)

let idx_sign_bad2 () =
  let k, p = gen_pub "a" in
  let idx = Index.index "a" in
  let signed = Private.sign_index idx p in
  let signature = match signed.Index.signature with
    | None -> assert false
    | Some x -> x
  in
  let idx' = Index.index ~counter:23L "b" in
  let raw = Data.index_to_string idx' in
  check_ver "signed index does not verify (wrong data)"
    (Error (`InvalidSignature ("a", Signature.extend_data raw "a")))
    (Publickey.verify k raw signature) ;
  check_sig "index" k (Data.index_to_string idx) signature

let idx_tests = [
  "good index", `Quick, idx_sign ;
  "good index other", `Quick, idx_sign_other ;
  "bad index", `Quick, idx_sign_bad ;
  "bad index 2", `Quick, idx_sign_bad2 ;
]

let tests = [
  ("Publickey", public_tests) ;
  ("Signature", sign_tests) ;
  ("Keystore", ks_tests) ;
  ("Index", idx_tests)
]
