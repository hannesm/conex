open Core
open Common

let publickey =
  let module M = struct
    type t = Publickey.t
    let pp = Publickey.pp_publickey
    let equal = Publickey.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let gen_pub ?counter ?role ?(priv = Private.generate ()) id =
  match Publickey.publickey ?counter ?role id (Some (Private.pub_of_priv priv)) with
  | Ok public -> (public, priv)
  | Error s -> invalid_arg s

(* since Publickey.t is private, we've no way to properly test Publickey.publickey *)
let good_publickey () =
  let apriv = Private.generate () in
  let pk, _ = gen_pub ~counter:0L ~role:`Author ~priv:apriv "a" in
  Alcotest.check
    (result publickey Alcotest.string)
    "good key is good"
    (Ok pk)
    (Publickey.publickey "a" (Some (Private.pub_of_priv apriv)))

let good_publickey_2 () =
  let apriv = Private.generate ~bits:4096 () in
  let pk, _ = gen_pub ~counter:100L ~role:`Janitor ~priv:apriv "a" in
  Alcotest.check
    (result publickey Alcotest.string)
    "good key is good"
    (Ok pk)
    (Publickey.publickey ~counter:100L ~role:`Janitor "a" (Some (Private.pub_of_priv apriv)))

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

let err =
  let module M = struct
    type t = Core.error
    let pp = Core.pp_error
    let equal a b = match a, b with
      | `InvalidBase64Encoding _, `InvalidBase64Encoding _ -> true
      | `InvalidSignature _, `InvalidSignature _ -> true
      | `InvalidRole _, `InvalidRole _ -> true
      | `InvalidPublicKey _, `InvalidPublicKey _ -> true
      | `InvalidIdentifier _, `InvalidIdentifier _ -> true
      | `InvalidCounter _, `InvalidCounter _ -> true
      | `InsufficientQuorum _, `InsufficientQuorum _ -> true
      | `InvalidAuthorisation _, `InvalidAuthorisation _ -> true
      | `InvalidSignatures _, `InvalidSignatures _ -> true
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let check_ver = Alcotest.check (result Alcotest.string err)

let invalid_sig = Error (`InvalidSignature ("", `PublicKey, "", ""))
let invalid_b64 = Error (`InvalidBase64Encoding ("", ""))
let invalid_role = Error (`InvalidRole (`Author, `Author))

let sig_good () =
  let pid = "foobar" in
  let pub, p = gen_pub pid in
  let (id, sigval) = Private.sign pid p `PublicKey "bla" in
  Alcotest.(check string "id of sign is same as given" pid id) ;
  check_ver "signature is good" (Ok id) (Publickey.verify pub `Author `PublicKey "bla" (id, sigval))

let check_sig prefix pub role kind raw (id, sv) =
  check_ver (prefix ^ " signature can be verified") (Ok id)
    (Publickey.verify pub role kind raw (id, sv)) ;
  check_ver (prefix ^ " signature of other id") invalid_sig
    (Publickey.verify pub role kind raw ("foo", sv)) ;
  check_ver (prefix ^ " signature empty") invalid_sig
    (Publickey.verify pub role kind raw (id, "")) ;
  check_ver (prefix ^ " signature is bad (b64prefix)") invalid_b64
    (Publickey.verify pub role kind raw (id, "\000" ^ sv)) ;
  check_ver (prefix ^ " signature is bad (prefix)") invalid_sig
    (Publickey.verify pub role kind raw (id, "abcd" ^ sv)) ;
  check_ver (prefix ^ " signature is bad (postfix)") invalid_sig (* should be invalid_b64 once nocrypto is fixed *)
    (Publickey.verify pub role kind raw (id, sv ^ "abcd")) ;
  check_ver (prefix ^ " signature is bad (raw)") invalid_sig
    (Publickey.verify pub role kind "" (id, sv)) ;
  check_ver (prefix ^ " signature has bad role") invalid_role
    (Publickey.verify pub `Janitor kind raw (id, sv))

let sign_single () =
  let id = "a"
  and role = `Author
  in
  let pub, priv = gen_pub ~role id in
  let raw = Data.publickey_raw pub in
  let s = Private.sign id priv `PublicKey raw in
  check_sig "common" pub role `PublicKey raw s

let sig_good_role () =
  let pid = "foobar" in
  let pub, p = gen_pub ~role:`Janitor pid in
  let (id, sigval) = Private.sign pid p `PublicKey "bla" in
  Alcotest.(check string "id of sign is same as given" pid id) ;
  check_ver "signature is good" (Ok id) (Publickey.verify pub `Janitor `PublicKey "bla" (id, sigval))

let sig_bad_role () =
  let pid = "foobar" in
  let pub, p = gen_pub pid in
  let (id, sigval) = Private.sign pid p `PublicKey "bla" in
  Alcotest.(check string "id of sign is same as given" pid id) ;
  check_ver
    "verify fails (role)"
    invalid_role
    (Publickey.verify pub `Janitor `PublicKey "bla" (id, sigval))

let sig_bad_kind () =
  let pid = "foobar" in
  let pub, p = gen_pub pid in
  let (id, sigval) = Private.sign pid p `PublicKey "bla" in
  Alcotest.(check string "id of sign is same as given" pid id) ;
  check_ver
    "verify fails (kind)"
    invalid_sig
    (Publickey.verify pub `Author `Checksum "bla" (id, sigval))

let sig_bad_data () =
  let pid = "foobar" in
  let pub, p = gen_pub pid in
  let (id, sigval) = Private.sign pid p `PublicKey "bla" in
  Alcotest.(check string "id of sign is same as given" pid id) ;
  check_ver
    "verify fails (data)"
    invalid_sig
    (Publickey.verify pub `Author `Checksum "blabb" (id, sigval))

let sign_tests = [
  "sign and verify is good", `Quick, sig_good ;
  "sign and verify is good", `Quick, sig_good_role ;
  "self-sign is good", `Quick, sign_single ;
  "bad signature (role)", `Quick, sig_bad_role ;
  "bad signature (kind)", `Quick, sig_bad_kind ;
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
  and k3, _ = gen_pub "a"
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

let verify_all exp ks role pub =
  List.iter2 (fun s r ->
      check_ver "signature is valid" r
        (Keystore.verify ks role `PublicKey (Data.publickey_raw pub) s))
    pub.Publickey.signatures
    exp

let ks_sign_single () =
  let id = "a"
  and role = `Author
  in
  let pub, priv = gen_pub ~role id in
  let s =
    let raw = Data.publickey_raw pub in
    Private.sign id priv `PublicKey raw
  in
  let ks = Keystore.(add empty (Publickey.add_sig pub s)) in
  let ks_pub = Keystore.find ks id in
  Alcotest.(check int "signature size is 1" 1 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Ok "a"] ks role ks_pub ;
  let pub = Publickey.add_sig (Publickey.add_sig (Publickey.add_sig pub s) s) s in
  let ks = Keystore.(add empty pub) in
  let ks_pub = Keystore.find ks id in
  Alcotest.(check int "signature size is 3" 3 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Ok "a" ; Ok "a" ; Ok "a"] ks role ks_pub

let ks_sign_revoked () =
  let id = "a"
  and role = `Author
  in
  let pub, priv = gen_pub ~role id in
  let s =
    let raw = Data.publickey_raw pub in
    Private.sign id priv `PublicKey raw
  in
  let ks_pub = Publickey.add_sig pub s in
  let ks =
    match Publickey.publickey ~role ~signatures:[s] id None with
    | Error s -> invalid_arg s
    | Ok ks_pub -> Keystore.(add empty ks_pub)
  in
  let ks_pub' = Keystore.find ks id in
  Alcotest.(check int "signature size is 1" 1 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Error (`InvalidPublicKey "")] ks role ks_pub' ;
  check_ver "keystore key can be verified if pubkey provided" (Ok "a")
    (Publickey.verify ks_pub role `PublicKey (Data.publickey_raw ks_pub) s)

let ks_sign_multiple () =
  let ida = "a"
  and idb = "b"
  and role = `Author
  in
  let puba, priva = gen_pub ~role ida in
  let pubb, privb = gen_pub ~role idb in
  let ks =
    let raw = Data.publickey_raw puba in
    let sa = Private.sign ida priva `PublicKey raw in
    let sb = Private.sign idb privb `PublicKey raw in
    Keystore.(add empty (Publickey.add_sig (Publickey.add_sig puba sb) sa))
  in
  let ks_pub = Keystore.find ks ida in
  Alcotest.(check int "signature size of 'a' is 2" 2 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Ok "a" ; Error (`InvalidIdentifier "")] ks role ks_pub ;
  let ks =
    let sbb = Private.sign idb privb `PublicKey (Data.publickey_raw pubb) in
    Keystore.(add ks (Publickey.add_sig pubb sbb))
  in
  let ks_pub = Keystore.find ks ida in
  Alcotest.(check int "signature size of 'a' is still 2" 2 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Ok "a" ; Ok "b"] ks role ks_pub ;
  let ks_pub = Keystore.find ks idb in
  Alcotest.(check int "signature size of 'b' is 1" 1 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Ok "b"] ks role ks_pub

let ks_sign_tests = [
  "keystore sign", `Quick, ks_sign_single ;
  "keystore sign revoked", `Quick, ks_sign_revoked ;
  "keystore sign multiple", `Quick, ks_sign_multiple ;
]

let tests = [
  ("Publickey", public_tests) ;
  ("Signature", sign_tests) ;
  ("Keystore", ks_tests) ;
  ("KeystoreSign", ks_sign_tests)
]

