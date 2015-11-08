open Core

let empty () =
  let store = Keystore.empty in
  Alcotest.(check int "Empty keystore is empty" 0 (Keystore.size store))

let gen_pub ?counter ?role ?(priv = Private.generate ()) id =
  let public = Publickey.publickey ?counter ?role id (Some (Private.pub_of_priv priv)) in
  (public, priv)

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

let result (type a) (type e) a e =
  let (module A: Alcotest.TESTABLE with type t = a) = a in
  let (module E: Alcotest.TESTABLE with type t = e) = e in
  let module M = struct
    type t = (a, e) result
    let pp fmt t = match t with
      | Ok    t -> Format.fprintf fmt "Ok @[(%a)@]" A.pp t
      | Error e -> Format.fprintf fmt "Error @[(%a)@]" E.pp e
    let equal x y = match x, y with
      | Ok    x, Ok    y -> A.equal x y
      | Error x, Error y -> E.equal x y
      | _      , _       -> false
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let err =
  let module M = struct
    type t = error
    let pp = pp_error
    let equal a b = match a, b with
      | `InvalidBase64Encoding _, `InvalidBase64Encoding _ -> true
      | `InvalidSignature _, `InvalidSignature _ -> true
      | `InvalidRole _, `InvalidRole _ -> true
      | `InvalidPublicKey _, `InvalidPublicKey _ -> true
      | `InvalidIdentifier _, `InvalidIdentifier _ -> true
      | `InvalidCounter _, `InvalidCounter _ -> true
      | `InsufficientQuorum _, `InsufficientQuorum _ -> true
      | `InvalidDelegate _, `InvalidDelegate _ -> true
      | `InvalidSignatures _, `InvalidSignatures _ -> true
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let check_ver = Alcotest.check (result Alcotest.string err)

let invalid_sig = Error (`InvalidSignature ("", `RSA_PSS, "", ""))
let invalid_b64 = Error (`InvalidBase64Encoding ("", ""))
let invalid_role = Error (`InvalidRole (`Developer, `Developer))

let check_sig prefix pub role raw (id, alg, sv) =
  check_ver (prefix ^ " signature can be verified") (Ok id)
    (Publickey.verify pub role raw (id, alg, sv)) ;
  check_ver (prefix ^ " signature of other id") invalid_sig
    (Publickey.verify pub role raw ("foo", alg, sv)) ;
  check_ver (prefix ^ " signature empty") invalid_sig
    (Publickey.verify pub role raw (id, alg, "")) ;
  check_ver (prefix ^ " signature is bad (b64prefix)") invalid_b64
    (Publickey.verify pub role raw (id, alg, "\000" ^ sv)) ;
  check_ver (prefix ^ " signature is bad (prefix)") invalid_sig
    (Publickey.verify pub role raw (id, alg, "abcd" ^ sv)) ;
  check_ver (prefix ^ " signature is bad (postfix)") invalid_sig
    (Publickey.verify pub role raw (id, alg, sv ^ "abcd")) ;
  check_ver (prefix ^ " signature is bad (raw)") invalid_sig
    (Publickey.verify pub role "" (id, alg, sv)) ;
  check_ver (prefix ^ " signature has bad role") invalid_role
    (Publickey.verify pub `RepositoryMaintainer raw (id, alg, sv))

let sign_single () =
  let id = "a"
  and role = `Developer
  in
  let pub, priv = gen_pub ~role id in
  let raw = Data.publickey_raw pub in
  let s = Private.sign id priv raw in
  check_sig "common" pub role raw s

let sign_pss () =
  let id = "a"
  and role = `Developer
  and alg = `RSA_PSS
  in
  let pub, priv = gen_pub ~role id in
  let raw = Data.publickey_raw pub in
  let pss = Private.sign ~algorithm:alg id priv raw in
  check_sig "PSS" pub role raw pss

let sign_pkcs () =
  let id = "a"
  and role = `Developer
  and alg = `RSA_PKCS
  in
  let pub, priv = gen_pub ~role id in
  let raw = Data.publickey_raw pub in
  let pkcs = Private.sign ~algorithm:alg id priv raw in
  check_sig "PKCS" pub role raw pkcs

let sign_tests = [
  "self-sign", `Quick, sign_single ;
  "pss-sign", `Quick, sign_pss ;
  "pkcs-sign", `Quick, sign_pkcs ;
]

let verify_all exp ks role pub =
  List.iter2 (fun s r ->
      check_ver "signature is valid" r
        (Keystore.verify ks role (Data.publickey_raw pub) s))
    pub.Publickey.signatures
    exp

let ks_sign_single () =
  let id = "a"
  and role = `Developer
  in
  let pub, priv = gen_pub ~role id in
  let s =
    let raw = Data.publickey_raw pub in
    Private.sign id priv raw
  in
  let ks = Keystore.(add empty { pub with Publickey.signatures = [ s ] }) in
  let ks_pub = Keystore.find ks id in
  Alcotest.(check int "signature size is 1" 1 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Ok "a"] ks role ks_pub ;
  let ks = Keystore.(add empty { pub with Publickey.signatures = [ s ; s ; s ] }) in
  let ks_pub = Keystore.find ks id in
  Alcotest.(check int "signature size is 3" 3 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Ok "a" ; Ok "a" ; Ok "a"] ks role ks_pub

let ks_sign_revoked () =
  let id = "a"
  and role = `Developer
  in
  let pub, priv = gen_pub ~role id in
  let s =
    let raw = Data.publickey_raw pub in
    Private.sign id priv raw
  in
  let ks_pub = { pub with Publickey.signatures = [ s ] } in
  let ks = Keystore.(add empty { ks_pub with Publickey.key = None }) in
  let ks_pub' = Keystore.find ks id in
  Alcotest.(check int "signature size is 1" 1 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Error (`InvalidPublicKey "")] ks role ks_pub' ;
  check_ver "keystore key can be verified if pubkey provided" (Ok "a")
    (Publickey.verify ks_pub role (Data.publickey_raw ks_pub) s)

let ks_sign_multiple () =
  let ida = "a"
  and idb = "b"
  and role = `Developer
  in
  let puba, priva = gen_pub ~role ida in
  let pubb, privb = gen_pub ~role idb in
  let ks =
    let raw = Data.publickey_raw puba in
    let sa = Private.sign ida priva raw in
    let sb = Private.sign idb privb raw in
    Keystore.(add empty { puba with Publickey.signatures = [ sa ; sb ] })
  in
  let ks_pub = Keystore.find ks ida in
  Alcotest.(check int "signature size of 'a' is 2" 2 (List.length ks_pub.Publickey.signatures)) ;
  verify_all [Ok "a" ; Error (`InvalidIdentifier "")] ks role ks_pub ;
  let ks =
    let sbb = Private.sign idb privb (Data.publickey_raw pubb) in
    Keystore.(add ks { pubb with Publickey.signatures = [ sbb ] })
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

let () =
  Nocrypto_entropy_unix.initialize () ;
  Alcotest.run "Conex tests" [
    ("Keystore", ks_tests) ;
    ("Signature", sign_tests) ;
    ("KeystoreSign", ks_sign_tests)
  ]

