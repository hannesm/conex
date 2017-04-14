open Conex_utils
open Conex_resource

module SIGN = Conex_nocrypto.NC_S

let sset =
  let module M = struct
    type t = S.t
    let pp ppf v = Format.fprintf ppf "%a" (pp_list pp_id) (S.elements v)
    let equal = S.equal
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let privkey = ref None

let gen_pub () =
  let priv = match !privkey with
    | Some p -> p
    | None ->
      let p = Conex_nocrypto.NC_S.generate ~bits:2048 Uint.zero () in
      privkey := Some p ;
      p
  in
  match Conex_nocrypto.NC_S.pub_of_priv priv with
  | Ok pub -> (pub, priv)
  | Error e -> Alcotest.fail e

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

let team_eq a b =
  let open Team in
  id_equal a.name b.name && a.counter = b.counter && S.equal a.members b.members

let team =
  let module M = struct
    type t = Team.t
    let pp = Team.pp
    let equal = team_eq
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let idx_eq a b =
  Author.equal a b && a.Author.counter = b.Author.counter

let ji =
  let module M = struct
    type t = Author.t
    let pp = Author.pp
    let equal = idx_eq
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let id =
  let module M = struct
    type t = [ `Author of Author.t | `Team of Team.t ]
    let pp ppf = function `Team t -> Team.pp ppf t | `Author idx -> Author.pp ppf idx
    let equal a b = match a, b with
      | `Team t, `Team t' -> team_eq t t'
      | `Author k, `Author k' -> idx_eq k k'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let auth =
  let module M = struct
    type t = Authorisation.t
    let pp = Authorisation.pp
    let equal a b =
      let open Authorisation in
      a.counter = b.counter &&
      name_equal a.name b.name &&
      S.equal a.authorised b.authorised
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let rels =
  let module M = struct
    type t = Releases.t
    let pp = Releases.pp
    let equal a b =
      let open Releases in
      a.counter = b.counter &&
      a.name = b.name &&
      S.equal a.versions b.versions
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let css =
  let module M = struct
    type t = Checksums.t
    let pp = Checksums.pp
    let equal a b = match Checksums.compare_t a b with Ok () -> true | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let verr =
  let module M = struct
    type t = Conex_crypto.verification_error
    let pp = Conex_crypto.pp_verification_error
    let equal a b = match a, b with
      | `InvalidBase64Encoding, `InvalidBase64Encoding
      | `InvalidSignature, `InvalidSignature
      | `InvalidPublicKey, `InvalidPublicKey -> true
      (* for OpenSSL where we don't have detailed error reporting *)
      | `InvalidSignature, `InvalidPublicKey
      | `InvalidPublicKey, `InvalidSignature -> true
      (* until nocrypto >0.5.3 is not released *)
      | `InvalidSignature, `InvalidBase64Encoding
      | `InvalidBase64Encoding, `InvalidSignature -> true
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let sign_idx idx p =
  let idx = List.fold_left Author.approve idx idx.Author.queued in
  match SIGN.sign Uint.zero idx p with
  | Ok idx -> idx
  | Error e -> Alcotest.fail e
