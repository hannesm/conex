open Core

let gen_pub ?counter ?role ?(priv = Private.generate ()) id =
  match Publickey.publickey ?counter ?role id (Some (Private.pub_of_priv priv)) with
  | Ok public -> (public, priv)
  | Error s -> invalid_arg s

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

let publickey =
  let module M = struct
    type t = Publickey.t
    let pp = Publickey.pp_publickey
    let equal = Publickey.equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let auth =
  let module M = struct
    type t = Authorisation.t
    let pp = Authorisation.pp_authorisation
    let equal a b =
      let open Authorisation in
      a.counter = b.counter &&
      a.version = b.version &&
      a.name = b.name &&
      List.length a.authorised = List.length b.authorised &&
      List.for_all (fun n -> List.mem n b.authorised) a.authorised
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let releases =
  let module M = struct
    type t = Releases.t
    let pp = Releases.pp_releases
    let equal a b =
      let open Releases in
      a.counter = b.counter &&
      a.version = b.version &&
      a.name = b.name &&
      List.length a.releases = List.length b.releases &&
      List.for_all (fun n -> List.mem n b.releases) a.releases
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let ji =
  let module M = struct
    type t = Janitorindex.t
    let pp = Janitorindex.pp_janitorindex
    let equal a b =
      let open Janitorindex in
      a.counter = b.counter &&
      a.version = b.version &&
      a.identifier = b.identifier &&
      List.length a.resources = List.length b.resources &&
      List.for_all (fun (n, k, d) ->
          List.exists (fun (n', k', d') -> n = n' && k = k' && d = d')
            b.resources)
        a.resources
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let cs =
  let module M = struct
    type t = Checksum.t
    let pp = Checksum.pp_checksums
    let equal = Checksum.checksums_equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

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

let invalid_sig = Error (`InvalidSignature ("", `PublicKey, "", ""))
let invalid_b64 = Error (`InvalidBase64Encoding ("", ""))
let invalid_role = Error (`InvalidRole (`Author, `Author))
