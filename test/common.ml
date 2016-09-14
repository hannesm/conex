open Core

let sset =
  let module M = struct
    type t = S.t
    let pp ppf v = Format.fprintf ppf "%a" (pp_list pp_id) (S.elements v)
    let equal = S.equal
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let privkey = ref None

let gen_pub ?counter ?role ?priv id =
  let priv = match priv, !privkey with
    | Some p, _ -> p
    | None, Some x -> x
    | None, None ->
      let p = Private.generate () in
      privkey := Some p ;
      p
  in
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
      name_equal a.name b.name &&
      S.equal a.authorised b.authorised
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
      S.equal a.releases b.releases
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let ji =
  let module M = struct
    type t = Index.t
    let pp = Index.pp_index
    let equal a b =
      let open Index in
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
    type t = Repository.error
    let pp = Repository.pp_error
    let equal a b = match a, b with
      | `InvalidName (w, h), `InvalidName (w', h') -> name_equal w w' && name_equal h h'
      | `InsufficientQuorum (id, q), `InsufficientQuorum (id', q') -> id_equal id id' && S.equal q q'
      | `InvalidResource (w, h), `InvalidResource (w', h') -> resource_equal w w' && resource_equal h h'
      | `MissingSignature id, `MissingSignature id' -> id_equal id id'
      | `NotSigned (n, r, js), `NotSigned (n', r', js') -> name_equal n n' && resource_equal r r' && S.equal js js'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let verr =
  let module M = struct
    type t = Core.verification_error
    let pp = Core.pp_verification_error
    let equal a b = match a, b with
      | `InvalidBase64Encoding (id, _), `InvalidBase64Encoding (id', _) -> id_equal id id'
      | `InvalidSignature (id, _, data), `InvalidSignature (id', _, data') -> id_equal id id' && String.compare data data' = 0
      | `InvalidPublicKey id, `InvalidPublicKey id' -> id_equal id id'
      | `InvalidIdentifier id, `InvalidIdentifier id' -> id_equal id id'
      | `NotAuthorised (a, s), `NotAuthorised (a', s') -> id_equal a a' && id_equal s s'
      | `NoSignature id, `NoSignature id' -> id_equal id id'
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)
