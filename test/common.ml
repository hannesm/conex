open Conex_utils
open Conex_resource

module FS = struct
  let data = Hashtbl.create 3

  let ids () = Hashtbl.fold (fun k _ acc -> k :: acc) data []
  let read id = match Hashtbl.find data id with
    | exception Not_found -> Error "not found"
    | v -> Ok (v, "")
  let write k v = Hashtbl.add data k v ; Ok ()
end

module PRIV = Conex_private.Make(Conex_nocrypto.C)(FS)

let sset =
  let module M = struct
    type t = S.t
    let pp = S.pp
    let equal = S.equal
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let privkey = ref None

let gen_pub () =
  let priv = match !privkey with
    | Some p -> p
    | None ->
      match PRIV.generate ~bits:2048 `RSA "foo" () with
      | Error e -> Alcotest.fail e
      | Ok p ->
        privkey := Some p ;
        p
  in
  let pub = PRIV.pub_of_priv priv in
  (pub, priv)

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

let str_err =
  let module M = struct
    type t = string
    let pp ppf x = Format.pp_print_string ppf x
    let equal _ _ = true
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let r_err =
  let module M = struct
    type t = err
    let pp = pp_err
    let equal a b = match a, b with
      | `Parse _, `Parse _ -> true
      | `Unknown_alg _, `Unknown_alg _ -> true
      | `Malformed, `Malformed -> true
      | _ -> false
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)
