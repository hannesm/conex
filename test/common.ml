open Core

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
