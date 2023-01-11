module V = Conex_mirage_crypto.NC_V
module C = Conex.Make(Logs)(V)
module PRIV = Conex_private.Make(Conex_mirage_crypto.C)(Conex_unix_private_key)

let to_str pp = function
  | Ok x -> Ok x
  | Error e -> Error (Fmt.to_to_string pp e)

let now = Ptime_clock.now ()

let now_rfc3339 = Ptime.to_rfc3339 now

let to_ts f = match Ptime.of_float_s f with
  | None -> None
  | Some x -> Some (Ptime.to_rfc3339 x)

let init_priv_id id =
  let ( let* ) = Result.bind in
  let id' = match id with None -> "" | Some id -> id in
  let* priv = to_str PRIV.pp_r_err (PRIV.read to_ts id') in
  let id'' = PRIV.id priv in
  Ok (priv, id'')
