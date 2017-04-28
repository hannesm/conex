open Conex_utils

(* this is the FRONT *)
module type S = sig
  open Conex_resource

  type t

  val ids : unit -> identifier list
  val read : identifier -> (t, string) result
  val bits : t -> int
  val created : t -> Uint.t
  val generate : ?bits:int -> Key.alg -> identifier -> Uint.t -> unit ->
    (t, string) result
  val pub_of_priv : t -> (Key.t, string) result
  val sign : Uint.t -> Author.t -> Signature.alg -> t ->
    (Author.t, string) result
end

module type FS = sig
  val ids : unit -> string list
  val read : string -> ((string * Conex_utils.Uint.t), string) result
  val write : string -> string -> (unit, string) result
end

module type S_RSA_BACK = sig
  type t

  val ids : unit -> string list
  val read : string -> (t, string) result
  val bits : t -> int
  val created : t -> Uint.t
  val generate_rsa : ?bits:int -> string -> Uint.t -> unit -> (t, string) result
  val pub_of_priv_rsa : t -> (string, string) result
  val sign_pss : t -> string -> (string, string) result
end

module Make (C : S_RSA_BACK) = struct
  open Conex_resource

  type t = C.t

  let ids = C.ids

  let read = C.read

  let bits = C.bits

  let created = C.created

  let generate ?bits alg id ts () =
    match alg with
    | `RSA -> C.generate_rsa ?bits id ts ()

  let pub_of_priv t =
    C.pub_of_priv_rsa t >>= fun pub ->
    Ok (`RSA, pub, created t)

  let sign now idx alg t =
    let idx, _overflow = Author.prep_sig idx in
    let data = Wire.to_string (Author.wire_raw idx)
    and id = idx.Author.name
    in
    match alg with
    | `RSA_PSS_SHA256 ->
      let hdr = alg, now in
      let data = Wire.to_string (Signature.wire id hdr data) in
      C.sign_pss t data >>= fun raw ->
      pub_of_priv t >>= fun key ->
      Ok (Author.replace_sig idx (key, (hdr, raw)))
end
