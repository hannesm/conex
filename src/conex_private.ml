open Conex_utils

(* this is the FRONT *)
module type S = sig
  open Conex_resource

  type t

  val ids : unit -> identifier list
  type r_err = [ `Decode of string | `Read of string | `None | `Multiple of string list ]
  val pp_r_err : r_err fmt
  val read : (float -> Conex_resource.timestamp option) -> identifier -> (t, r_err) result
  val bits : t -> int
  val created : t -> timestamp
  val id : t -> string
  val generate : ?bits:int -> (float -> Conex_resource.timestamp option) -> Key.alg -> identifier -> unit -> (t, string) result
  val pub_of_priv : t -> Key.t
  val sign : Wire.t -> timestamp -> identifier -> Signature.alg -> t ->
    (Signature.t, string) result
end

module type FS = sig
  val ids : unit -> Conex_resource.identifier list
  val read : (float -> Conex_resource.timestamp option) -> Conex_resource.identifier -> ((string * Conex_resource.timestamp), string) result
  val write : Conex_resource.identifier -> string -> (unit, string) result
end

module type S_RSA_BACK = sig
  type t

  val decode_priv : string -> Conex_resource.timestamp -> string -> (t, string) result
  val bits : t -> int
  val created : t -> Conex_resource.timestamp
  val id : t -> Conex_resource.identifier
  val generate_rsa : ?bits:int -> unit -> string * string
  val pub_of_priv_rsa : t -> string
  val sign_pss : t -> string -> (string, string) result
  val sha256 : string -> string
end

module Make (C : S_RSA_BACK) (F : FS) = struct
  open Conex_resource

  type t = C.t

  type r_err = [ `Decode of string | `Read of string | `None | `Multiple of string list ]

  let pp_r_err ppf = function
    | `Decode str -> Format.fprintf ppf "decode failure: %s" str
    | `Read str -> Format.fprintf ppf "read failure: %s" str
    | `None -> Format.pp_print_string ppf "id does not exist"
    | `Multiple ids -> Format.fprintf ppf "found multiple matching ids %a"
                         (pp_list Format.pp_print_string) ids

  let ids = F.ids

  let get_id id = match String.cut '.' id with | None -> id | Some (a, _) -> a

  let read to_ts id =
    let decode_e = function Ok t -> Ok t | Error e -> Error (`Decode e) in
    match F.read to_ts id with
    | Ok (k, ts) -> decode_e (C.decode_priv (get_id id) ts k)
    | Error _ ->
      (* treat id as prefix, look whether we've something *)
      match List.filter (fun fn -> String.is_prefix ~prefix:id fn) (F.ids ()) with
      | [ id' ] ->
        begin match F.read to_ts id' with
          | Error e -> Error (`Read e)
          | Ok (k, ts) -> decode_e (C.decode_priv (get_id id') ts k)
        end
      | [] -> Error `None
      | ids -> Error (`Multiple ids)

  let bits = C.bits

  let created = C.created

  let id = C.id

  let generate ?bits to_ts alg id () =
    match alg with
    | `RSA ->
      let key, pub = C.generate_rsa ?bits () in
      let filename =
        let pub' = (id, "", `RSA, pub) in
        let keyid = Key.keyid (fun s -> `SHA256, C.sha256 s) pub' in
        get_id id ^ "." ^ Digest.to_string keyid
      in
      F.write filename key >>= fun () ->
      F.read to_ts filename >>= fun (_, ts) ->
      C.decode_priv id ts key

  let pub_of_priv t =
    let pub = C.pub_of_priv_rsa t in
    (id t, created t, `RSA, pub)

  (* TODO allows data to be empty, is this good? *)
  let sign data now id alg t =
    match alg with
    | `RSA_PSS_SHA256 ->
      let data = Wire.to_string (to_be_signed data now id alg) in
      C.sign_pss t data >>= fun raw ->
      Ok (id, now, alg, raw)
end
