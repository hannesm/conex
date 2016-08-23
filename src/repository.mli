open Core

type t

val repository : ?store:Keystore.t -> ?quorum:int -> Provider.t -> t
val provider : t -> Provider.t
val quorum : t -> int

val change_provider : t -> Provider.t -> t

type ok = [ `Identifier of identifier | `Quorum of identifier list | `Both of identifier * identifier list ]
val pp_ok : Format.formatter -> ok -> unit

type res = (ok, error) result
val pp_res : Format.formatter -> res -> unit

val verify_key : t -> Publickey.t -> res
val verify_authorisation : t -> ?authorised:(identifier list) -> Authorisation.t -> res
val verify_checksum : t -> Authorisation.t -> Checksum.t -> res
val verify_releases : t -> Authorisation.t -> Releases.t -> res
val verify_index : t -> Index.t -> res

(* TODO: are these return types good? *)
val load_keys : ?verify:bool -> t -> identifier list -> t
val load_janitor : ?verify:bool -> t -> identifier -> t
val load_janitors : ?verify:bool -> t -> t

val add_trusted_key : t -> Publickey.t -> t

val all_keyids : t -> identifier list
val all_janitors : t -> identifier list
val all_authorisations : t -> string list

type r_err = [ `NotFound of string | `NameMismatch of string * string ]
type 'a r_res = ('a, r_err) result

val pp_r_err : Format.formatter -> r_err -> unit

val read_key : t -> identifier -> Publickey.t r_res
val write_key : t -> Publickey.t -> unit

val read_index : t -> identifier -> Index.t r_res
val write_index : t -> Index.t -> unit

val read_authorisation : t -> name -> Authorisation.t r_res
val write_authorisation : t -> Authorisation.t -> unit

val read_releases : t -> name -> Releases.t r_res
val write_releases : t -> Releases.t -> unit

val read_checksum : t -> name -> Checksum.t r_res
val write_checksum : t -> Checksum.t -> unit
val compute_checksum : t -> name -> Checksum.t r_res
