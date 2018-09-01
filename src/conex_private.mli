(** Private key operations and handling *)

open Conex_utils

(** The private key module type *)
module type S = sig
  open Conex_resource

  (** The type of a private key *)
  type t

  (** [ids ()] is the list of all available private keys. *)
  val ids : unit -> identifier list

  type r_err = [ `Decode of string | `Read of string | `None | `Multiple of string list ]

  val pp_r_err : r_err fmt

  (** [read id] is either [Ok priv], the private key corresponding to [id], or
      an [Error].  *)
  val read : (float -> Conex_resource.timestamp option) -> identifier -> (t, r_err) result

  (** [bits t] is the number of bits of the private key [t]. *)
  val bits : t -> int

  (** [created t] is the timestamp when [t] was created. *)
  val created : t -> timestamp

  (** [id t] is the identifier of [t]. *)
  val id : t -> identifier

  (** [generate ~bits alg id ()] generates a fresh private key using [alg]
      for [id], or an error.  Generate also ensures to persistently store the
      generated key if desired. *)
  val generate : ?bits:int -> (float -> Conex_resource.timestamp option) ->
    Key.alg -> identifier -> unit -> (t, string) result

  (** [pub_of_priv priv] extracts the public key out of [priv]. *)
  val pub_of_priv : t -> Key.t

  (** [sign wire now id alg priv] signs [wire] with [priv] using [alg], and
     evaluates to a [signature], or an error. *)
  val sign : Wire.t -> timestamp -> identifier -> Signature.alg -> t ->
    (Signature.t, string) result
end

(** A simple IO module type for certain private key operations. *)
module type FS = sig

  (** [ids ()] is the list of available identifiers. *)
  val ids : unit -> Conex_resource.identifier list

  (** [read id] is either the content and creation timestamp of [id], or an
      error. *)
  val read : (float -> Conex_resource.timestamp option) -> Conex_resource.identifier -> ((string * Conex_resource.timestamp), string) result

  (** [write id data] stores [data] as [id] persistently, or errors. *)
  val write : Conex_resource.identifier -> string -> (unit, string) result
end

(** The RSA backend module type *)
module type S_RSA_BACK = sig

  (** The abstract type t for keys *)
  type t

  (** [decode_priv id ts data] decodes the private key from [data] and returns
      a [t] or an error. *)
  val decode_priv : string -> Conex_resource.timestamp -> string -> (t, string) result

  (** [bits t] is the number of bits in [t]. *)
  val bits : t -> int

  (** [created t] is the timestamp of creation of [t]. *)
  val created : t -> Conex_resource.timestamp

  (** [id t] is the identifier of [t]. *)
  val id : t -> Conex_resource.identifier

  (** [generate_rsa ~bits ()] generates an RSA private key. *)
  val generate_rsa : ?bits:int -> unit -> string * string

  (** [pub_of_priv_rsa priv] is the PEM-encoded PKCS8 public key of [priv]. *)
  val pub_of_priv_rsa : t -> string

  (** [sign_pss priv data] is either the raw PSS signature of [data] using
      [priv] or an error. *)
  val sign_pss : t -> string -> (string, string) result

  val sha256 : string -> string
end

(** Given a RSA backend, instantiate the private key module type S. *)
module Make (C : S_RSA_BACK) (F : FS) : S
