(** Private key operations and handling *)

open Conex_utils

(** The private key module type *)
module type S = sig
  open Conex_resource

  (** The type of a private key *)
  type t

  (** [ids ()] is the list of all available private keys. *)
  val ids : unit -> identifier list

  (** [read id] is either [Ok priv], the private key corresponding to [id], or
      an [Error].  *)
  val read : identifier -> (t, string) result

  (** [bits t] is the number of bits of the private key [t]. *)
  val bits : t -> int

  (** [created t] is the timestamp when [t] was created. *)
  val created : t -> Uint.t

  (** [generate ~bits alg id now ()] generates a fresh private key using [alg]
      for [id], or an error.  Generate also ensures to persistently store the
      generated key if desired. *)
  val generate : ?bits:int -> Key.alg -> identifier -> Uint.t -> unit ->
    (t, string) result

  (** [pub_of_priv priv] extracts the public key out of [priv], or an error. *)
  val pub_of_priv : t -> (Key.t, string) result

  (** [sign now idx alg priv] signs [idx] with [priv] using [alg], and evaluates
      to a new [idx], or an error.  The counter of [idx] is increased. *)
  val sign : Uint.t -> Author.t -> Signature.alg -> t ->
    (Author.t, string) result
end

(** A simple IO module type for certain private key operations. *)
module type FS = sig

  (** [ids ()] is the list of available identifiers. *)
  val ids : unit -> string list

  (** [read id] is either the content and creation timestamp of [id], or an
      error. *)
  val read : string -> ((string * Conex_utils.Uint.t), string) result

  (** [write id data] stores [data] as [id] persistently, or errors. *)
  val write : string -> string -> (unit, string) result
end

(** The RSA backend module type *)
module type S_RSA_BACK = sig

  (** The abstract type t for keys *)
  type t

  (** [ids ()] is the list of available identifiers. *)
  val ids : unit -> string list

  (** [read id] is either [t], the RSA private key of [id], or an error. *)
  val read : string -> (t, string) result

  (** [bits t] is the number of bits in [t]. *)
  val bits : t -> int

  (** [created t] is the timestamp of creation of [t]. *)
  val created : t -> Uint.t

  (** [generate_rsa ~bits id now ()] generates an RSA private key for [id], and
      stores it persistently, or signals an error. *)
  val generate_rsa : ?bits:int -> string -> Uint.t -> unit -> (t, string) result

  (** [pub_of_priv_rsa priv] is either a PEM encoded PKCS8 public key of [priv]
      or an error. *)
  val pub_of_priv_rsa : t -> (string, string) result

  (** [sign_pss priv data] is either the raw PSS signature of [data] using
      [priv] or an error. *)
  val sign_pss : t -> string -> (string, string) result
end

(** Given a RSA backend, instantiate the private key module type S. *)
module Make (C : S_RSA_BACK) : S
