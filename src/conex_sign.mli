(** Sign: signing resources, and private key utilities. *)

open Conex_utils
open Conex_resource
open Conex_crypto

(** [write_private_key prov id key] writes the given key to disk (in
    "~/.conex/", using the [basedir] of [prov] as file prefix. *)
val write_private_key : Conex_io.t -> string -> Key.priv -> (unit, string) result

(** Potential read errors *)
type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list | `Msg of string ]

(** [pp_err err] pretty prints [err]. *)
val pp_err : err fmt

(** [read_private_key ~id prov] is either [Ok (identifier, priv)] or an [Error].
    If [id] is provided, the private key corresponding to [prov] and [id] is
    loaded.  If no [id] is provided, and there is only one private key for the
    given [prov], it is loaded and [identifier] is returned. *)
val read_private_key : ?id:string -> Conex_io.t -> ((string * Key.priv), err) result

module type S = sig

  (** [generate ?bits created ()] generates a [bits] sized private key. *)
  val generate : ?bits:int -> Uint.t -> unit -> Key.priv

  (** [pub_of_priv priv] extracts the public key from the private. *)
  val pub_of_priv : Key.priv -> (Key.t, string) result

  (** [sign created author priv] uses {!Conex_resource.Author.prep_sig} to
      prepare [author] (incrementing counter, moving queued resource to signed).
      {!Conex_resource.Signature.wire} is used to produce the to-be-signed
      data.  This includes the author name, and the signature algorithm (at the
      moment RSA-PSS-SHA256). *)
  val sign : Uint.t -> Author.t -> Key.priv -> (Author.t, string) result
end

(** Instantiating the functor. *)
module Make (C : SIGN) : S
