(** Private key IO *)

open Conex_utils
open Conex_resource

(** [write_private_key prov id key] writes the given key to disk (in
    "~/.conex/", using the [basedir] of [prov] as file prefix. *)
val write : Conex_io.t -> string -> Key.priv -> (unit, string) result

(** Potential read errors *)
type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list | `Msg of string ]

(** [pp_err err] pretty prints [err]. *)
val pp_err : err fmt

(** [read_private_key ~id prov] is either [Ok (identifier, priv)] or an [Error].
    If [id] is provided, the private key corresponding to [prov] and [id] is
    loaded.  If no [id] is provided, and there is only one private key for the
    given [prov], it is loaded and [identifier] is returned. *)
val read : ?id:string -> Conex_io.t -> ((string * Key.priv), err) result
