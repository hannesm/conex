(** Private key IO *)

open Conex_utils
open Conex_resource

(** [write_private_key id key] writes the given key to disk (in "~/.conex/". *)
val write : string -> Key.priv -> (unit, string) result

(** Potential read errors *)
type err = [ `NotFound of string | `Msg of string ]

(** [pp_err err] pretty prints [err]. *)
val pp_err : err fmt

(** [read_private_key id] is either [Ok priv] or an [Error].  The private key
    corresponding to [id] is loaded. *)
val read : string -> (Key.priv, err) result

(** [find_ids ()] is a [id] list of private keys present. *)
val find_ids : unit -> (string list, string) result
