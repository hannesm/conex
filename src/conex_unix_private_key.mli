(** Private key IO *)

open Conex_utils
open Conex_resource

(** [write_private_key prov id key] writes the given key to disk (in
    "~/.conex/", using the [basedir] of [prov] as file prefix. *)
val write : Conex_io.t -> string -> Key.priv -> (unit, string) result

(** Potential read errors *)
type err = [ `NotFound of string | `Msg of string ]

(** [pp_err err] pretty prints [err]. *)
val pp_err : err fmt

(** [read_private_key prov id] is either [Ok priv] or an [Error].
    The private key corresponding to [prov] and [id] is
    loaded. *)
val read : Conex_io.t -> string -> (Key.priv, err) result

(** [find_ids ()] is a [(id, path)] list of private keys present. *)
val find_ids : unit -> ((string * string) list, string) result
