open Core

type priv
val decode_priv : string -> priv
val encode_priv : priv -> string

val pp_priv : Format.formatter -> priv -> unit

(* how's the public? *)
val pub_of_priv : priv -> Publickey.pub
(* generate a new private *)
val generate : unit -> priv
(* sign a piece of data *)
val sign : ?algorithm:algorithm -> identifier -> priv -> string -> Signature.t

val write_private_key : Repository.t -> string -> priv -> unit

type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list ]
val pp_err : Format.formatter -> err -> unit
val read_private_key : ?id:string -> Repository.t -> ((string * priv), err) result

val all_private_keys : Repository.t -> string list
