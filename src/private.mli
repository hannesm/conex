open Core

type priv

val pp_priv : Format.formatter -> priv -> unit

(* how's the public? *)
val pub_of_priv : priv -> Publickey.pub
(* generate a new private *)
val generate : ?bits:int -> unit -> priv
(* sign a piece of data *)
val sign : identifier -> priv -> string -> Signature.t

val sign_index : Index.t -> priv -> Index.t

val write_private_key : Repository.t -> string -> priv -> unit

type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list ]
val pp_err : Format.formatter -> err -> unit
val read_private_key : ?id:string -> Repository.t -> ((string * priv), err) result

val all_private_keys : Repository.t -> string list
