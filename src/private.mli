open Conex_result
open Conex_core

val sign_index : Conex_resource.Index.t -> priv -> (Conex_resource.Index.t, string) result

val write_private_key : Repository.t -> string -> priv -> unit

type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list ]
val pp_err : Format.formatter -> err -> unit
val read_private_key : ?id:string -> Repository.t -> ((string * priv), err) result

val all_private_keys : Repository.t -> string list
