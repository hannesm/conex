open Conex_result
open Conex_core

val sign_index : Conex_resource.Index.t -> priv -> (Conex_resource.Index.t, string) result

val write_private_key : Conex_repository.t -> string -> priv -> (unit, string) result

type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list | `Msg of string ]
val pp_err : Format.formatter -> err -> unit
val read_private_key : ?id:string -> Conex_repository.t -> ((string * priv), err) result
