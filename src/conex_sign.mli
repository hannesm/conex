open Conex_result
open Conex_utils
open Conex_resource

val generate : ?bits:int -> Uint.t -> unit -> Key.priv

val pub_of_priv : Key.priv -> (Key.t, string) result

val sign : Uint.t -> Author.t -> Key.priv -> (Author.t, string) result

val write_private_key : Conex_provider.t -> string -> Key.priv -> (unit, string) result

type err = [ `NotFound of string | `NoPrivateKey | `MultiplePrivateKeys of string list | `Msg of string ]
val pp_err : Format.formatter -> err -> unit
val read_private_key : ?id:string -> Conex_provider.t -> ((string * Key.priv), err) result
