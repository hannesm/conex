open Conex_result
open Conex_resource
open Conex_core

val verify : Key.t -> string -> string -> (unit, [> verification_error ]) result

val id : Key.t -> string

val pub_of_priv : Key.priv -> (Key.t, string) result

val generate : ?bits:int -> unit -> Key.priv

val sign : Key.priv -> string -> (string, string) result


val digest : string -> Digest.t
