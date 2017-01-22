open Conex_result
open Conex_core

val verify : pub -> string -> string -> (unit, [> verification_error ]) result

val id : pub -> string

val pub_of_priv : priv -> (pub, string) result

val generate : ?bits:int -> unit -> priv

val sign : priv -> string -> (string, string) result


val digest : string -> digest
