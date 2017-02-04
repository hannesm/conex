open Conex_result
open Conex_resource.Wire

val decode : string -> (t, string) result
val encode : t -> string
