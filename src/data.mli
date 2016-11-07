open Conex_result
open Conex_data_persistency

val decode : string -> (t, string) result
val encode : t -> string
