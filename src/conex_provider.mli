open Conex_result
open Conex_core

val fs_provider : string -> (Provider.t, string) result
val fs_ro_provider : string -> (Provider.t, string) result
