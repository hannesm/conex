open Conex_core
open Conex_resource

(* our keystore *)
type t

(* with a constructor *)
val empty : t
val size : t -> int
(* retrieval functions *)
val mem : t -> identifier -> bool
val find : t -> identifier -> Publickey.t
(* common modification *)
val add : t -> Publickey.t -> t
val remove : t -> identifier -> t
