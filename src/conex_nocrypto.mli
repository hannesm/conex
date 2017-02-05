open Conex_result
open Conex_core

val verify_rsa_pss : key:string -> data:string -> signature:string -> (unit, [> verification_error ]) result

val pub_of_priv_rsa : string -> (string, string) result

val generate_rsa : ?bits:int -> unit -> string

val sign_rsa_pss : key:string -> string -> (string, string) result

val b64sha256 : string -> string
