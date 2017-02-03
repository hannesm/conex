open Conex_result
open Conex_core
open Conex_resource
open Conex_provider

type cc_err = [ `FileNotFound of name | `NotADirectory of name ]
val compute_checksum : t -> name -> (Checksum.t, cc_err) result

val pp_cc_err : Format.formatter -> cc_err -> unit

val compute_releases : t -> name -> (Releases.t, string) result

val ids : t -> S.t
val items : t -> S.t
val subitems : t -> name -> S.t

type r_err = [ `NotFound of string * string | `ParseError of name * string | `NameMismatch of string * string ]

val pp_r_err : Format.formatter -> r_err -> unit

val read_id : t -> identifier ->
  ([ `Id of Index.t | `Team of Team.t ],
   [> r_err ]) result

val read_team : t -> identifier -> (Team.t, [> r_err ]) result
val write_team : t -> Team.t -> (unit, string) result

val read_index : t -> identifier -> (Index.t, [> r_err ]) result
val write_index : t -> Index.t -> (unit, string) result

val read_authorisation : t -> name -> (Authorisation.t, [> r_err ]) result
val write_authorisation : t -> Authorisation.t -> (unit, string) result

val read_releases : t -> name -> (Releases.t, [> r_err ]) result
val write_releases : t -> Releases.t -> (unit, string) result

val read_checksum : t -> name -> (Checksum.t, [> r_err ]) result
val write_checksum : t -> Checksum.t -> (unit, string) result

(* this should likely live in the individual Conex_resource modules (which should get a module sig!?) *)
type m_err = [ r_err | `NotIncreased of resource * name | `Deleted of resource * name | `Msg of string ]

val pp_m_err : Format.formatter -> [< m_err ] -> unit

val monotonicity : t -> t -> resource -> name -> (unit, [> m_err ]) result
