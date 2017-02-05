open Conex_result
open Conex_utils
open Conex_resource
open Conex_provider

type cc_err = [ `FileNotFound of name | `NotADirectory of name ]
val compute_release : t -> Uint.t -> name -> (Release.t, cc_err) result

val pp_cc_err : Format.formatter -> cc_err -> unit

val compute_package : t -> Uint.t -> name -> (Package.t, string) result

val ids : t -> (S.t, string) result
val items : t -> (S.t, string) result
val subitems : t -> name -> (S.t, string) result

type r_err = [ `NotFound of typ * name | `ParseError of typ * name * string | `NameMismatch of typ * name * name ]

val pp_r_err : Format.formatter -> r_err -> unit

val read_id : t -> identifier ->
  ([ `Author of Author.t | `Team of Team.t ],
   [> r_err ]) result

val read_team : t -> identifier -> (Team.t, [> r_err ]) result
val write_team : t -> Team.t -> (unit, string) result

val read_author : t -> identifier -> (Author.t, [> r_err ]) result
val write_author : t -> Author.t -> (unit, string) result

val read_authorisation : t -> name -> (Authorisation.t, [> r_err ]) result
val write_authorisation : t -> Authorisation.t -> (unit, string) result

val read_package : t -> name -> (Package.t, [> r_err ]) result
val write_package : t -> Package.t -> (unit, string) result

val read_release : t -> name -> (Release.t, [> r_err ]) result
val write_release : t -> Release.t -> (unit, string) result
