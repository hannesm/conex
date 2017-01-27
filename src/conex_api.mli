open Conex_result
open Conex_core
open Conex_repository

module Log : sig
  type ('a, 'b) msgf = (?header:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
  type 'a log = ('a, unit) msgf -> unit

  type level = [ `Debug | `Info | `Warn ]

  val set_level : level -> unit

  val set_styled : bool -> unit

  val debug : 'a log
  val info : 'a log
  val warn : 'a log
end

val load_janitors : ?valid:(identifier -> string -> bool) -> t -> (t, string) result

val load_id : t -> identifier -> (t, string) result

val verify_item :
  ?authorised:(S.t -> bool) -> ?release:(name -> bool) -> t -> name ->
  (t, string) result

val verify_diff : t -> string -> (t, string) result
