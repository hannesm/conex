open Conex_result
open Conex_core
open Conex_repository

(* this is stripped down from Logs library *)
module type LOGS = sig
  module Tag : sig
    type set
  end

  type ('a, 'b) msgf =
    (?header:string -> ?tags:Tag.set ->
     ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b
  type 'a log = ('a, unit) msgf -> unit

  type src

  val debug : ?src:src -> 'a log
  val info : ?src:src -> 'a log
  val warn : ?src:src -> 'a log
end

module Make (L : LOGS) : sig
  val load_id : Conex_provider.t -> t -> identifier -> (t, string) result

  val load_ids : ?ids:S.t -> Conex_provider.t -> t -> (t, string) result

  val load_janitors : ?valid:(identifier -> string -> bool) -> Conex_provider.t -> t -> (t, string) result

  val verify_item :
    ?authorised:(S.t -> bool) -> ?release:(name -> bool) -> Conex_provider.t -> t -> name ->
    (t, string) result

  val verify_diff : Conex_provider.t -> t -> string -> (t, string) result
end
