open Conex_result
open Conex_utils
open Conex_resource
open Conex_crypto

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

module Make (L : LOGS) (C : VERIFY): sig
  val load_id : Conex_provider.t -> Conex_repository.t -> identifier -> (Conex_repository.t, string) result

  val load_ids : ?ids:S.t -> Conex_provider.t -> Conex_repository.t -> (Conex_repository.t, string) result

  val load_janitors : ?valid:(identifier -> string -> bool) -> Conex_provider.t -> Conex_repository.t -> (Conex_repository.t, string) result

  val verify_item :
    ?authorised:(S.t -> bool) -> ?release:(name -> bool) -> Conex_provider.t -> Conex_repository.t -> name ->
    (Conex_repository.t, string) result

  val verify_diff : Conex_provider.t -> Conex_repository.t -> string -> (Conex_repository.t, string) result
end
