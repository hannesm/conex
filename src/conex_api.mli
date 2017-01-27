open Conex_result
open Conex_core
open Conex_repository

val load_janitors :
  ?valid:(identifier -> string -> bool) ->
  ?debug:Format.formatter ->
  ?out:Format.formatter ->
  t -> (t, string) result

val load_id :
  ?debug:Format.formatter ->
  ?out:Format.formatter ->
  t -> identifier -> (t, string) result

val verify_item :
  ?authorised:(S.t -> bool) ->
  ?release:(name -> bool) ->
  ?debug:Format.formatter ->
  ?out:Format.formatter ->
  t -> name -> (t, string) result

val verify_diff :
  ?debug:Format.formatter ->
  ?out:Format.formatter ->
  t -> string -> (t, string) result
