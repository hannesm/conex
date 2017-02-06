open Conex_result
open Conex_crypto

open Conex_result
open Conex_crypto

module C : sig
  include SIGN
end

module V : sig
  include VERIFY
end

module NC_S : (Conex_sign.S)

module NC_R : (Conex_repository.S)
