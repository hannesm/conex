(** Crypto provided by the nocrypto package *)

module C : sig
  include Conex_crypto.SIGN
end

module V : sig
  include Conex_crypto.VERIFY
end

module NC_S : (Conex_sign.S)

module NC_R : (Conex_repository.S)
