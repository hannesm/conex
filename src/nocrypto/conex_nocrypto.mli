(** Crypto provided by the nocrypto package *)

module C : sig
  include Conex_crypto.SIGN_BACK
end

module V : sig
  include Conex_crypto.VERIFY
end

module NC_S : (Conex_crypto.SIGN)

module NC_R : (Conex_repository.S)
