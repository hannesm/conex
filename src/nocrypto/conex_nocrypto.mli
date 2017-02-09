(** Crypto provided by the nocrypto package *)

module C : sig
  include Conex_crypto.SIGN_BACK
end

module V : sig
  include Conex_crypto.VERIFY_BACK
end

module NC_S : (Conex_crypto.SIGN)

module NC_V : (Conex_crypto.VERIFY)
