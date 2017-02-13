(** Verification crypto provided by the OpenSSL command line tool *)

module V : sig
  include Conex_crypto.VERIFY_BACK
end

module O_V : (Conex_crypto.VERIFY)
