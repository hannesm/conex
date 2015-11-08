type t

val parse : string -> t
val normalise : t -> string

val signature_to_data : Signature.t -> t
val data_to_signature : t -> Signature.t

val parse_signed_data : t -> (t * Signature.t list)
val combine_signed : t -> Signature.t list -> t

val publickey_to_data : Publickey.t -> t
val data_to_publickey : t -> Publickey.t
val publickey_raw : Publickey.t -> string

val delegate_to_data : Delegate.t -> t
val data_to_delegate : t -> Delegate.t
val delegate_raw : Delegate.t -> string

val checksums_to_data : Checksum.t -> t
val data_to_checksums : t -> Checksum.t
val checksums_raw : Checksum.t -> string
