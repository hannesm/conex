type t

val parse : string -> t
val normalise : t -> string

val signature_to_data : Signature.t -> t
val data_to_signature : t -> Signature.t

val parse_signed_data : t -> (t * Signature.t list)
val combine_signed : t -> Signature.t list -> t

val index_to_data : Index.t -> t
val data_to_index : t -> Index.t
val index_raw : Index.t -> string

val publickey_to_data : Publickey.t -> t
val data_to_publickey : t -> Publickey.t
val publickey_raw : Publickey.t -> string

val releases_to_data : Releases.t -> t
val data_to_releases : t -> Releases.t
val releases_raw : Releases.t -> string

val authorisation_to_data : Authorisation.t -> t
val data_to_authorisation : t -> Authorisation.t
val authorisation_raw : Authorisation.t -> string

val checksums_to_data : Checksum.t -> t
val data_to_checksums : t -> Checksum.t
val checksums_raw : Checksum.t -> string
