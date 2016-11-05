open Core
open Conex_resource

val string_to_index : string -> (Index.t, string) result
val index_to_string : Index.t -> string

val index_to_raw : Index.t -> string

val string_to_publickey : string -> (Publickey.t, string) result
val publickey_to_string : Publickey.t -> string

val string_to_releases : string -> (Releases.t, string) result
val releases_to_string : Releases.t -> string

val string_to_authorisation : string -> (Authorisation.t, string) result
val authorisation_to_string : Authorisation.t -> string

val string_to_team : string -> (Team.t, string) result
val team_to_string : Team.t -> string

val string_to_checksums : string -> (Checksum.t, string) result
val checksums_to_string : Checksum.t -> string
