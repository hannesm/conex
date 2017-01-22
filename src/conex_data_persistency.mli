open Conex_result
open Conex_resource

type s =
  | Map of s Conex_core.M.t
  | List of s list
  | String of string
  | Int of Conex_core.Uint.t

type t = s Conex_core.M.t

val t_to_team : t -> (Team.t, string) result
val team_to_t : Team.t -> t

val t_to_releases : t -> (Releases.t, string) result
val releases_to_t : Releases.t -> t

val t_to_authorisation : t -> (Authorisation.t, string) result
val authorisation_to_t : Authorisation.t -> t

val t_to_checksums : t -> (Checksum.t, string) result
val checksums_to_t : Checksum.t -> t

val t_to_index : t -> (Index.t, string) result
val index_to_t : Index.t -> t
val index_sigs_to_t : Index.t -> t
val publickey_to_t : Conex_core.identifier -> Conex_core.pub -> t
