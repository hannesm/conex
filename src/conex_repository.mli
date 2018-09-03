open Conex_resource
open Conex_utils

type t

val root : t -> Root.t

val keydir : t -> path

val datadir : t -> path

val targets : t -> (Digest.t * Uint.t * S.t) Tree.t

val with_targets : t -> (Digest.t * Uint.t * S.t) Tree.t -> t

val maintainer_delegation : t -> (Expression.t * bool * S.t) option

val create : Root.t -> t


type res = [
  | `Only_on_disk of path
  | `Only_in_targets of path
  | `No_match of path * (Digest.t * Uint.t) list * (Digest.t * Uint.t * S.t) list
]

val pp_res : res fmt

val validate_targets : t -> (Digest.t * Uint.t) Tree.t -> res list

val collect_and_validate_delegations : (Digest.t * Uint.t) M.t -> path ->
  Expression.t -> Targets.t list -> (path * Expression.t * bool * S.t) list

val collect_and_validate_targets : ?tree:(Digest.t * Uint.t * S.t) Tree.t ->
  (Digest.t * Uint.t) M.t -> path -> Expression.t -> Targets.t list ->
  (Digest.t * Uint.t * S.t) Tree.t
