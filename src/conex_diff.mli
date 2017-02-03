open Conex_core

type hunk

type diff = {
  mine_name : string ;
  their_name : string ;
  hunks : hunk list ;
}

val file : diff -> string
val to_diffs : string -> diff list

val diffs_to_components : diff list -> (S.t * S.t * S.t * S.t M.t)

val apply : Conex_provider.t -> diff -> Conex_provider.t
