type hunk

type diff = {
  mine_name : string ;
  their_name : string ;
  hunks : hunk list ;
}

val file : diff -> string
val apply : string option -> diff -> string
val to_diffs : string -> diff list
