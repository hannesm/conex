type hunk

type diff = {
  mine_name : string ;
  their_name : string ;
  hunks : hunk list ;
}

val file : diff -> string
val apply : string option -> diff -> string option
val to_diffs : string -> diff list
