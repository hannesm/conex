(** Data provider using an existing provider and a diff *)

val apply : Conex_io.t -> Conex_diff.t list -> Conex_io.t

val apply_diff : Conex_io.t -> string -> (Conex_io.t * Conex_diff.t list)
