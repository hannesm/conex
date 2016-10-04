open Core

val apply : Repository.t -> Diff.diff -> Repository.t

val verify_diff : Repository.t -> string -> (Repository.t, string) result

