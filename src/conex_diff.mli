open Conex_utils

(** Diff: decode patch files into hunks. *)

(** A hunk. *)
type hunk

(** A diff is a list of hunks, and a filename (mine and their are different for
    file addition and removal, otherwise they should be equal. *)
type t = {
  mine_name : string option ;
  their_name : string option ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}

val pp : Format.formatter -> t -> unit

(** [filename name] strips potentially leading "a/" or "b/" from given [name].
    If [name] is /dev/null, [None] is returned! *)
val filename : string -> string option

(** [to_diffs str] decodes the given patch into a list of [diff]. *)
val to_diffs : string -> t list

(** [ids rootname keydir diffs] returns whether the root file was changed, and
    the set of modified ids. *)
val ids : string -> path -> t list -> (bool * S.t, string) result

(** [patch data diff] is [data'], which is the result of applying [diff] to
    [data]. *)
val patch : string option -> t -> string
