open Conex_utils

(** Diff: decode patch files into hunks. *)

(** A hunk. *)
type hunk

(** A diff is a list of hunks, and a filename (mine and their are different for
    file addition and removal, otherwise they should be equal. *)
type diff = {
  mine_name : string ;
  their_name : string ;
  hunks : hunk list ;
}

(** [file diff] is [mine_name] unless this is "/dev/null", in which case
    [their_name] is used.  A potentially leading "a/" or "b/" is stripped from
    mine/their. *)
val file : diff -> string

(** [to_diffs str] decodes the given patch into a list of [diff]. *)
val to_diffs : string -> diff list

(** [diffs_to_components diffs] uses {!Conex_opam_repository_layout.categorise}
    to gather [id], [authorisation], [package], and [release] into three sets
    and a map. *)
val diffs_to_components : diff list -> (S.t * S.t * S.t * S.t M.t)

(** [patch data diff] is [data'], which is the result of applying [diff] to
    [data]. *)
val patch : string option -> diff -> string
