open Conex_utils

(** Diff: decode patch files into hunks. *)

(** A hunk. *)
type hunk

(** A diff is a list of hunks, and a filename (mine and their are different for
    file addition and removal, otherwise they should be equal. *)
type t = {
  mine_name : string ;
  their_name : string ;
  hunks : hunk list ;
}

(** [file diff] is [mine_name] unless this is "/dev/null", in which case
    [their_name] is used.  A potentially leading "a/" or "b/" is stripped from
    mine/their. *)
val file : t -> string

(** [to_diffs str] decodes the given patch into a list of [diff]. *)
val to_diffs : string -> t list

(** [ids rootname keydir diffs] returns whether the root file was changed, and
    the set of modified ids. *)
val ids : string -> path -> t list -> (bool * S.t, string) result

(** [patch data diff] is [data'], which is the result of applying [diff] to
    [data]. *)
val patch : string option -> t -> string
