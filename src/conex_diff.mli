open Conex_utils

(** Diff: decode patch files into hunks. *)

(** A hunk. *)
type hunk

type operation =
  | Edit of string
  | Rename of string * string
  | Delete of string
  | Create of string

val pp_operation : Format.formatter -> operation -> unit

val operation_eq : operation -> operation -> bool

(** A diff is a list of hunks, and a filename (mine and their are different for
    file addition and removal, otherwise they should be equal. *)
type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}

val pp : Format.formatter -> t -> unit

(** [to_diffs str] decodes the given patch into a list of [diff]. *)
val to_diffs : string -> t list

(** [ids rootname keydir diffs] returns whether the root file was changed, and
    the set of modified ids. *)
val ids : string -> path -> t list -> (bool * S.t, string) result

(** [patch data diff] is [data'], which is the result of applying [diff] to
    [data]. *)
val patch : string option -> t -> string
