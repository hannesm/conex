open Conex_utils

(** Diff: decode patch files into hunks. *)

(** A hunk *)
type hunk

val pp_hunk : Format.formatter -> hunk -> unit

type operation =
  | Edit of string
  | Rename of string * string
  | Delete of string
  | Create of string
  | Rename_only of string * string

val pp_operation : git:bool -> Format.formatter -> operation -> unit

val operation_eq : operation -> operation -> bool

(** A diff is a list of hunks, and an operation. *)
type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}

val pp : git:bool -> Format.formatter -> t -> unit

(** [to_diffs str] decodes the given patch into a list of [diff]. *)
val to_diffs : string -> t list

(** [patch data diff] is [data'], which is the result of applying [diff] to
    [data]. If [data'] is [None], it was deleted. *)
val patch : string option -> t -> string option

(** [ids rootname keydir diffs] returns whether the root file was changed, and
    the set of modified ids. *)
val ids : string -> path -> t list -> (bool * S.t, string) result
