open Conex_result
open Conex_core
open Conex_resource

type component =
  | Idx of identifier * Diff.diff
  | Id of identifier * Diff.diff
  | Authorisation of name * Diff.diff
  | Dir of name * name * Diff.diff list
  | OldDir of name * name * Diff.diff list

val pp_component : Format.formatter -> component -> unit

val categorise : Diff.diff -> component option

val diffs_to_components : Diff.diff list -> component list

val apply : Repository.t -> Diff.diff -> Repository.t

type err = [ verification_error
           | Repository.base_error
           | `InsufficientQuorum of name * S.t
           | `InvalidReleases of name * S.t * S.t
           | `AuthRelMismatch of name * name
           | `NotInReleases of name * S.t
           | `FileNotFound of name
           | `NotADirectory of name
           | `ChecksumsDiff of name * name list * name list * (Checksum.c * Checksum.c) list
           | `NameMismatch of string * string
           | `ParseError of name * string
           | `MissingSignature of identifier
           | `NotFound of string
           | `CounterNotIncreased
           | `CounterNotZero
           | `IllegalId
           | `IllegalName
           | `InvalidKeyTeam
           | `MissingAuthorisation of name ]

val verify_diff : Repository.t -> string -> (Repository.t, [> err ]) result
