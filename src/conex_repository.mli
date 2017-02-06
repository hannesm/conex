open Conex_result
open Conex_utils
open Conex_resource
open Conex_crypto

type t

val repository : ?quorum:int -> ?strict:bool -> unit -> t
val quorum : t -> int
val strict : t -> bool

val find_team : t -> identifier -> S.t option

val id_loaded : t -> identifier -> bool

val authorised : t -> Authorisation.t -> identifier -> bool

(* Unsafe operation, think before usage! *)
val add_valid_resource : t -> identifier -> Author.r -> (t, string) result
val add_index : t -> Author.t -> t * string list

(* Unsafe operation, think before usage! *)
val add_team : t -> Team.t -> t

module type S = sig

  val digest : string -> Digest.t
  val id : Key.t -> string

  val contains : ?queued:bool -> Author.t -> name -> typ -> Wire.t -> bool

  val pp_ok : Format.formatter -> [< `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ] -> unit

  type base_error = [
    | `InvalidName of name * name
    | `InvalidResource of name * typ * typ
    | `NotSigned of name * typ * S.t
  ]

  val pp_error : Format.formatter ->
    [< base_error
    | `InsufficientQuorum of name * typ * S.t
    | `MissingSignature of identifier
    | `IdNotPresent of name * S.t
    | `MemberNotPresent of identifier * S.t
    | `AuthRelMismatch of name * name
    | `InvalidReleases of name * S.t * S.t
    | `NoSharedPrefix of name * S.t
    | `NotInReleases of name * S.t
    | `ChecksumsDiff of name * name list * name list * (Release.c * Release.c) list ]
    -> unit

  val verify : identifier -> Key.t -> string -> Signature.t -> (unit, [> verification_error]) result

  val verify_author : t -> Author.t -> (t * string list * identifier, [> verification_error ]) result

  val verify_signatures : Author.t -> Key.t list * (Key.t * verification_error) list

  val verify_key : t -> identifier -> Key.t ->
    ([ `Both of identifier * S.t ],
     [> base_error | `InsufficientQuorum of name * typ * S.t | `MissingSignature of identifier ]) result

  val verify_team : t -> Team.t ->
    (t * [ `Quorum of S.t ],
     [> base_error | `InsufficientQuorum of name * typ * S.t | `MemberNotPresent of identifier * S.t ]) result

  val verify_authorisation : t -> Authorisation.t ->
    ([ `Quorum of S.t ],
     [> base_error | `InsufficientQuorum of name * typ * S.t | `IdNotPresent of name * S.t ]) result

  val verify_package : t -> ?on_disk:Package.t -> Authorisation.t -> Package.t ->
    ([ `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ],
     [> base_error | `AuthRelMismatch of name * name | `InvalidReleases of name * S.t * S.t | `NoSharedPrefix of name * S.t ]) result

  val verify_release : t -> ?on_disk:Release.t -> Authorisation.t -> Package.t -> Release.t ->
    ([ `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ],
     [> base_error
     | `AuthRelMismatch of name * name
     | `NotInReleases of name * S.t
     | `ChecksumsDiff of name * name list * name list * (Release.c * Release.c) list ]) result

  (* Monotonicity *)
  type m_err = [ `NotIncreased of typ * name | `Deleted of typ * name | `Msg of typ * string ]

  val pp_m_err : Format.formatter -> m_err -> unit

  val monoton_author : ?old:Author.t -> ?now:Author.t -> t -> (unit, m_err) result
  val monoton_team : ?old:Team.t -> ?now:Team.t -> t -> (unit, m_err) result
  val monoton_authorisation : ?old:Authorisation.t -> ?now:Authorisation.t -> t -> (unit, m_err) result
  val monoton_package : ?old:Package.t -> ?now:Package.t -> t -> (unit, m_err) result
  val monoton_release : ?old:Release.t -> ?now:Release.t -> t -> (unit, m_err) result
end

module Make (C : VERIFY) : S
