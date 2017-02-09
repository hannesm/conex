(** Repository state and validation logic

    The repository keeps track of valid resources, which are added after
    successful verification of an author.  The only configuration element is the
    [quorum]: how many janitors have to sign off a change.

    The single resource which is signed with an asymmetric crypto operation is
    the author, which contains a list of resources the author signed off.  To
    validate a resource, first the authorised authors have to be validated.
    Once they are approved, resource validation is a lookup of a computed hash
    in a map which is part of the repository.

    This module contains the program logic to validate the different resources,
    which have different rules:
    {ul
    {- {!validate_author} requires a self-signature with a validated key OR has
       an empty list of resources and approved by a quorum of janitors}
    {- {!validate_account} is successful if the account is in the authors
       resource list, and approved by a quorum of janitors}
    {- {!validate_key} is successful if the key is in the authors resource list,
       and approved by a quorum of janitors}
    {- {!validate_team} is successful if approved by a quorum of janitors}
    {- {!validate_authorisation} is successful if approved by a quorum of
       janitors}
    {- {!validate_package} is successful if approved by an authorised author OR
       a quorum of janitors}
    {- {!validate_release} is successful if approved by an authorised author OR a
       quorum of janitors}}

    Monotonicity requirements are as follows:
    {ul
    {- {!monoton_author} requires the counter to increase, deletion is not
       supported (they can have an empty resource list and no public key).}
    {- {!monoton_team} requires the counter to increase, deletion is not
       supported (a team may have no members)}
    {- {!monoton_authorisation} requires the counter to increase, deletion is
       not supported}
    {- {!monoton_package} requires the counter to increase, deletion is not
       supported (the list of releases can be empty)}
    {- {!monoton_release} requires the counter to increase}}

*)

(* TODO: I can support removal of team and id: fold over items and look that id is not used! *)
open Conex_utils
open Conex_resource

(** {1 Repository type, constructor, predicates} *)

(** The repository type *)
type t

(* TODO: remove strict, deal via warn + err *)
(** [repository ~quorum ~strict digest ()] is a fresh empty repository. *)
val repository : ?quorum:int -> ?strict:bool -> (Wire.t -> Digest.t) -> unit -> t

val digestf : t -> (Wire.t -> Digest.t)

(** [quorum repo] is the configured quorum. *)
val quorum : t -> int

(** [strict repo] is the configured strictness *)
val strict : t -> bool

(** [find_team repo id] returns the members of the team [id] or [None] if there
    is no such team. *)
val find_team : t -> identifier -> S.t option

(** [id_loaded repo id] is [true] if [id] has been loaded. *)
val id_loaded : t -> identifier -> bool

(** [authorised repo auth id] is [true] if [id] is a member of [auth] or member
    of a team which is authorised. *)
val authorised : t -> Authorisation.t -> identifier -> bool

(** {2 Unsafe operations} *)

(** These operations extend which resources are trusted.  Usually you should not
    need them, but use {!validate_author} and {!validate_team} instead. *)

type conflict = [
  | `NameConflict of name * Author.r
  | `TypConflict of typ * Author.r
]

val pp_conflict : conflict fmt

(** [add_valid_resource repo id r] marks resource [r] valid under [id] in
    [repo].  If the digest of [r] is already present for a different resource,
    an error will be returned. *)
val add_valid_resource : t -> identifier -> Author.r -> (t, conflict) result

(** [add_index repo author] applies {!add_valid_resource} to each member of
    the resource list from [author]. *)
val add_index : t -> Author.t -> t * conflict list

(** [add_team repo team] adds the team to the repo. *)
val add_team : t -> Team.t -> t

val add_id : t -> identifier -> t

(** [contains ~queued author name typ data] is [true] if [data] named [name]
    of [typ] is a member in the list of resources of [author].  If [queued] is
    [true], membership of the resource in the queued list of resources is
    sufficient.  *)
val contains : ?queued:bool -> t -> Author.t -> name -> typ -> Wire.t -> bool

(** [pp_ok] pretty prints validation success *)
val pp_ok : [< `Approved of identifier | `Quorum of S.t | `Both of identifier * S.t ] fmt

(** Errors which can occur during any validation. *)
type base_error = [
  | `InvalidName of name * name
  | `InvalidResource of name * typ * typ
  | `NotApproved of name * typ * S.t
]

(** [pp_error] prints validation errors. *)
val pp_error :
  [< base_error
  | `InsufficientQuorum of name * typ * S.t
  | `AuthorWithoutKeys of identifier
  | `IdNotPresent of name * S.t
  | `MemberNotPresent of identifier * S.t
  | `AuthRelMismatch of name * name
  | `InvalidReleases of name * S.t * S.t
  | `NoSharedPrefix of name * S.t
  | `NotInReleases of name * S.t
  | `ChecksumsDiff of name * name list * name list * (Release.c * Release.c) list ]
    fmt

(** [validate_author repo author] validates the author resource: all
    signatures must be good, and at least one public key must be approved by
    janitors (or resource list is empty and it is approved by a quorum of
    janitors). *)
val validate_author :
  t -> Author.t ->
  (t * conflict list,
   [> base_error | `InsufficientQuorum of name * typ * S.t | `AuthorWithoutKeys of identifier ]) result

val validate_account : t -> Author.t -> Author.account ->
  ([ `Approved of identifier ],
   [> base_error | `InsufficientQuorum of name * typ * S.t ]) result

(** [validate_key repo id key] *)
val validate_key : t -> identifier -> Key.t ->
  ([ `Both of identifier * S.t ],
   [> base_error | `InsufficientQuorum of name * typ * S.t ]) result

val validate_team : t -> Team.t ->
  (t * [ `Quorum of S.t ],
   [> base_error | `InsufficientQuorum of name * typ * S.t | `MemberNotPresent of identifier * S.t ]) result

val validate_authorisation : t -> Authorisation.t ->
  ([ `Quorum of S.t ],
   [> base_error | `InsufficientQuorum of name * typ * S.t | `IdNotPresent of name * S.t ]) result

val validate_package : t -> ?on_disk:Package.t -> Authorisation.t -> Package.t ->
  ([ `Approved of identifier | `Quorum of S.t | `Both of identifier * S.t ],
   [> base_error | `AuthRelMismatch of name * name | `InvalidReleases of name * S.t * S.t | `NoSharedPrefix of name * S.t ]) result

val validate_release : t -> ?on_disk:Release.t -> Authorisation.t -> Package.t -> Release.t ->
  ([ `Approved of identifier | `Quorum of S.t | `Both of identifier * S.t ],
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
