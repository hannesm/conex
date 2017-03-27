(** Repository state and validation logic

    The repository keeps track of valid resources, which are added after
    successful verification of an author.  The only configuration element is the
    [quorum]: how many janitors have to sign off a change.

    The only resource which is signed with an asymmetric cryptographic operation
    is the author, which contains a list of resources the author vouches for.
    To validate a resource, first the authorised authors have to be validated.
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
    {- {!validate_releases} is successful if approved by an authorised author OR
       a quorum of janitors}
    {- {!validate_checksums} is successful if approved by an authorised author OR a
       quorum of janitors}}

    Monotonicity requirements are as follows:
    {ul
    {- {!monoton_author} requires the counter to increase, deletion is not
       supported (they can have an empty resource list and no public key).}
    {- {!monoton_team} requires the counter to increase, deletion is not
       supported (a team may have no members)}
    {- {!monoton_authorisation} requires the counter to increase, deletion is
       not supported}
    {- {!monoton_releases} requires the counter to increase, deletion is not
       supported (the list of releases can be empty)}
    {- {!monoton_checksums} requires the counter to increase}}

*)

(* TODO: I can support removal of team and id: fold over packages and look that id is not used! *)
open Conex_utils
open Conex_resource

(** {1 Repository type, constructor, predicates} *)

(** The repository type *)
type t

(** [repository ~quorum ~strict digest ()] is a fresh empty repository. *)
val repository : ?quorum:int -> (Wire.t -> Digest.t) -> unit -> t

val digestf : t -> (Wire.t -> Digest.t)

(** [quorum repo] is the configured quorum. *)
val quorum : t -> int

(** [find_team repo id] returns the members of the team [id] or [None] if there
    is no such team. *)
val find_team : t -> identifier -> S.t option

(** [id_loaded repo id] is [true] if [id] has been loaded. *)
val id_loaded : t -> identifier -> bool

(** [authorised repo auth id] is [true] if [id] is a member of [auth] or member
    of a team which is authorised. *)
val authorised : t -> Authorisation.t -> identifier -> bool

(** [contains ~queued author name typ data] is [true] if [data] named [name]
    of [typ] is a member in the list of resources of [author].  If [queued] is
    [true], membership of the resource in the queued list of resources is
    sufficient.  *)
val contains : ?queued:bool -> t -> Author.t -> name -> typ -> Wire.t -> bool

(** {1 Validation} *)

(** The variant of a conflict in the digest map. *)
type conflict = [
  | `NameConflict of name * Author.r
  | `TypConflict of typ * Author.r
]

(** [pp_conflict] is a pretty printer for {!conflict}. *)
val pp_conflict : conflict fmt


(** [pp_ok] pretty prints validation success *)
val pp_ok :
  [< `Approved of identifier
  | `Quorum of S.t
  | `Both of identifier * S.t ]
    fmt

(** Errors which can occur during any validation. *)
type base_error = [
  | `InvalidName of name * name
  | `InvalidResource of name * typ * typ
  | `NotApproved of name * typ * S.t
]

(** [pp_error] prints validation errors. *)
val pp_error :
  [< base_error
  | conflict
  | `InsufficientQuorum of name * typ * S.t * int
  | `AuthorWithoutKeys of identifier
  | `IdNotPresent of name * S.t
  | `MemberNotPresent of identifier * S.t
  | `AuthRelMismatch of name * name
  | `InvalidReleases of name * S.t * S.t
  | `NoSharedPrefix of name * S.t
  | `NotInReleases of name * S.t
  | `ChecksumsDiff of name * name list * name list * (Checksums.c * Checksums.c) list ]
    fmt

(** [validate_author repo author] validates [author]: at least one public key
    must be approved by a quorum of janitors or the resource list is empty and
    it is approved by a quorum of janitors.  The valid resource list is added to
    the repository (using {!add_index}). *)
val validate_author :
  t -> Author.t ->
  (t,
   [> base_error
   | conflict
   | `InsufficientQuorum of name * typ * S.t * int
   | `AuthorWithoutKeys of identifier ]) result

(** [validate_account repo author account] validates [account] of [author]: a
    quorum of janitors have to approve an account. *)
val validate_account : t -> Author.t -> Author.account ->
  ([ `Both of identifier * S.t ],
   [> base_error | `InsufficientQuorum of name * typ * S.t * int ]) result

(** [validate_key repo id key] validates [key] of [id]: a quorum of janitors
    have to approve a key. *)
val validate_key : t -> identifier -> Key.t ->
  ([ `Both of identifier * S.t ],
   [> base_error | `InsufficientQuorum of name * typ * S.t * int ]) result

(** [validate_team repo team] validates [team]: a quorum of janitors have to
    approve a team, and all team members must be in the repository.  If valid,
    the team is added to [repo] (using {!add_team}). *)
val validate_team : t -> Team.t ->
  (t * [ `Quorum of S.t ],
   [> base_error
   | `InsufficientQuorum of name * typ * S.t * int
   | `MemberNotPresent of identifier * S.t ]) result

(** [validate_authorisation repo auth] validates [auth]: a quorum of janitors
    have to approve an authorisation, and all authorised ids have to be present
    in [repo]. *)
val validate_authorisation : t -> Authorisation.t ->
  ([ `Quorum of S.t ],
   [> base_error
   | `InsufficientQuorum of name * typ * S.t * int
   | `IdNotPresent of name * S.t ]) result

(** [validate_releases repo ~on_disk auth releases] validates [releases]: an
    {!authorised} identity must approve the package in [repo], all releases must
    be prefixed with the package name, and if [on_disk] is given, the list of
    releases have to be identical. *)
val validate_releases : t -> ?on_disk:Releases.t -> Authorisation.t -> Releases.t ->
  ([ `Approved of identifier | `Quorum of S.t | `Both of identifier * S.t ],
   [> base_error
   | `AuthRelMismatch of name * name
   | `InvalidReleases of name * S.t * S.t
   | `NoSharedPrefix of name * S.t ]) result

(** [validate_checksums repo ~on_disk auth releases checksums] validates [checksums]:
    an {!authorised} (using [auth]) identity must approve the release in [repo],
    it must be part of the releases of [package], and if [on_disk] is given,
    the files listed must be equal, as well as their checksums.  *)
val validate_checksums : t -> ?on_disk:Checksums.t -> Authorisation.t -> Releases.t -> Checksums.t ->
  ([ `Approved of identifier | `Quorum of S.t | `Both of identifier * S.t ],
   [> base_error
   | `AuthRelMismatch of name * name
   | `NotInReleases of name * S.t
   | `ChecksumsDiff of name * name list * name list * (Checksums.c * Checksums.c) list ]) result

(** {1 Monotonicity} *)

(** The variant of monotonicity errors. *)
type m_err = [
  | `NotIncreased of typ * name
  | `Deleted of typ * name
  | `Msg of typ * string
]

(** [pp_m_err] is a pretty printer for {!m_err}. *)
val pp_m_err : m_err fmt

(** [monoton_author ~old ~now repo] checks that the counter increased. *)
val monoton_author : ?old:Author.t -> ?now:Author.t -> t -> (unit, m_err) result

(** [monoton_team ~old ~now repo] checks that the counter increased. *)
val monoton_team : ?old:Team.t -> ?now:Team.t -> t -> (unit, m_err) result

(** [monoton_authorisation ~old ~now repo] checks that the counter increased. *)
val monoton_authorisation : ?old:Authorisation.t -> ?now:Authorisation.t -> t -> (unit, m_err) result

(** [monoton_releases ~old ~now repo] checks that the counter increased. *)
val monoton_releases : ?old:Releases.t -> ?now:Releases.t -> t -> (unit, m_err) result

(** [monoton_checksums ~old ~now repo] checks that the counter increased. *)
val monoton_checksums : ?old:Checksums.t -> ?now:Checksums.t -> t -> (unit, m_err) result

(** {1 Unsafe operations} *)

(** These operations extend which resources are trusted.  Usually you should not
    need them, but use {!validate_author} and {!validate_team} instead. *)

(** [add_valid_resource id repo r] marks resource [r] valid under [id] in
    [repo].  If the digest of [r] is already present for a different resource,
    an error will be returned. *)
val add_valid_resource : identifier -> t -> Author.r -> (t, conflict) result

(** [add_index repo author] applies {!add_valid_resource} to each member of
    the resource list from [author]. *)
val add_index : t -> Author.t -> (t, conflict) result

(** [add_team repo team] adds the team to the repo. *)
val add_team : t -> Team.t -> t

(** [add_id repo id] adds the id to the repo. *)
val add_id : t -> identifier -> t
