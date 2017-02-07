(** Repository state and verification logic

    The repository keeps track of valid resources, which are added after
    successful verification of author resources.  The only configuration element
    is the [quorum]: how many janitors have to sign off a change.

    The single resource which is signed with an asymmetric crypto operation is
    the author, which contains a list of resources the author signed off.  To
    verify a resource, first the authorised authors have to be verified.  Once
    they are verified, resource verification is a lookup of a computed hash in a
    map which is part of the repository.

    This module contains the program logic to verify the different resources,
    which have different rules:
    {ul
    {- {!S.verify_author} requires a self-signature with a verified key OR has
       an empty list of resources and approved by a quorum of janitors}
    {- {!S.verify_key} is successful if the key is in the authors resource list,
       and approved by a quorum of janitors}
    {- {!S.verify_team} is successful if approved by a quorum of janitors}
    {- {!S.verify_authorisation} is successful if approved by a quorum of
       janitors}
    {- {!S.verify_package} is successful if approved by an authorised author OR
       a quorum of janitors}
    {- {!S.verfy_release} is successful if approved by an authorised author OR a
       quorum of janitors}}

    Monotonicity requirements are as follows:
    {ul
    {- {!S.monoton_author} requires the counter to increase, deletion is not
       supported (they can have an empty resource list and no public key).}
    {- {!S.monoton_team} requires the counter to increase, deletion is not
       supported (a team may have no members)}
    {- {!S.monoton_authorisation} requires the counter to increase, deletion is
       not supported}
    {- {!S.monoton_package} requires the counter to increase, deletion is not
       supported (the list of releases can be empty)}
    {- {!S.monoton_release} requires the counter to increase}}

*)

(* TODO: this should not be functorised - can we take the digest (or function) as input?
   TODO: use verify for author only, and some other term (validate?) for the other resources *)
open Conex_utils
open Conex_resource
open Conex_crypto

(** {1 Repository type, constructor, predicates} *)

(** The repository type *)
type t

(* TODO: remove strict, deal via warn + err *)
(** [repository ~quorum ~strict ()] is a fresh empty repository. *)
val repository : ?quorum:int -> ?strict:bool -> unit -> t

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

(** [add_valid_resource repo id r] marks resource [r] valid under [id] in
    [repo].  If the digest of [r] is already present for a different resource,
    an error will be returned. *)
val add_valid_resource : t -> identifier -> Author.r -> (t, string) result

(** [add_index repo author] applies {!add_valid_resource} to each member of
    the resource list from [author]. *)
val add_index : t -> Author.t -> t * string list

(** [add_team repo team] adds the team to the repo. *)
val add_team : t -> Team.t -> t

module type S = sig

  (** [digest str] computes a digest from the given string. *)
  val digest : string -> Digest.t

  (** [id key] is the public key identifier. *)
  val id : Key.t -> string

  (* TODO: why not use Author.r here as well? *)
  (** [contains ~queued author name typ data] is [true] if [data] named [name]
      of [typ] is a member in the list of resources of [author].  If [queued] is
      [true], membership of the resource in the queued list of resources is
      sufficient.  *)
  val contains : ?queued:bool -> Author.t -> name -> typ -> Wire.t -> bool

  (** [pp_ok] pretty prints verification success *)
  val pp_ok : [< `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ] fmt

  (** Errors which can occur during any verification. *)
  type base_error = [
    | `InvalidName of name * name
    | `InvalidResource of name * typ * typ
    | `NotSigned of name * typ * S.t
  ]

  (** [pp_error] prints verification errors. *)
  val pp_error :
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
    fmt

  (** [verify id key data signature] succeeds when the [signature] was done
      using the private part of the [key], and the [id]. *)
  val verify : identifier -> Key.t -> string -> Signature.t -> (unit, [> verification_error]) result

  (** [verify_author repo author] verifies the author resource: at least one
      public keys must be signed off by janitors.  If successful, the resources
      of [author] are added to the [repo]. *)
  val verify_author : t -> Author.t -> (t * string list * identifier, [> verification_error ]) result

  (** [verify_signatures author] splits the keys into two partitions: those
      with a proper signatures and those without. *)
  val verify_signatures : Author.t -> Key.t list * (Key.t * verification_error) list

  (** [verify_key repo id key] *)
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
