open Conex_result
open Conex_core
open Conex_resource

type t

val repository : ?quorum:int -> ?strict:bool -> unit -> t
val quorum : t -> int
val strict : t -> bool

val find_team : t -> identifier -> S.t option

val id_loaded : t -> identifier -> bool

(* Unsafe operation, think before usage! *)
val add_valid_resource : t -> identifier -> Index.r -> (t, string) result
val add_index : t -> Index.t -> t * string list

(* Unsafe operation, think before usage! *)
val add_team : t -> Team.t -> t
val add_id : t -> identifier -> t

val contains : ?queued:bool -> Index.t -> name -> resource -> Wire.t -> bool


val authorised : t -> Authorisation.t -> identifier -> bool

val verify : pub -> string -> signature -> (unit, [> verification_error]) result

val verify_index : t -> Index.t -> (t * string list * identifier, [> verification_error ]) result

val verify_signatures : Index.t -> pub list * (pub * verification_error) list

val pp_ok : Format.formatter -> [< `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ] -> unit

type base_error = [
  | `InvalidName of name * name
  | `InvalidResource of name * resource * resource
  | `NotSigned of name * resource * S.t
]

val pp_error : Format.formatter ->
  [< base_error
  | `InsufficientQuorum of name * resource * S.t
  | `MissingSignature of identifier
  | `AuthRelMismatch of name * name
  | `InvalidReleases of name * S.t * S.t
  | `NotInReleases of name * S.t
  | `ChecksumsDiff of name * name list * name list * (Checksum.c * Checksum.c) list ]
  -> unit

val verify_key : t -> identifier -> pub ->
 ([ `Both of identifier * S.t ],
  [> base_error | `InsufficientQuorum of name * resource * S.t | `MissingSignature of identifier ]) result

val verify_team : t -> Team.t ->
  (t * [ `Quorum of S.t ],
   [> base_error | `InsufficientQuorum of name * resource * S.t ]) result

val verify_authorisation : t -> Authorisation.t ->
  ([ `Quorum of S.t ],
   [> base_error | `InsufficientQuorum of name * resource * S.t ]) result

val verify_releases : t -> ?on_disk:Releases.t -> Authorisation.t -> Releases.t ->
  ([ `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ],
   [> base_error | `AuthRelMismatch of name * name | `InvalidReleases of name * S.t * S.t ]) result

val verify_checksum : t -> ?on_disk:Checksum.t -> Authorisation.t -> Releases.t -> Checksum.t ->
  ([ `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ],
   [> base_error
   | `AuthRelMismatch of name * name
   | `NotInReleases of name * S.t
   | `ChecksumsDiff of name * name list * name list * (Checksum.c * Checksum.c) list ]) result

(* Monotonicity *)
type m_err = [ `NotIncreased of resource * name | `Deleted of resource * name | `Msg of resource * string ]

val pp_m_err : Format.formatter -> m_err -> unit

val monoton_index : ?old:Index.t -> ?now:Index.t -> t -> (unit, m_err) result
val monoton_team : ?old:Team.t -> ?now:Team.t -> t -> (unit, m_err) result
val monoton_authorisation : ?old:Authorisation.t -> ?now:Authorisation.t -> t -> (unit, m_err) result
val monoton_releases : ?old:Releases.t -> ?now:Releases.t -> t -> (unit, m_err) result
val monoton_checksum : ?old:Checksum.t -> ?now:Checksum.t -> t -> (unit, m_err) result
