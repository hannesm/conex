open Conex_result
open Conex_core
open Conex_resource

type t

val repository : ?quorum:int -> ?strict:bool -> Provider.t -> t
val provider : t -> Provider.t
val quorum : t -> int
val strict : t -> bool

val find_team : t -> identifier -> S.t option

val id_loaded : t -> identifier -> bool

val authorised : t -> Authorisation.t -> identifier -> bool

val change_provider : t -> Provider.t -> t

val verify : pub -> string -> signature -> (unit, [> verification_error]) result

val verify_index : t -> Index.t -> (t * string list * identifier, [> verification_error ]) result

val verify_signatures : Index.t -> pub list * (pub * verification_error) list

val contains : ?queued:bool -> Index.t -> name -> resource -> Conex_data_persistency.t -> bool

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
  | `FileNotFound of name
  | `NotADirectory of name
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

val verify_releases : t -> Authorisation.t -> Releases.t ->
  ([ `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ],
   [> base_error | `AuthRelMismatch of name * name | `InvalidReleases of name * S.t * S.t ]) result

val compute_checksum : t -> name -> (Checksum.t, [ `FileNotFound of name | `NotADirectory of name ]) result

val verify_checksum : t -> Authorisation.t -> Releases.t -> Checksum.t ->
  ([ `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ],
   [> base_error
   | `AuthRelMismatch of name * name
   | `NotInReleases of name * S.t
   | `FileNotFound of name
   | `NotADirectory of name
   | `ChecksumsDiff of name * name list * name list * (Checksum.c * Checksum.c) list ]) result

(* Unsafe operation, think before usage! *)
val add_valid_resource : t -> identifier -> Index.r -> (t, string) result
val add_index : t -> Index.t -> t * string list

(* Unsafe operation, think before usage! *)
val add_team : t -> Team.t -> t
val add_id : t -> identifier -> t

val ids : t -> S.t
val items : t -> S.t
val subitems : t -> name -> S.t

type r_err = [ `NotFound of string * string | `ParseError of name * string | `NameMismatch of string * string ]

val pp_r_err : Format.formatter -> r_err -> unit

val read_id : t -> identifier ->
  ([ `Id of Index.t | `Team of Team.t ],
   [> r_err ]) result

val read_team : t -> identifier -> (Team.t, [> r_err ]) result
val write_team : t -> Team.t -> (unit, string) result

val read_index : t -> identifier -> (Index.t, [> r_err ]) result
val write_index : t -> Index.t -> (unit, string) result

val read_authorisation : t -> name -> (Authorisation.t, [> r_err ]) result
val write_authorisation : t -> Authorisation.t -> (unit, string) result

val read_releases : t -> name -> (Releases.t, [> r_err ]) result
val write_releases : t -> Releases.t -> (unit, string) result

val read_checksum : t -> name -> (Checksum.t, [> r_err ]) result
val write_checksum : t -> Checksum.t -> (unit, string) result

type m_err = [ r_err | `NotIncreased of resource * name | `Deleted of resource * name | `Msg of string ]

val pp_m_err : Format.formatter -> [< m_err ] -> unit

val monotonicity : t -> t -> resource -> name -> (unit, [> m_err ]) result
