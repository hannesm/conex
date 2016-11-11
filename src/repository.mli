open Conex_result
open Conex_core
open Conex_resource

type t

val repository : ?store:Keystore.t -> ?quorum:int -> Provider.t -> t
val provider : t -> Provider.t
val quorum : t -> int
val teams : t -> S.t M.t

val team : t -> string -> S.t

val valid : t -> digest -> (name * Uint.t * resource * S.t) option

val change_provider : t -> Provider.t -> t

val verify : Publickey.t -> string -> Signature.t -> (identifier, [> verification_error]) result

val verify_index : t -> Index.t -> (identifier, [> verification_error ]) result

val pp_ok : Format.formatter -> [< `Signed of identifier | `Quorum of S.t | `Both of identifier * S.t ] -> unit

type base_error = [
  | `InvalidName of name * name
  | `InvalidResource of name * resource * resource
  | `NotSigned of name * resource * S.t
]

val pp_error : Format.formatter ->
  [< base_error
  | `InsufficientQuorum of name * S.t
  | `MissingSignature of identifier
  | `AuthRelMismatch of name * name
  | `InvalidReleases of name * S.t * S.t
  | `NotInReleases of name * S.t
  | `FileNotFound of name
  | `NotADirectory of name
  | `ChecksumsDiff of name * name list * name list * (Checksum.c * Checksum.c) list ]
  -> unit

val verify_key : t -> Publickey.t ->
  ([ `Quorum of S.t | `Both of identifier * S.t ],
   [> base_error | `InsufficientQuorum of name * S.t | `MissingSignature of identifier ]) result

val verify_team : t -> Team.t ->
  ([ `Quorum of S.t ],
   [> base_error | `InsufficientQuorum of name * S.t ]) result

val verify_authorisation : t -> Authorisation.t ->
  ([ `Quorum of S.t ],
   [> base_error | `InsufficientQuorum of name * S.t ]) result

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

val add_index : t -> Index.t -> t

val add_trusted_key : t -> Publickey.t -> t

val add_team : t -> Team.t -> t

val all_ids : t -> S.t
val all_authorisations : t -> S.t

type r_err = [ `NotFound of string | `ParseError of name * string | `NameMismatch of string * string ]

val pp_r_err : Format.formatter -> r_err -> unit

val read_id : t -> identifier ->
  ([ `Key of Publickey.t | `Team of Team.t ],
   [> r_err ]) result

val read_key : t -> identifier -> (Publickey.t, [> r_err ]) result
val write_key : t -> Publickey.t -> unit

val read_team : t -> identifier -> (Team.t, [> r_err ]) result
val write_team : t -> Team.t -> unit

val read_index : t -> identifier -> (Index.t, [> r_err ]) result
val write_index : t -> Index.t -> unit

val read_authorisation : t -> name -> (Authorisation.t, [> r_err ]) result
val write_authorisation : t -> Authorisation.t -> unit

val read_releases : t -> name -> (Releases.t, [> r_err ]) result
val write_releases : t -> Releases.t -> unit

val read_checksum : t -> name -> (Checksum.t, [> r_err ]) result
val write_checksum : t -> Checksum.t -> unit
