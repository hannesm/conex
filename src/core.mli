type ('a, 'b) result = Ok of 'a | Error of 'b

type file_type = File | Directory

type path = string list
val path_to_string : path -> string
val string_to_path : string -> path

(* keyid (NOTE: maybe abstract, checking for printable ASCII characters, case-insensitive, make private!) *)
type identifier = string

type kind =
  | PublicKey
  | Checksum
  | Releases
  | JanitorIndex
  | Authorisation

val kind_to_string : kind -> string
val string_to_kind : string -> kind option
val pp_kind : Format.formatter -> kind -> unit

type role = Author | Janitor | Other of string
val role_to_string : role -> string
val string_to_role : string -> role
val pp_role : Format.formatter -> role -> unit

type error = [
  | `InvalidBase64Encoding of identifier * string
  | `InvalidSignature of identifier * kind * string * string
  | `InvalidRole of role * role
  | `InvalidPublicKey of identifier
  | `InvalidIdentifier of identifier
  | `InvalidCounter of string * int64 * int64
  | `InsufficientQuorum of string * identifier list * error list
  | `InvalidAuthorisation of string * string
  | `InvalidSignatures of string * error list
]

val pp_error : Format.formatter -> error -> unit

val (>>=) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val guard : bool -> 'a -> (unit, 'a) result
val foldM : ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result
val anyM : ('b -> ('a, 'c) result) -> 'b list -> ('a, unit) result
