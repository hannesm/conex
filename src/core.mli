type ('a, 'b) result = Ok of 'a | Error of 'b

type file_type = [ `File | `Directory ]

type path = string list
val path_to_string : path -> string
val string_to_path : string -> path

(* keyid (NOTE: maybe abstract, checking for printable ASCII characters, case-insensitive, make private!) *)
type identifier = string

type algorithm = [
  | `RSA_PSS
  | `RSA_PKCS
]

val algorithm_to_string : algorithm -> string
val string_to_algorithm : string -> algorithm
val pp_algorithm : Format.formatter -> algorithm -> unit

(* roles, store should be parametrised over them *)
type role = [ `Developer | `RepositoryMaintainer | `SnapshotBot ]
val role_to_string : role -> string
val string_to_role : string -> role
val pp_role : Format.formatter -> role -> unit

type error = [
  | `InvalidBase64Encoding of identifier * string
  | `InvalidSignature of identifier * algorithm * string * string
  | `InvalidRole of role * role
  | `InvalidPublicKey of identifier
  | `InvalidIdentifier of identifier
  | `InvalidCounter of string * int64 * int64
  | `InsufficientQuorum of string * identifier list * error list
  | `InvalidDelegate of string * string
  | `InvalidSignatures of string * error list
]

val pp_error : Format.formatter -> error -> unit

val (>>=) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val guard : bool -> 'a -> (unit, 'a) result
val foldM : ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result
val anyM : ('b -> ('a, 'c) result) -> 'b list -> ('a, unit) result
