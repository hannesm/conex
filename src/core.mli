type ('a, 'b) result = Ok of 'a | Error of 'b

val pp_list : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

type file_type = File | Directory

type path = string list
val path_to_string : path -> string
val string_to_path : string -> path

type name = string
val pp_name : Format.formatter -> name -> unit
val name_equal : name -> name -> bool

(* keyid (NOTE: maybe abstract, checking for printable ASCII characters, case-insensitive, make private!) *)
type identifier = string
val pp_id : Format.formatter -> identifier -> unit
val id_equal : identifier -> identifier -> bool

type digest = string
val pp_digest : Format.formatter -> digest -> unit
val digest : string -> string

type resource = [
  | `PublicKey
  | `Checksum
  | `Releases
  | `JanitorIndex
  | `Authorisation
]
val resource_to_string : resource -> string
val string_to_resource : string -> resource option
val pp_resource : Format.formatter -> resource -> unit
val resource_equal : resource -> resource -> bool

type role = [ `Author | `Janitor | `Other of string ]
val role_to_string : role -> string
val string_to_role : string -> role
val pp_role : Format.formatter -> role -> unit

type error = [
  | `InvalidBase64Encoding of identifier * string
  | `InvalidSignature of identifier * resource * string * string
  | `InvalidPublicKey of identifier
  | `InvalidIdentifier of identifier
  | `InvalidCounter of string * int64 * int64
  | `InsufficientQuorum of string * identifier list
  | `InvalidAuthorisation of string * string
  | `InvalidReleases of string * string
  | `NotAuthorised of identifier
]

val pp_error : Format.formatter -> error -> unit

val (>>=) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val (<<|>>) : ('a, 'b) result -> ('a, 'b) result -> ('a, 'b) result
val guard : bool -> 'a -> (unit, 'a) result
val foldM : ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result
