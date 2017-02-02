open Conex_result

module Uint : sig
  type t

  val zero : t

  val compare : t -> t -> int

  val succ : t -> bool * t

  val to_string : t -> string

  val of_string : string -> t

  val of_float : float -> t

  val of_int : int -> t
end

module S : (Set.S with type elt = string)

val s_of_list : string list -> S.t

module M : (Map.S with type key = string)

val pp_list : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

type file_type = File | Directory

type path = string list
val path_to_string : path -> string
val string_to_path : string -> path

module Provider : sig
  type item = [
    | `File of string
    | `Dir of string
  ]

  type t = {
    name : string ;
    description : string ;
    file_type : path -> (file_type, string) result ;
    read : path -> (string, string) result ;
    write : path -> string -> (unit, string) result ;
    read_dir : path -> (item list, string) result ;
    exists : path -> bool ;
  }

  val pp_provider : Format.formatter -> t -> unit
end

type priv = [ `RSA_priv of string ]

type pub = [ `RSA_pub of string ]
val pub_equal : pub -> pub -> bool
val pp_pub : Format.formatter -> pub -> unit
val pubtype_to_string : pub -> string
val string_to_pubtype : string -> pub option

type name = string
val pp_name : Format.formatter -> name -> unit
val name_equal : name -> name -> bool

(* keyid (NOTE: maybe abstract, checking for printable ASCII characters, case-insensitive, make private!) *)
type identifier = string
val pp_id : Format.formatter -> identifier -> unit
val id_equal : identifier -> identifier -> bool

type digest = string
val pp_digest : Format.formatter -> digest -> unit

type resource = [
  | `PublicKey
  | `Team
  | `Checksums
  | `Releases
  | `Authorisation
  | `Index
]
val resource_to_string : resource -> string
val string_to_resource : string -> resource option
val pp_resource : Format.formatter -> resource -> unit
val resource_equal : resource -> resource -> bool

type verification_error = [
  | `InvalidBase64Encoding
  | `InvalidSignature
  | `InvalidPublicKey
  | `NoSignature
]

val pp_verification_error : Format.formatter -> verification_error -> unit

val (>>=) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val guard : bool -> 'a -> (unit, 'a) result
val foldM : ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result
