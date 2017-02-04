open Conex_result

(** Conex core definitions *)

(** {1 Unsigned integers} *)

(** 64 bit unsigned integer with explicit overflow behaviour (see
    {!Uint.succ}). *)
module Uint : sig

  (** A 64 bit unsigned integer (using a [int64]). *)
  type t

  (** [zero] is the smallest member. *)
  val zero : t

  (** [compare a b] is [res]: 0 if [a = b], -1 if [a < b], 1 if [a > b] *)
  val compare : t -> t -> int

  (** [succ t] is [carry, next]: if [carry] is true, an overflow happened.
      [next] is always the next integer in the [Z/2^64-1Z] group. *)
  val succ : t -> bool * t

  (** [to_string t] is [t] converted to a string in hexadecimal ([%LX]).  *)
  val to_string : t -> string

  (** [of_string s] attempts to parse the string [s] as hexadecimal encoded
      number using [Int64.of_string "0x" ^ s]. *)
  val of_string : string -> t option

  (** [of_float f] is [Int64.of_float f] if [f >= 0.0]. *)
  val of_float : float -> t option

  (** [of_int i] is [Int64.of_int i] if [i >= 0]. *)
  val of_int : int -> t option

  (** [of_int_exn i] is [Int64.of_int i] if [i >= 0], raises an exception on
      failure. *)
  val of_int_exn : int -> t
end


(** {1 Sets, Maps, Format utils} *)

(** [S] is a string set. *)
module S : (Set.S with type elt = string)

(** [s_of_list xs] transforms the string list [xs] to a set. *)
val s_of_list : string list -> S.t

(** [M] is a [Map] which keys are strings. *)
module M : (Map.S with type key = string)

(** ['a fmt] is the signature for pretty printers. *)
type 'a fmt = Format.formatter -> 'a -> unit

(** [pp_list pp] is a pretty printer for a list (surrounded by square brackets,
    elements are separated by semicolon).  The [pp] is be a pretty printer for
    list elements. *)
val pp_list : 'a fmt -> 'a list fmt


(** {1 Names and identifiers} *)

(** The name of resources, used e.g. for package names. *)
type name = string

(** [pp_name name] is a pretty printer for [name]. *)
val pp_name : name fmt

(** [name_equal a b] is the result of a case insensitive comparison of [a] and [b]. *)
val name_equal : name -> name -> bool

(** The type of identifiers. *)
type identifier = string

(** [pp_id id] is a pretty printer for [identifier]. *)
val pp_id : identifier fmt

(** [id_equal a b] is the result of a case insensitive comparison of [a] and [b]. *)
val id_equal : identifier -> identifier -> bool


(** {1 File system types} *)

(** The sum type of possible file types we expect *)
type file_type = File | Directory

(** A [path] is a list of strings *)
type path = string list

(** [path_to_string path] is [String.concat "/" path]. *)
val path_to_string : path -> string

(** [string_to_path str] is [String.cut "/" str]. *)
val string_to_path : string -> path


(** {1 Asymmetric key types} *)

(** The sum type of supported asymmetric key algorithms. *)
type keyalg = [ `RSA ]

(** [keyalg_to_string pub] is a string representing the key algorithm. *)
val keyalg_to_string : keyalg -> string

(** [string_to_keyalg str] is either [Some keyalg], or [None]. *)
val string_to_keyalg : string -> keyalg option

(** The type of private keys *)
type priv = keyalg * string

(** The type of public keys *)
type pub = keyalg * string

(** [pub_equal a b] is [true] if both public keys are the same. *)
val pub_equal : pub -> pub -> bool

(** [pp_pub] is a pretty printer for public keys *)
val pp_pub : pub fmt


(** {1 Cryptographic signatures} *)

(** The sum type of supported signature algorithms. *)
type sigalg = [ `RSA_PSS_SHA256 ]

(** [sigalg_to_string sig] is a string representing the signature algorithm. *)
val sigalg_to_string : sigalg -> string

(** [string_to_sigalg str] is either [Some sigalg] or [None]. *)
val string_to_sigalg : string -> sigalg option

(** The signature header, containing a [created] timestamp (seconds since Unix epoch, the signature algorithm, and the identifier. *)
type sig_hdr = {
  created : Uint.t ;
  sigalg : sigalg ;
  signame : identifier ;
}

(** [extend_sig hdr data] extends the given data with the header to a string which is then signed. *)
val extend_sig : sig_hdr -> string -> string

(** A signature is a pair of header and value. *)
type signature = sig_hdr * string

(** [pp_signature sig] is a pretty printer for a signature. *)
val pp_signature : signature fmt


(** {1 Digests} *)

(** The sum type of supported digest algorithms. *)
type digestalg = [ `SHA256 ]

(** [digestalg_to_string alg] is a string representing the digest algorithm. *)
val digestalg_to_string : digestalg -> string

(** [string_to_digestalg str] is either [Some digestalg] or [None]. *)
val string_to_digestalg : string -> digestalg option

(** A digest is a pair of digest algorithm and value. *)
type digest = digestalg * string

(** [digest_to_string digest] is a string representation of [digest]. *)
val digest_to_string : digest -> string

(** [pp_digest digest] is a pretty printer for a [digest]. *)
val pp_digest : digest fmt

(** [digest_eq a b] is [true] when [a] and [b] use the same algorithm type, and
    have the same value. *)
val digest_eq : digest -> digest -> bool


(** {1 Resources} *)

(** The sum type of all possible resources. *)
type resource = [
  | `PublicKey
  | `Team
  | `Checksums
  | `Releases
  | `Authorisation
  | `Index
]

(** [resource_to_string res] is the string representation of [res]. *)
val resource_to_string : resource -> string

(** [string_to_resource str] is either [Some resource] or [None]. *)
val string_to_resource : string -> resource option

(** [pp_resource pp] is a pretty printer for [resource]. *)
val pp_resource : resource fmt

(** [resource_equal a b] is [true] if they are the same, otherwise [false]. *)
val resource_equal : resource -> resource -> bool


(** {1 Verification errors} *)

(** Potential error case when verifying a signature *)
type verification_error = [
  | `InvalidBase64Encoding
  | `InvalidSignature
  | `InvalidPublicKey
  | `NoSignature
]

(** [pp_verification_error pp] is a pretty printer for [verification_error]. *)
val pp_verification_error : Format.formatter -> verification_error -> unit


(** {1 Result combinators} *)

(** [r >>= f] is [f a] unless [r] is an [Error], which is propagated.  Monadic bind. *)
val (>>=) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result

(** [guard pred err] is either [Ok ()] (if [pred] holds), [Error err] otherwise. *)
val guard : bool -> 'a -> (unit, 'a) result

(** [foldM f a xs] applies [f] to each element of [xs], returns either [Ok] and
    the produced value, or [Error]. *)
val foldM : ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result

(** [foldS f a s] applies [f] to each element of the set [s], returns either
    [Ok] and the produced value, or [Error]. *)
val foldS : ('a -> string -> ('a, 'c) result) -> 'a -> S.t -> ('a, 'c) result
