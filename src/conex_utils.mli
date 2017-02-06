
(** {1 Utility functions for Conex} *)

(** [filter_map f xs] is [xs'], a list which contains all elements where [f]
    resulted in [Some _]. *)
val filter_map : f:('a -> 'b option) -> 'a list -> 'b list

(** {1 String module} *)

(** Some useful [String] utilities implemented here to avoid external
    dependencies.  This is a subset of
    {{:http://erratique.ch/software/astring}Astring}. *)
module String : sig

  (** Our string type is an OCaml [string] *)
  type t = string

  (** [cuts sep str] separates [str] into multiple substrings, stripping out the
      separating character [sep].  [String.concat "/" (String.cuts '/' xs)] is
      not the identity since empty substrings are stripped. *)
  val cuts : char -> t -> t list

  (** [cut sep str] cuts the string [str] at the first occurence of [sep] into
      its left and right parts.  If [sep] is not found, [None] is returned.
      If [Some (a, b)] is returned, [a ^ sep ^ b] is equal to [str]. *)
  val cut : char -> t -> (t * t) option

  (** [is_prefix ~prefix str] is [true] if [str] begins with [prefix], [false]
      otherwise. *)
  val is_prefix : prefix:t -> t -> bool

  (** [is_suffix ~suffix str] is [true] if [str] ends with [suffix], [false]
      otherwise. *)
  val is_suffix : suffix:t -> t -> bool

  (** [slice ~start ~stop str] slices [str] into a smaller piece, starting at
      offset [start] (default 0), ending at [stop] (default [String.length]). *)
  val slice : ?start:int -> ?stop:int -> t -> t

  (** [to_lower str] converts all printable ASCII characters to lowercase. *)
  val to_lower : t -> t

  (** [is_ascii ~p str] is [true] if all characters in [str] are ASCII
      characters: 0-9, a-z, A-Z OR satisfy [p].  Otherwise [false]. *)
  val is_ascii : ?p:(char -> bool) -> t -> bool

  (** [trim str] removes leading and trailing whitespaces of [str]. *)
  val trim : t -> t

  (** [get str offset] retrieves the character at [offset] in [str]. *)
  val get : t -> int -> char

  (** [concat sep xs] concatenates all [xs], using [sep] as separator. *)
  val concat : t -> t list -> t

  (** [compare a b] compares [a] with [b] using [String.compare]. *)
  val compare : t -> t -> int

  (** [length str] is the byte length of [str]. *)
  val length : t -> int

  (** [compare_insensitive a b] first converts [a] and [b] to lowercase strings,
      then uses [compare]. *)
  val compare_insensitive : t -> t -> bool
end

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

  (** [decimal t] is [t] converted to a string in decimal ([%Lu]).  *)
  val decimal : t -> string

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
