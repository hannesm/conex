
(** {1 Utility functions for Conex} *)

(** [option none f r] is [none] if [r] is [None], [f r] otherwise. *)
val option : 'a -> ('b -> 'a) -> 'b option -> 'a

(** [filter_map f xs] is [xs'], a list which contains all elements where [f]
    resulted in [Some _]. *)
val filter_map : f:('a -> 'b option) -> 'a list -> 'b list

(** {1 String module} *)

(** Some useful [String] utilities, a subset of
    {{:http://erratique.ch/software/astring}, Astring} - implemented here to
    avoid dependencies. *)
module String : sig

  (** Our string type is an OCaml [string] *)
  type t = string

  (** [cuts sep str] separates [str] into multiple substrings, stripping out the
      separating character [sep].  [String.concat "/" (String.split '/' xs)] is
      not the identity since empty substrings are stripped ([String.split '/'
      "foo//bar/baz"] is [ "foo" ; "bar" ; "baz" ]. *)
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
