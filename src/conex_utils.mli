
val option : 'a -> ('b -> 'a) -> 'b option -> 'a

val filter_map : f:('a -> 'b option) -> 'a list -> 'b list

module String : sig
  type t = string

  val cuts : char -> t -> t list

  val cut : char -> t -> (t * t) option

  val is_prefix : prefix:t -> t -> bool

  val is_suffix : suffix:t -> t -> bool

  val cut_suffix : suffix:t -> t -> t

  val slice : ?start:int -> ?stop:int -> t -> t

  val to_lower : t -> t

  val is_ascii : ?p:(char -> bool) -> t -> bool

  val trim : t -> t

  val get : t -> int -> char

  val concat : t -> t list -> t

  val compare : t -> t -> int

  val length : t -> int

  val compare_insensitive : t -> t -> bool
end
