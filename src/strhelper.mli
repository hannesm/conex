
val cuts : char -> string -> string list

val cut : char -> string -> (string * string) option

val is_prefix : prefix:string -> string -> bool

val is_suffix : suffix:string -> string -> bool

val cut_suffix : suffix:string -> string -> string

val slice : ?start:int -> ?stop:int -> string -> string

val lowercase_string : string -> string

val is_ascii : ?p:(char -> bool) -> string -> bool
