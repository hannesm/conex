(** File system operations using Unix *)

open Conex_utils

(** [exists] is [Sys.exists]. *)
val exists : string -> bool

(** [mkdir ~mode name] creates a directory [name], or errors. *)
val mkdir : ?mode:int -> string -> (unit, string) result

(** [remove] is [Sys.remove] or error. *)
val remove : string -> (unit, string) result

(** [rename] is [Sys.rename] or error. *)
val rename : string -> string -> (unit, string) result

(** [file_type path] is the file type of [path] or error. *)
val file_type : string -> (file_type, string) result

(** [read_file path] is the contents of [path] or error. *)
val read_file : string -> (string, string) result

(** [write_file ~mode path data] writes [data] under [path] or error. *)
val write_file : ?mode:int -> string -> string -> (unit, string) result

(** [write_replace ~mode path data] writes [data] under [path.tmp] and
    renames [path.tmp] to [path] or error. *)
val write_replace : ?mode:int -> string -> string -> (unit, string) result

(** [collect_dir path] are the inhabitants of [path] or error. *)
val collect_dir : string -> (string list, string) result
