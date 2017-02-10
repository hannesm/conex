(** Unix IO provider *)

(** [fs_provider path] is a data provider backed by a file system or error. *)
val fs_provider : string -> (Conex_io.t, string) result

(** [fs_ro_provider path] is a read-only data provider backed by a file system
    or error. *)
val fs_ro_provider : string -> (Conex_io.t, string) result
