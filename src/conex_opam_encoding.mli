(** Opam file encoding

    Persistent files of the opam repository should be kept in the same file
    format as opam files.  This module uses the opam-file-format package to
    decode and encode the {{!Conex_resource.Wire.t}Wire.t} representation into
    opam files.
*)

(** [decode str] is either [Ok t] or [Error str], the input is a string in opam
    format. *)
val decode : string -> (Conex_resource.Wire.t, string) result

(** [encode t] encodes [t] into a string in opam format. *)
val encode : Conex_resource.Wire.t -> string
