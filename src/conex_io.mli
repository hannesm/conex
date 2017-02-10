(** IO operations

    Conex relies on providers to read data from and write data to.  Each access
    consists of a {!path} used as key.  Only basic file types are supported (no
    symbolic links).
*)

open Conex_utils
open Conex_resource

(** {1 IO provider} *)

(** A provider contains its base directory, a description, and read/write/exist
    functionality.  TODO: define this as a module type. *)
type t = {
  basedir : string ;
  description : string ;
  file_type : path -> (file_type, string) result ;
  read : path -> (string, string) result ;
  write : path -> string -> (unit, string) result ;
  read_dir : path -> (item list, string) result ;
  exists : path -> bool ;
}

(** [pp t] is a pretty printer for [t]. *)
val pp : t fmt

(** {1 Listings} *)

(** [ids t] are all ids present on [t]. *)
val ids : t -> (S.t, string) result

(** [packages t] are all packages present on [t]. *)
val packages : t -> (S.t, string) result

(** [releases t name] are all releases of [name] on [t]. *)
val releases : t -> name -> (S.t, string) result

(* TODO: better name than computation! *)
(** {1 Resource computation} *)

(** The variant of errors when computing checksums *)
type cc_err = [ `FileNotFound of name | `NotADirectory of name ]

(** [pp_cc_err] is a pretty printer for {!cc_err}. *)
val pp_cc_err : cc_err fmt

(** [compute_release digestf t now name] computes the release by computing
    checksums of all files and directories of [name] using [digestf].  If
    release [name] is not found, an error is signalled. *)
val compute_release : (string -> Digest.t) -> t -> Uint.t -> name -> (Release.t, cc_err) result

(** [compute_package t create name] computes the package by listing all
    subdirectories of [name]. *)
val compute_package : t -> Uint.t -> name -> (Package.t, string) result


(** {1 Reading of resource files} *)

(** The variant of read and parse errors. *)
type r_err = [ `NotFound of typ * name | `ParseError of typ * name * string | `NameMismatch of typ * name * name ]

(** [pp_r_err] is a pretty printer for {!r_err}. *)
val pp_r_err : r_err fmt

(** [read_id t id] reads and parses the given identifier on [t]. *)
val read_id : t -> identifier ->
  ([ `Author of Author.t | `Team of Team.t ],
   [> r_err ]) result

(** [read_author t id] reads and parses the given author on [t]. *)
val read_author : t -> identifier -> (Author.t, [> r_err ]) result

(** [read_team t id] reads and parses the given team on [t]. *)
val read_team : t -> identifier -> (Team.t, [> r_err ]) result

(** [read_authorisation t name] reads and parses the authorisation for the given
    [name] on [t]. *)
val read_authorisation : t -> name -> (Authorisation.t, [> r_err ]) result

(** [read_package t name] reads and parses the package for the given
    [name] on [t]. *)
val read_package : t -> name -> (Package.t, [> r_err ]) result

(** [read_release t name.version] reads and parses the release for the given
    [name.version] on [t]. *)
val read_release : t -> name -> (Release.t, [> r_err ]) result

(** {1 Writing of resources to files} *)

(** [write_author t author] writes the given [author] on [t]. *)
val write_author : t -> Author.t -> (unit, string) result

(** [write_team t team] writes the given team on [t]. *)
val write_team : t -> Team.t -> (unit, string) result

(** [write_authorisation t a] writes the given authorisation on [t]. *)
val write_authorisation : t -> Authorisation.t -> (unit, string) result

(** [write_package t p] writes the given package on [t]. *)
val write_package : t -> Package.t -> (unit, string) result

(** [write_release t r] writes the given release on [t]. *)
val write_release : t -> Release.t -> (unit, string) result
