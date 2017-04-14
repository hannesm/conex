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

(** [compute_checksums digestf t now name] computes the release by computing
    checksums of all files and directories of [name] using [digestf].  If
    release [name] is not found, an error is signalled. *)
val compute_checksums : (string -> Digest.t) -> t -> Uint.t -> name -> (Checksums.t, cc_err) result

(** [compute_releases t create name] computes the package by listing all
    subdirectories of [name]. *)
val compute_releases : t -> Uint.t -> name -> (Releases.t, string) result


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

(** [read_releases t name] reads and parses the package for the given
    [name] on [t]. *)
val read_releases : t -> name -> (Releases.t, [> r_err ]) result

(** [read_checksums t name.version] reads and parses the release for the given
    [name.version] on [t]. *)
val read_checksums : t -> name -> (Checksums.t, [> r_err ]) result

(** {1 Writing of resources to files} *)

(** [write_author t author] writes the given [author] on [t]. *)
val write_author : t -> Author.t -> (unit, string) result

(** [write_team t team] writes the given team on [t]. *)
val write_team : t -> Team.t -> (unit, string) result

(** [write_authorisation t a] writes the given authorisation on [t]. *)
val write_authorisation : t -> Authorisation.t -> (unit, string) result

(** [write_releases t p] writes the given releases on [t]. *)
val write_releases : t -> Releases.t -> (unit, string) result

(** [write_checksums t r] writes the given checksums on [t]. *)
val write_checksums : t -> Checksums.t -> (unit, string) result
