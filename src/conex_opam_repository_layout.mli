open Conex_utils
open Conex_resource
(** Opam repository layout

    The core of conex is agnostic towards the repository layout - it needs some
    functions to {!categorise} received patches into packages, authorisations,
    and identities.
*)

(** [valid_id id] returns [true] if [id] is valid: this calls
    {{!Conex_utils.String.is_ascii}String.is_ascii} [id]. *)
val valid_id : identifier -> bool

(** [valid_name name] returns [true] if [name] is valid: either it is '-' or '_'
    or {{!Conex_utils.String.is_ascii}String.is_ascii} [name]. *)
val valid_name : identifier -> bool

(** [authorisation_of_package n] returns either [Some n'], where [n'] is the
    prefix of [n] up until the first '.' (which separates package name from
    version suffix), or [None]. *)
val authorisation_of_package : name -> name option

(** [data_path] is the path from repository root to packages, "packages". *)
val data_path : path

(** [id_path] is the path where identities are stored, at the moment "id". *)
val id_path : path

(** [id_file id] is the path to the specific id (thus "id/foo" for identifier
    foo. *)
val id_file : identifier -> path

(** [authorisation_path name] is the full path where the "authorisation" file
    for [name] is stored.  Currently [data_dir; name; "authorisation"]. *)
val authorisation_path : name -> path

(** [package_path name] is the full path where the "releases" file for [name] is
    stored.  Currently [data_dir; name; "releases"]. *)
val package_path : name -> path

(** [checksums_filename] is "checksums". *)
val checksums_filename : name

(** [release_dir name] is [data_dir;] {!authorisation_of_package} (if [None],
    [name]), followed by [name]. *)
val release_dir : name -> path

(** [checksums_path name] is [release_dir @ checksums_filename]. *)
val checksums_path : name -> path

(** [categorise path] identifies which resource owns the given [path]. *)
val categorise : path -> [ `Id of identifier
                         | `Authorisation of identifier
                         | `Package of name
                         | `Release of name * name
                         | `Compiler of name * name
                         | `Unknown ]
