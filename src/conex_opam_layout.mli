open Conex_core

val unique_id : S.t -> identifier -> bool
val unique_data : S.t -> name -> bool

(* if we make the constructor private, no need for this anymore *)
val valid_id : identifier -> bool
val valid_name : identifier -> bool

val authorisation_of_item : name -> name option

val ids : Provider.t -> identifier list
val items : Provider.t -> name list
val subitems : Provider.t -> name -> name list

val id_path : identifier -> path
val authorisation_path : name -> path
val releases_path : name -> path
val checksum_path : name -> path
val checksum_dir : name -> path

val checksum_files : Provider.t -> string -> path list

val categorise : path -> [ `Id of identifier | `Authorisation of identifier | `Releases of name | `Package of name * name | `Compiler of name * name | `Unknown ]
