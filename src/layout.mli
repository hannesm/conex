open Core

val unique_keyid : S.t -> identifier -> bool
val unique_data : S.t -> name -> bool

(* if we make the constructor private, no need for this anymore *)
val valid_keyid : identifier -> bool
val valid_name : identifier -> bool

val authorisation_of_item : name -> name option

val private_keys : Provider.t -> identifier list
val ids : Provider.t -> identifier list
val authorisations : Provider.t -> name list
val items : Provider.t -> name -> name list

val private_key_path : path -> identifier -> path
val key_path : identifier -> path
val index_path : identifier -> path
val authorisation_path : name -> path
val releases_path : name -> path
val checksum_path : name -> path
val checksum_dir : name -> path

val checksum_files : Provider.t -> string -> path list

val is_index : path -> identifier option
val is_key : path -> identifier option
val is_authorisation : path -> name option
val is_item : path -> (name * name) option
val is_old_item : path -> (name * name) option
val is_compiler : path -> (name * name) option
