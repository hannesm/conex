open Core

val unique_keyid : identifier list -> identifier -> bool
val unique_data : string list -> string -> bool

val authorisation_of_item : string -> string

val private_keys : Provider.t -> identifier list
val keys : Provider.t -> identifier list
val authorisations : Provider.t -> string list
val items : Provider.t -> string -> string list

val private_key_path : path -> identifier -> path
val key_path : identifier -> path
val authorisation_path : string -> path
val checksum_path : string -> path

val checksum_files : Provider.t -> string -> path list

val is_key : path -> identifier option
val is_authorisation : path -> string option
val is_item : path -> (string * string) option
