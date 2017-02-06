open Conex_core
open Conex_utils
open Conex_resource

val unique_id : S.t -> identifier -> bool
val unique_data : S.t -> name -> bool

val valid_id : identifier -> bool
val valid_name : identifier -> bool

val authorisation_of_item : name -> name option

val data_path : path
val id_path : path
val id_file : identifier -> path
val authorisation_path : name -> path
val package_path : name -> path
val release_filename : name
val release_path : name -> path
val release_dir : name -> path

val categorise : path -> [ `Id of identifier | `Authorisation of identifier | `Package of name | `Release of name * name | `Compiler of name * name | `Unknown ]
