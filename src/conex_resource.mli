open Conex_result
open Conex_core

module Signature : sig
  (* a signature is a tuple *)
  type t = identifier * int64 * string

  val extend_data : string -> identifier -> int64 -> string

  val pp_signature : Format.formatter -> t -> unit
end

module Publickey : sig
  type t = private {
    counter : int64 ;
    version : int64 ;
    keyid : identifier ;
    key : pub option ;
  }

  val publickey : ?counter:int64 -> ?version:int64 -> identifier -> pub option -> t

  val pp_publickey : Format.formatter -> t -> unit
end

module Team : sig
  type t = private {
    counter : int64 ;
    version : int64 ;
    name : identifier ;
    members : S.t
  }

  val team : ?counter:int64 -> ?version:int64 -> ?members:S.t -> identifier -> t

  val add : t -> identifier -> t
  val remove : t -> identifier -> t

  val pp_team : Format.formatter -> t -> unit
end

module Authorisation : sig
  type t = private {
    counter : int64 ;
    version : int64 ;
    name : name ;
    authorised : S.t ;
  }

  val authorisation : ?counter:int64 -> ?version:int64 -> ?authorised:S.t -> name -> t

  val add : t -> identifier -> t
  val remove : t -> identifier -> t

  val pp_authorisation : Format.formatter -> t -> unit
  val pp_authorised : Format.formatter -> S.t -> unit
end

module Releases : sig
  type t = private {
    counter : int64 ;
    version : int64 ;
    name : name ;
    releases : S.t ;
  }

  val releases : ?counter:int64 -> ?version:int64 -> ?releases:S.t -> name -> (t, string) result

  val pp_releases : Format.formatter -> t -> unit
end

module Checksum : sig
  type c = {
    filename : name ;
    bytesize : int64 ;
    checksum : digest ;
  }

  val pp_checksum : Format.formatter -> c -> unit
  val checksum : string -> string -> c
  val checksum_equal : c -> c -> bool

  type checksum_map

  type t = private {
    counter : int64 ;
    version : int64 ;
    name : name ;
    files : checksum_map ;
  }

  val pp_checksums : Format.formatter -> t -> unit

  val checksums : ?counter:int64 -> ?version:int64 -> string -> c list -> t

  val set_counter : t -> int64 -> t

  val compare_checksums : t -> t ->
    (unit,
     [> `InvalidName of name * name
     | `ChecksumsDiff of name * name list * name list * (c * c) list ])
      result

  val fold : (c -> 'b -> 'b) -> checksum_map -> 'b -> 'b
  val find : checksum_map -> string -> c
end

module Index : sig
  type r = private {
    index : int64 ;
    name : string ;
    size : int64 ;
    resource : resource ;
    digest : digest ;
  }

  val r : int64 -> string -> int64 -> resource -> digest -> r

  val pp_resource : Format.formatter -> r -> unit

  type t = private {
    counter : int64 ;
    version : int64 ;
    identifier : identifier ;
    resources : r list ;
    signatures : Signature.t list ;
  }

  val index : ?counter:int64 -> ?version:int64 -> ?resources:(r list) -> ?signatures:(Signature.t list) -> identifier -> t

  val next_id : t -> int64

  val add_resource : t -> r -> t

  val add_resources : t -> r list -> t

  val pp_index : Format.formatter -> t -> unit

  val add_sig : t -> Signature.t -> t

  val replace_sig : t -> Signature.t -> t
end
