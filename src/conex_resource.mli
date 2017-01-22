open Conex_result
open Conex_core

module Signature : sig
  (* a signature is a tuple *)
  type t = Uint.t * string

  val extend_data : string -> Uint.t -> string

  val pp_signature : Format.formatter -> t -> unit
end

module Team : sig
  type t = private {
    counter : Uint.t ;
    version : Uint.t ;
    name : identifier ;
    members : S.t
  }

  val team : ?counter:Uint.t -> ?version:Uint.t -> ?members:S.t -> identifier -> t

  val add : t -> identifier -> t
  val remove : t -> identifier -> t
  val prep : t -> t * bool

  val equal : t -> t -> bool

  val pp_team : Format.formatter -> t -> unit
end

module Authorisation : sig
  type t = private {
    counter : Uint.t ;
    version : Uint.t ;
    name : name ;
    authorised : S.t ;
  }

  val authorisation : ?counter:Uint.t -> ?version:Uint.t -> ?authorised:S.t -> name -> t

  val add : t -> identifier -> t
  val remove : t -> identifier -> t
  val prep : t -> t * bool

  val equal : t -> t -> bool

  val pp_authorisation : Format.formatter -> t -> unit
  val pp_authorised : Format.formatter -> S.t -> unit
end

module Releases : sig
  type t = private {
    counter : Uint.t ;
    version : Uint.t ;
    name : name ;
    releases : S.t ;
  }

  val releases : ?counter:Uint.t -> ?version:Uint.t -> ?releases:S.t -> name -> (t, string) result

  val add : t -> name -> t
  val remove : t -> name -> t
  val prep : t -> t * bool

  val equal : t -> t -> bool

  val pp_releases : Format.formatter -> t -> unit
end

module Checksum : sig
  type c = {
    filename : name ;
    bytesize : Uint.t ;
    checksum : digest ;
  }

  val pp_checksum : Format.formatter -> c -> unit
  val checksum : string -> string -> c
  val checksum_equal : c -> c -> bool

  type checksum_map

  type t = private {
    counter : Uint.t ;
    version : Uint.t ;
    name : name ;
    files : checksum_map ;
  }

  val pp_checksums : Format.formatter -> t -> unit

  val checksums : ?counter:Uint.t -> ?version:Uint.t -> string -> c list -> t

  val equal : t -> t -> bool

  val set_counter : t -> Uint.t -> t

  val compare_checksums : t -> t ->
    (unit,
     [> `InvalidName of name * name
     | `ChecksumsDiff of name * name list * name list * (c * c) list ])
      result

  val fold : (c -> 'b -> 'b) -> checksum_map -> 'b -> 'b
  val find : checksum_map -> string -> c

  val prep : t -> t * bool
end

module Index : sig
  type r = private {
    index : Uint.t ;
    rname : string ;
    size : Uint.t ;
    resource : resource ;
    digest : digest ;
  }

  val r : Uint.t -> string -> Uint.t -> resource -> digest -> r

  val r_equal : r -> r -> bool

  val pp_resource : Format.formatter -> r -> unit

  type email = identifier

  type service = [
    | `Email of email
    | `GitHub of identifier
    | `Other of identifier * string
  ]

  type t = private {
    accounts : service list ;
    keys : pub list ;
    counter : Uint.t ;
    version : Uint.t ;
    name : identifier ;
    resources : r list ;
    signatures : Signature.t list ;
    queued : r list ;
  }

  val pp_index : Format.formatter -> t -> unit

  val index : ?accounts:(service list) -> ?keys:(pub list) -> ?counter:Uint.t -> ?version:Uint.t -> ?resources:(r list) -> ?signatures:(Signature.t list) -> ?queued:(r list) -> identifier -> t

  val next_id : t -> Uint.t

  val add_resource : t -> r -> t

  val equal : t -> t -> bool

  val reset : t -> t

  val prep_sig : t -> t * bool

  val add_sig : t -> Signature.t -> t
end
