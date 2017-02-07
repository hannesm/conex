(** Persistent data: on wire and record types *)

(*
    Every resource in conex is a piece of data (or metadata), and has its own
    purpose.  Resources are persistent objects stored in the repository.  Conex'
    view on repository updates are resource modifications over time: addition of
    a package release, introduction of a new team, modification of dependency
    constraints of a certain package, key renewal by its author, authorising a
    team instead of an individual for a package, provisioning of a new
    timestamping service by a quorum of janitors, ...

    Initially, conex verifies the authenticity and freshness of a cloned
    repository (authenticity by using public key fingerprints verified over a
    trusted channel, and freshness by looking at the last signature of the
    timestamping service).

    Afterwards, it trusts the local repository and verifies resource updates.
    Not every update has to be verified individually (if a client is updating
    rarely, they don't verify intermediate states).  Both authenticity and
    monotonicity are preserved.  To minimise the amount of public key
    operations, instead of signing each resource with their private key, each
    author digitally signs their individual list of approved resources: their
    name, their type, and their digest.  Each author can approve multiple
    versions of a single resource (e.g. a team red with members Joseph and
    Vladimir in version 41, and also the same team red with Joseph, Vladimir,
    and in version 42).  The resource list is just a resource, which is modified
    only by the author who owns it (thus no merge conflicts from simultaneous
    edits by multiple people).

    Resources are stored in the same repository they should protect from
    malicious updates or downgrades.  Additionally, they have to protect
    themselves from attackers.

    Some resource updates need approval by a quorum of janitors, this is to
    avoid a centralised trust authority which is a single point of failure.  As
    long as fewer than a quorum of janitor private keys are compromised, the
    guarantees provided by conex are preserved.  Janitors themselves are a team,
    and part of the repository.  This allows dynamic updates of the trusted
    group.

    The possibility to have multiple versions of a resource approved is crucial
    for the update workflow: only after a quorum of janitors approved the update
    (each by adding it to their resource list), this resource update can be
    verified by conex (the merge of the resource update needs to wait until
    sufficient janitors approved it).

    Resources stored on disk consists of a common header: a name, a type, a
    counter, a wraparound counter, and a creation timestamp.  There are
    broadly three kinds of resources: those containing identities ({!Team} and
    {!Author}), those regulating access to packages ({!Authorisation}), and those
    with the digests of the opam repository data ({!Package} and {!Release}).
*)

open Conex_utils

(** {1 Names and identifiers} *)

(** The name of resources, used e.g. for package names. *)
type name = string

(** [pp_name name] is a pretty printer for [name]. *)
val pp_name : name fmt

(** [name_equal a b] is the result of a case insensitive comparison of [a] and [b]. *)
val name_equal : name -> name -> bool

(** The type of identifiers. *)
type identifier = string

(** [pp_id id] is a pretty printer for [identifier]. *)
val pp_id : identifier fmt

(** [id_equal a b] is the result of a case insensitive comparison of [a] and [b]. *)
val id_equal : identifier -> identifier -> bool



(** {1 Wire format} *)

(** The wire encoding is abstract here, one suitable decoding and encoding
    engine is {!Conex_opam_encoding}.  The wire encoding is used for digest
    computations, and persistent storage on disk. *)
module Wire : sig

  (** The values in the key value store: either a map, a list, a string, or an
      unsigned integer. *)
  type s =
    | Map of s M.t
    | List of s list
    | String of string
    | Int of Uint.t

  (** The toplevel node, a Map *)
  type t = s M.t

  val to_string : t -> string
end

(** {1 Resource types} *)

(** The sum type of all possible resources. *)
type typ = [
  | `Signature
  | `Key
  | `Account
  | `Author
  | `Wrap
  | `Team
  | `Authorisation
  | `Package
  | `Release
]

(** [resource_to_string res] is the string representation of [res]. *)
val typ_to_string : typ -> string

(** [string_to_resource str] is either [Some resource] or [None]. *)
val string_to_typ : string -> typ option

(** [pp_resource pp] is a pretty printer for [resource]. *)
val pp_typ : typ fmt

(** [resource_equal a b] is [true] if they are the same, otherwise [false]. *)
val typ_equal : typ -> typ -> bool

val typ_of_wire : Wire.s -> (typ, string) result

module Header : sig
  type t = {
    version : Uint.t ;
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : name ;
    typ : typ
  }

  val of_wire : Wire.t -> (t, string) result

  val timestamp : Uint.t -> string

  val counter : Uint.t -> Uint.t -> string

  val pp : t fmt

  val wire : t -> Wire.t
end


(** {1 Asymmetric key types} *)

module Key : sig

  (** The sum type of supported asymmetric key algorithms. *)
  type alg = [ `RSA ]

  (** [alg_to_string pub] is a string representing the key algorithm. *)
  val alg_to_string : alg -> string

  (** [string_to_alg str] is either [Some alg], or [None]. *)
  val string_to_alg : string -> alg option

  (** The type of private keys *)
  type priv = [ `Priv of alg * string * Uint.t ]

  (** The type of public keys *)
  type t = alg * string * Uint.t

  (** [equal a b] is [true] if both public keys are the same. *)
  val equal : t -> t -> bool

  (** [pp] is a pretty printer for public keys *)
  val pp : t fmt

  val of_wire : Wire.s -> (t, string) result

  val wire_raw : t -> Wire.s

  val wire : identifier -> t -> Wire.t
end

(** {1 Cryptographic signatures} *)

module Signature : sig
  (** The sum type of supported signature algorithms. *)
  type alg = [ `RSA_PSS_SHA256 ]

  (** [alg_to_string sig] is a string representing the signature algorithm. *)
  val alg_to_string : alg -> string

  (** [string_to_alg str] is either [Some alg] or [None]. *)
  val string_to_alg : string -> alg option

  (** The signature header, an algorithm and a created timestamp. *)
  type hdr = alg * Uint.t

  (** [wire id hdr data] extends the given data with the header and id to a string which is then signed. *)
  val wire : identifier -> hdr -> string -> Wire.t

  (** A signature is a pair of header and value. *)
  type t = hdr * string

  (** [pp sig] is a pretty printer for a signature. *)
  val pp : t fmt

  val of_wire : Wire.s -> (t, string) result

  val wire_raw : t -> Wire.s
end

(** {1 Digests} *)

module Digest : sig

  (** The sum type of supported digest algorithms. *)
  type alg = [ `SHA256 ]

  (** [alg_to_string alg] is a string representing the digest algorithm. *)
  val alg_to_string : alg -> string

  (** [string_to_alg str] is either [Some alg] or [None]. *)
  val string_to_alg : string -> alg option

  (** A digest is a pair of digest algorithm and value. *)
  type t = alg * string

  (** [to_string digest] is a string representation of [digest]. *)
  val to_string : t -> string

  (** [pp digest] is a pretty printer for [digest]. *)
  val pp : t fmt

  (** [equal a b] is [true] when [a] and [b] use the same algorithm type, and
      have the same value. *)
  val equal : t -> t -> bool

  val of_wire : Wire.s -> (t, string) result

  val wire_raw : t -> Wire.s
end

module Author : sig
  type r = private {
    index : Uint.t ;
    rname : string ;
    size : Uint.t ;
    rtyp : typ ;
    digest : Digest.t ;
  }

  val r : Uint.t -> string -> Uint.t -> typ -> Digest.t -> r

  val r_equal : r -> r -> bool

  val pp_resource : Format.formatter -> r -> unit

  type email = identifier

  type service = [
    | `Email of email
    | `GitHub of identifier
    | `Other of identifier * string
  ]

  type t = private {
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : identifier ;
    accounts : service list ;
    keys : Key.t list ;
    resources : r list ;
    signatures : Signature.t list ;
    queued : r list ;
  }

  val pp : t fmt

  val t : ?counter:Uint.t -> ?wraps:Uint.t -> ?accounts:(service list) -> ?keys:(Key.t list) -> ?resources:(r list) -> ?signatures:(Signature.t list) -> ?queued:(r list) -> Uint.t -> identifier -> t

  val of_wire : Wire.t -> (t, string) result

  (* resources only *)
  val wire_raw : t -> Wire.t
  val wire : t -> Wire.t

  val next_id : t -> Uint.t

  val add_resource : t -> r -> t

  val equal : t -> t -> bool

  val reset : t -> t

  val prep_sig : t -> t * bool

  val add_sig : t -> Signature.t -> t
end


(** {1 Team} *)

(** A team consists of a group of authors, and teams can be authorised for
     packages.  Team members can dynamically join and leave.  All
     modifications to teams require a quorum of janitors.  *)
module Team : sig
  type t = private {
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : identifier ;
    members : S.t
  }

  val t : ?counter:Uint.t -> ?wraps:Uint.t -> ?members:S.t -> Uint.t -> identifier -> t

  val add : t -> identifier -> t
  val remove : t -> identifier -> t
  val prep : t -> t * bool

  val equal : t -> t -> bool

  val of_wire : Wire.t -> (t, string) result
  val wire : t -> Wire.t

  val pp : t fmt
end

module Authorisation : sig
  type t = private {
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : name ;
    authorised : S.t ;
  }

  val t : ?counter:Uint.t -> ?wraps:Uint.t -> ?authorised:S.t -> Uint.t -> name -> t

  val add : t -> identifier -> t
  val remove : t -> identifier -> t
  val prep : t -> t * bool

  val equal : t -> t -> bool

  val of_wire : Wire.t -> (t, string) result
  val wire : t -> Wire.t

  val pp : Format.formatter -> t -> unit
end

module Package : sig
  type t = private {
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : name ;
    releases : S.t ;
  }

  val t : ?counter:Uint.t -> ?wraps:Uint.t -> ?releases:S.t -> Uint.t -> name -> t

  val add : t -> name -> t
  val remove : t -> name -> t
  val prep : t -> t * bool

  val equal : t -> t -> bool

  val of_wire : Wire.t -> (t, string) result
  val wire : t -> Wire.t

  val pp : Format.formatter -> t -> unit
end

module Release : sig
  type c = {
    filename : name ;
    size     : Uint.t ;
    digest   : Digest.t ;
  }

  val pp_checksum : c fmt
  val checksum_equal : c -> c -> bool

  type checksum_map

  type t = private {
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : name ;
    files : checksum_map ;
  }

  val pp : Format.formatter -> t -> unit

  val t : ?counter:Uint.t -> ?wraps:Uint.t -> Uint.t -> string -> c list -> t

  val of_wire : Wire.t -> (t, string) result
  val wire : t -> Wire.t

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
