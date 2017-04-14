(** Persistent data: on wire and record types

    Every resource in conex is a piece of data (or metadata), and has its own
    purpose.  Resources stored on disk consists of a common header: a name, a
    type, a counter, an epoch, and a creation timestamp.  There are
    broadly three kinds of resources: those containing identities ({!Team} and
    {!Author}), those regulating access to packages ({!Authorisation}), and
    those with the digests of the opam repository data ({!Releases} and
    {!Release}).
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

  (** The values in the key value store: either a map, a list, an identifier,
      data (represented as string), or an unsigned integer. *)
  type s =
    | Map of s M.t
    | List of s list
    | Identifier of identifier
    | Data of string
    | Int of Uint.t

  (** The toplevel node, a Map *)
  type t = s M.t

  (** [to_string t] is a string representing [t].  This is used by
      {!Conex_crypto} to compute digests and signatures.  There is no parser for
      this string encoding available. *)
  val to_string : t -> string
end

(** {1 Resource types} *)

(** The sum type of all possible resources. *)
type typ = [
  | `Signature
  | `Key
  | `Account
  | `Author
  | `Epoch
  | `Team
  | `Authorisation
  | `Releases
  | `Checksums
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


(** Common header on disk *)
module Header : sig

  (** The header consists of version, created, counter, epoch, name, and typ. *)
  type t = {
    version : Uint.t ;
    created : Uint.t ;
    counter : Uint.t ;
    epoch : Uint.t ;
    name : name ;
    typ : typ
  }

  (** [pp] is a pretty printer. *)
  val pp : t fmt

  (** [wire t] is the wire representation of [t]. *)
  val wire : t -> Wire.t

  (** [of_wire t] converts [t] into a {!Header.t} or error. *)
  val of_wire : Wire.t -> (t, string) result

  (** [timestamp uint] prints [uint] to a string as a timestamp (decimal as
      seconds since UNIX epoch). *)
  val timestamp : Uint.t -> string

  (** [counter ctr epoch] prints [ctr, epoch] to a string containing the counter
      and epoch (unless zero). *)
  val counter : Uint.t -> Uint.t -> string
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

  (** [of_wire w] converts [w] to a key or error. *)
  val of_wire : Wire.s -> (t, string) result

  (** [wire_raw t] is the raw wire representation of [t] used by
      {!Author.wire}. *)
  val wire_raw : t -> Wire.s

  (** [wire id key] is [w], the wire representation used for approving [key]. *)
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

  (** [wire id hdr data] extends the given data with the header and id to a
      string which is then signed. *)
  val wire : identifier -> hdr -> string -> Wire.t

  (** A signature is a pair of header and value. *)
  type t = hdr * string

  (** [pp sig] is a pretty printer for a signature. *)
  val pp : t fmt

  (** [of_wire w] converts [w] to a signature or error. *)
  val of_wire : Wire.s -> (t, string) result

  (** [wire_raw t] is the wire representation of [t], used by {!Author.wire}. *)
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

  (** [of_wire w] converts [w] to a digest or error. *)
  val of_wire : Wire.s -> (t, string) result

  (** [wire_raw t] is the wire representation of [t], used by {!Author.wire} and
      {!Release.wire}. *)
  val wire_raw : t -> Wire.s
end

(** {1 Author} *)

(** An author contains a list of approved resources (name, typ, digest).  It
    also contains a list of key and signature pairs, and a list of accounts.
    Keys have to be approved by a quorum of janitors, but the resource list is
    modified only by the author themselves. *)
module Author : sig

  (** {2 Resources} *)

  (** The type of a resource in the approved list: a counter (the index), a
      name, a typ, and its digest. *)
  type r = private {
    index : Uint.t ;
    rname : string ;
    rtyp : typ ;
    digest : Digest.t ;
  }

  (** [pp_r] is a pretty printer. *)
  val pp_r : r fmt

  (** [r idx name typ dgst] is a constructor. *)
  val r : Uint.t -> string -> typ -> Digest.t -> r

  (** [r_equal r r'] is [true] is the name, type, and digest of [r] and [r'] are
      equal. *)
  val r_equal : r -> r -> bool

  (** {2 Accounts} *)

  (** Variant of accounts *)
  type account = [
    | `Email of identifier
    | `GitHub of identifier
    | `Other of identifier * string
  ]

  (** [compare_account a b] compares accounts [a] and [b]. *)
  val compare_account : account -> account -> int

  (** [pp_account a] is a pretty printer. *)
  val pp_account : account fmt

  (** [wire_account id a] is the wire representation of [a]. *)
  val wire_account : identifier -> account -> Wire.t

  (** The record of an author: name, key/signature pairs, created, counter,
      approved and queued resource lists. *)
  type t = private {
    (* signed part *)
    created : Uint.t ;
    counter : Uint.t ;
    epoch : Uint.t ;
    name : identifier ;
    resources : r list ;
    (* unsigned part *)
    accounts : account list ;
    keys : (Key.t * Signature.t) list ;
    queued : r list ;
  }

  (** [pp] is a pretty printer. *)
  val pp : t fmt

  (** [t ~counter ~epoch ~accounts ~keys ~resources ~queued created name] is a
      constructor. *)
  val t : ?counter:Uint.t -> ?epoch:Uint.t ->
    ?accounts:(account list) ->
    ?keys:((Key.t * Signature.t) list) ->
    ?resources:(r list) ->
    ?queued:(r list) ->
    Uint.t -> identifier -> t

  (** [of_wire w] converts [w] to an author or error. *)
  val of_wire : Wire.t -> (t, string) result

  (** [wire_raw t] is the raw wire representation of [t], including only header
      and resource list.  This is used for signing.  *)
  val wire_raw : t -> Wire.t

  (** [wire t] is the raw wire representation of [t], as written to disk. *)
  val wire : t -> Wire.t

  (** [contains ~queued author resource] is [true] if [resource] is in
      [author.resources] (or [author.queued] if [queued] is true (default:
      false). *)
  val contains : ?queued:bool -> t -> r -> bool

  (** [next_id t] is the next free identitifer of the resource list index. *)
  val next_id : t -> Uint.t

  (** [queue t r] adds [r] to [t.queued]. *)
  val queue : t -> r -> t

  (** [approve t r] adds [r] to [t.resources], and removes [r] from
      [t.queued]. *)
  val approve : t -> r -> t

  (** [equal t t'] is true if name, keys, accounts, resource lists, and queued
      are equal. *)
  val equal : t -> t -> bool

  (** [reset t] drops [t.queued]. *)
  val reset : t -> t

  (** [prep_sig t] increments [t.counter].  Returns the carry bit as second
      component. *)
  val prep_sig : t -> t * bool

  (** [replace_sig t (k, s)] adds [k,s] to [t.keys], filtering existing pairs
      where the same public key is used. *)
  val replace_sig : t -> Key.t * Signature.t -> t
end


(** {1 Team} *)

(** A team consists of a group of authors.  Team members can dynamically join
     and leave. *)
module Team : sig

  (** The record for a team: a header and a set of members. *)
  type t = private {
    created : Uint.t ;
    counter : Uint.t ;
    epoch : Uint.t ;
    name : identifier ;
    members : S.t
  }

  (** [pp] is a pretty printer. *)
  val pp : t fmt

  (** [t ~counter ~epoch ~members created id] is a constructor for a team. *)
  val t : ?counter:Uint.t -> ?epoch:Uint.t ->
    ?members:S.t -> Uint.t -> identifier -> t

  (** [equal t t'] is true if the set of members is equal and the name is
      equal. *)
  val equal : t -> t -> bool

  (** [of_wire w] converts [w] to a team or error. *)
  val of_wire : Wire.t -> (t, string) result

  (** [wire t] is the wire representation of [t]. *)
  val wire : t -> Wire.t

  (** [add t id] adds [id] to team. *)
  val add : t -> identifier -> t

  (** [remove t id] removes [id] from team. *)
  val remove : t -> identifier -> t

  (** [prep t] prepares increments [t.counter].  Returns the carry bit as second
      component.  Used after a batch of changes. *)
  val prep : t -> t * bool
end

(** {1 Authorisation} *)

(** An authorisation contains the information who is authorised to modify a
    package.  There is always a single authorisation file per package, approved
    by a quorum of janitors. *)
module Authorisation : sig

  (** The authorisation record: a header, a name, and a set of authorised ids. *)
  type t = private {
    created : Uint.t ;
    counter : Uint.t ;
    epoch : Uint.t ;
    name : name ;
    authorised : S.t ;
  }

  (** [pp] is a pretty printer. *)
  val pp : t fmt

  (** [t ~counter ~epoch ~authorised created name] is a constructor. *)
  val t : ?counter:Uint.t -> ?epoch:Uint.t ->
    ?authorised:S.t -> Uint.t -> name -> t

  (** [equal t t'] is true if the names are equal and the set of authorised ids
      are equal. *)
  val equal : t -> t -> bool

  (** [of_wire w] converts [w] to an authorisation or error. *)
  val of_wire : Wire.t -> (t, string) result

  (** [wire t] is the wire representation of [t], written to disk. *)
  val wire : t -> Wire.t

  (** [add t id] adds [id] to [t.authorised]. *)
  val add : t -> identifier -> t

  (** [remove t id] removed [id] from [t.authorised]. *)
  val remove : t -> identifier -> t

  (** [prep t] increments [t.counter], returns the carry bit as second
      component. *)
  val prep : t -> t * bool
end

(** {1 Releases} *)

(** A releases lists all released versions of a given package.  There is one
    releases resource for each package. *)
module Releases : sig

  (** The record for a releases: a header, and a set of version names. *)
  type t = private {
    created : Uint.t ;
    counter : Uint.t ;
    epoch : Uint.t ;
    name : name ;
    versions : S.t ;
  }

  (** [pp] is a pretty printer. *)
  val pp : t fmt

  (** [t ~counter ~epoch ~releases created name] is a constructor. *)
  val t : ?counter:Uint.t -> ?epoch:Uint.t ->
    ?versions:S.t -> Uint.t -> name -> t

  (** [equal t t'] is true if the names are equal and the set of versions. *)
  val equal : t -> t -> bool

  (** [of_wire w] converts [w] to a releases or error. *)
  val of_wire : Wire.t -> (t, string) result

  (** [wire t] is the wire representation of [t], as stored on disk. *)
  val wire : t -> Wire.t

  (** [add t name] adds [name] to [t.versions]. *)
  val add : t -> name -> t

  (** [remove t name] removes [name] from [t.versions]. *)
  val remove : t -> name -> t

  (** [prep t] increments [t.counter], the carry bit is returned as second
      component. *)
  val prep : t -> t * bool
end

(** {1 Checksums} *)

(** A checksums contains a map of all files in the repository relevant for this
    package version and their digests. *)
module Checksums : sig

  (** The record for a checksum: filename and digest. *)
  type c = {
    filename : name ;
    digest   : Digest.t ;
  }

  (** [pp_c] is a pretty printer. *)
  val pp_c : c fmt

  (** [c_equal c c'] is true if the filenames are equal, and the digests are
      equal. *)
  val checksum_equal : c -> c -> bool

  (** Type of a checksum map. *)
  type checksum_map

  (** The record of checksums: a header, and a checksum map. *)
  type t = private {
    created : Uint.t ;
    counter : Uint.t ;
    epoch : Uint.t ;
    name : name ;
    files : checksum_map ;
  }

  (** [pp] is a pretty printer. *)
  val pp : t fmt

  (** [t ~counter ~epoch created name checksums] is a constructor. *)
  val t : ?counter:Uint.t -> ?epoch:Uint.t -> Uint.t -> string -> c list -> t

  (** [of_wire w] converts [w] to a checksums or error. *)
  val of_wire : Wire.t -> (t, string) result

  (** [wire t] is the wire representation of [t]. *)
  val wire : t -> Wire.t

  (** [equal t t'] is true if the name is equal and the checksum maps are
      equal. *)
  val equal : t -> t -> bool

  (** [set_counter t ctr] sets [t.counter] to [ctr]. *)
  val set_counter : t -> Uint.t -> t

  (** [compare_t t t'] compares [t] and [t'], either they are equal, their names
      are different, or their checksum maps differ. *)
  val compare_t : t -> t ->
    (unit,
     [> `InvalidName of name * name
     | `ChecksumsDiff of name * name list * name list * (c * c) list ])
      result

  (** [fold f m acc] folds [f] over [m]. *)
  val fold : (c -> 'b -> 'b) -> checksum_map -> 'b -> 'b

  (** [find m k] looks [k] up in [m]. *)
  val find : checksum_map -> string -> c

  (** [prep t] increments [t.counter], returns carry as second component. *)
  val prep : t -> t * bool
end
