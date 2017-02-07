(** Establishing trust into community repositories

    A community repository is a map where a package name is the key and its
    value are the releases of the package.  The goal of [conex] is that
    legitimate package authors can publish new releases and modify metadata.
    This requires the information who is authorised for a package, either an
    individual author or a team, which can change over time.  The authorisation
    information, as well as public keys of individuals, is approved by a quorum
    of janitors, a team whose members keep the repository clean.

    The repository itself is a directory tree, initially verified using a set of
    janitor public key fingerprints, and afterwards updated using patches.  A
    repository on its own can be verified, but also a repository on disk, and a
    patch.  The latter preserves monotonicity of the repository (otherwise, an
    attacker could present you a rolled back version of the repository).  A
    client uses conex when updating their repository to ensure that all updates
    were signed by the package author or a quorum of janitors.

    Together with a timestamping service, which updates its repository
    regularly, verifies all patches, and signs the (global) repository state,
    rollback, freeze, and mix-and-match attacks are avoided.  This timestamping
    service is not yet finished, but will be a MirageOS unikernel (to minimise
    the trusted computing base).

    This library was developed with bootstrapping in mind: [conex_verify], the
    command line tool clients will use, does only depend on [opam-file-format]
    and [cmdliner] - both of which is already required by opam.

    [Conex] is functorised over {{!Conex_utils.LOGS}logging},
    {{!Conex_crypto}crypto}, and soon {{!Conex_io}IO} to allow its usage
    in different environments (e.g. [openssl] as crypto provider, no [Unix] file
    system interaction at all).

    The high level API in this module is subject to change.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

open Conex_utils
open Conex_resource
open Conex_crypto

module Make (L : LOGS) (C : VERIFY): sig

  (** [load_id io repo id] reads [id], either a team or an individual author,
      in [repo].  and verifies the read data. *)
  val load_id : Conex_io.t -> Conex_repository.t -> identifier ->
    (Conex_repository.t, string) result

  (** [load_ids ~ids io repo] folds over [ids] (default: all
      ids present in [io]), applying {!load_id}. *)
  val load_ids : ?ids:S.t -> Conex_io.t -> Conex_repository.t ->
    (Conex_repository.t, string) result

  (** [load_janitors ~valid io repo] reads the team janitors from [io], and all
      team members.  It verifies the data by doing two steps: first it uses only
      those public keys which fingerprints are provided via [valid].  If
      successful, it uses in a second step the resulting repository to verify
      the remaining janitors, followed by verification of the janitor team. *)
  val load_janitors : ?valid:(identifier -> string -> bool) ->
    Conex_io.t -> Conex_repository.t ->
    (Conex_repository.t, string) result

  (** [verify_item ~authorised ~release io repo name] reads and verifies the
      authorisation of the package [name].  All [ids] who are authorised are
      loaded into the repository (using {!load_ids}).  If [authorised] is
      [true], the package data for [name] is read and verified, followed by all
      releases (filtered with [release]).  Each
      release is read and verified. *)
  val verify_item :
    ?authorised:(S.t -> bool) -> ?release:(name -> bool) ->
    Conex_io.t -> Conex_repository.t -> name ->
    (Conex_repository.t, string) result

  (** [verify_diff io repository patch] first parses the [patch] and applies it.
      The janitors of the old [repository] are loaded and their public key
      fingerprints are used to validate the janitors team of the repository with
      applied [patch].  All identifiers and packages of the repository with
      [patch] applied are verified, and additionally all modified resources have
      their monotonicity verified. *)
  val verify_diff : Conex_io.t -> Conex_repository.t -> string ->
    (Conex_repository.t, string) result
end
