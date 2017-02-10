(** Establish trust into community repositories

    A community repository gathers data from authors to provide an index
    including cross-references.

    Software package repositories contain metadata about libraries: license,
    author, dependencies, build instructions, url and checksum of the release
    tarball.  Someone publishes a library, and it gets integrated as long as the
    package name is not yet used and approved by those keeping the repository
    tidy.  Sometimes it is necessary to include small patch files in the
    repository, e.g. to get a library to compile with a new compiler release, or
    on non-mainstream operating system.  Library build instructions often
    include shell scripts, source code can execute arbitrary code, modifying
    version constraints in dependency may lead to compiling code with known
    security vulnerabilities.

    The transport layer to the repository is usually secured using TLS, or SSH
    and git.  But what about the repository itself?  Can we ensure that it
    serves the library metadata as intended by the authors?  Or do we have to
    trust the administrators of the repository server to ensure its integrity?
    Again, these are people who can be malicious, blackmailed, forced by the
    government, or have other evil intentions.

    [Conex] ensures that each library release has been approved by its author.
    Furthermore, once a library release is known to the client, this information
    cannot be rolled back by an attacker. A timestamping service periodically
    approves a global view of all libraries in the repository, and thus
    mitigates mix-and-match attacks where someone malicious prevents a client
    from getting updates for one library while retrieving other updates.

    The trust is rooted in digital signatures by library authors.  The server
    which hosts the repository does not need to be trusted.  Neither does the
    host serving release tarballs.

    Someone needs to vouch for an author: the private part of a public key
    stored in the repository should be known to the real author, and solely to
    that person.  If a single entity would be in control to vouch for authors,
    this entity would need to fully trusted by all clients.  Instead, a quorum
    of a janitor team is used in conex.  Janitors can together vouch for a
    public key of an author, extend and shrink teams, authorise and deauthorise
    an author or team for a package, and even checksums of packages and releases
    - useful for hot fixing when a core package is updated, and all reverse
    dependencies need adjustments.

    Conex adds metadata to a data repository (viewed as a map where package name
    is the domain and releases thereof the codomain) to ensure integrity and
    authenticity.  Different resource types are added:
    {ul
    {- Authors, consisting of a unique identifier, public key(s), accounts.}
    {- Teams, sharing the same namespace as authors, containing a set of
       members.}
    {- Authorisation, for each package, describing which identities are
       authorised for the package.}
    {- Package, for each package, listing all releases.}
    {- Release, for each release, listing checksums of all data files.}}

    Modifications to identities and authorisations need to be approved by a
    quorum of janitors, package and release files can be modified either by an
    authorised author or by a quorum of janitors.

    Conex initially ensures that the repository is properly signed, and updates
    to the repository are verified individually: the repository on disk is
    trusted, a given patch file is verified by ensuring all resources modified
    in the patch result in a valid repository (sufficient approvals).
    Additionally, monotonicity is preserved by embedding counters in each
    resource, and enforcing a counter increment after modification.

    {b NOTE: this is still work in progress, especially the documentation is not
    yet finished, and some API changes need to happen for clarification}

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(*
    time vs counter

    errors and warnings:
    - it is ok to have a package without package file -- (in non-strict mode) --> warning
    - all ids of an authorisation + team must exist -> otherwise: error!
    - resource digest conflicts are fatal errors

    - queued view: you'd like to see the state of repo if you'd sign now (and
      your public key is trusted -- but you also want to see what's missing (n/m
      in a quorum))
    - should queued have an influence on verification?  or kept external?  they
      should _not_ be added to the valid resources..

    - ./conex_verify (in initial mode) and ./conex_author status should have the
      same view on the repository -- but conex_author needs some special
      handling for _own_ key (might not yet be janitor-approved) and missing
      quorums on own packages

    - a janitor would also like to see: this is the repo, here's a PR, what's
      missing (quorum-wise)?

    - there's the "public view" (verify), strict or non-strict -- and then there
      is the "PR XXX requires a, b, c", and the "if approved, a, b, c would be
      valid" (plus "d, e, f are queued").

    # administration of repositories (janitors as trust anchors)

    What [conex] does not ensure

    index, which opens the possibility to depend on each others work.

    # Design: workflow, dependencies, goal (rollover)


    Our view of a community repository is 


    map where a package name is the key and its
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
    system interaction at all). *)

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
  val load_janitors : ?valid:(identifier -> Digest.t -> bool) ->
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
