(** Establish trust in community repositories

    Conex is a library to verify and attest package release integrity and
    authenticity through the use of cryptographic signatures.

    Each author cryptographically signs a list of resources (own public key,
    package releases) they vouch for.  The {{!Conex_repository.t}repository} is
    a map where resource digests are the key, and the set of warrantors the
    value.  Verification of the signature is done via
    {{!Conex_verify.S.verify}verify}.

    Given a {{!Conex_utils.LOGS}logs} and a {{!Conex_verify.S}verify}
    implementation, this modules provides functionality to verify identities and
    packages.  All functions require {{!Conex_io.t}IO} explicitly.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)


open Conex_utils
open Conex_resource

module Make (L : LOGS) (C : Conex_verify.S): sig

  (** [verify_id io repo id] reads [id] from [io] unless [id] is already part of
      [repo].  The read data will either be a team or an author.  If
      verification is successful, resource and identitiy data are added to
      [repo]. *)
  val verify_id : Conex_io.t -> Conex_repository.t -> identifier ->
    (Conex_repository.t, string) result

  (** [verify_ids ~ids io repo] folds over [ids] (default: all ids present on
      [io]), calling {!verify_id}. *)
  val verify_ids : ?ids:S.t -> Conex_io.t -> Conex_repository.t ->
    (Conex_repository.t, string) result

  (** [verify_janitors ~valid io repo] reads the team "janitors" from [io], and
      all its team members.  It verifies in two steps: first it uses only those
      public keys which fingerprints are [valid].  If successful, in a second
      step the resulting repository is used to verify the remaining janitors,
      followed by verification of the janitor team. *)
  val verify_janitors : ?valid:(identifier -> Digest.t -> bool) ->
    Conex_io.t -> Conex_repository.t ->
    (Conex_repository.t, string) result

  (** [verify_package ~ignore_missing io repo name] reads and verifies the
      authorisation of the package [name].  All [ids] who are authorised are
      loaded into the repository (using {!verify_ids}).  The package data for
      [name] is read and verified, followed by all releases.  Each release is
      read and verified. If [ignore_missing] is true, packages with missing
      authorisations or package indexes are ignored. *)
  val verify_package : ?ignore_missing:bool -> Conex_io.t -> Conex_repository.t -> name ->
    (Conex_repository.t, string) result

  (** [verify_snapshot io repo] reads and verifies the "snapshot", and verifies
      that all other authors (and no additional authors) are present, and the
      checksums match. *)
  val verify_snapshot : Conex_io.t -> Conex_repository.t -> (unit, string) result

  (** [verify_patch ~ignore_missing ~valid repo io newio diffs] verifies a
      patch: [valid] is used to validate the janitors team of the repository
      with applied patch [newio].  All identifiers and packages of the
      repository with [patch] applied are verified, and additionally all
      modified resources have their monotonicity verified.  If [ignore_missing]
      is true, packages with missing authorisations and package indexes are
      ignored. *)
  val verify_patch : ?ignore_missing:bool ->
    ?valid:(identifier -> Digest.t -> bool) -> Conex_repository.t ->
    Conex_io.t -> Conex_io.t -> Conex_diff.t list ->
    (Conex_repository.t, string) result
end
