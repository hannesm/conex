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

  val verify_root : ?valid:(Digest.t -> identifier -> bool) ->
    Conex_io.t -> name -> (Conex_repository.t, string) result

  val verify_targets : Conex_io.t -> Conex_repository.t -> identifier ->
    (Targets.t, string) result

  val verify : ?ignore_missing:bool -> Conex_io.t -> Conex_repository.t -> (unit, string) result

  val verify_diffs : string -> Conex_io.t -> Conex_io.t -> Conex_diff.t list ->
    (unit, string) result
end
