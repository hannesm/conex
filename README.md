[![Build Status](https://travis-ci.org/hannesm/conex.svg?branch=master)](https://travis-ci.org/hannesm/conex)

## Conex: a signed repository

Conex is a system inspired heavily by [TUF][] which provides secure distribution
of a repository.  A repository contains both metadata (public key and ownership)
and actual data.  The original developer of data cryptographically signs their
data, thus the repository server does not need to be trusted.  Another entity
are maintainers, who step in if an developer lost their private key, a piece of
data is owned by somebody else, etc.  A single malicious maintainer cannot
compromise the repository because a quorum of maintainer signatures are needed
for these operations.  The initial bootstrap relies on a set of trust anchors,
which have to be installed at the client using some trusted channel (out of
scope of this document).

A repository is a directory consisting of files and directories.  Any kind of
links are prohibited.  Each directory which contains actual data is owned by a
public key.  The owner is able to modify and add new data to the subdirectory.

The security relies on the fact that it is hard to compromise a quorum of
maintainer keys, the developer of an individual data item is trusted to not
release malicious data.  Distribution of the repository does not need to be via
secured channels.  Rollback of data is not possible.

In contrast to TUF, the keys and ownership information in Conex is local, which
allows simultaneous editing of keys, ownership information, and data.

Conex has three entities: public keys, identified by their (repository-wide
unique) key identifier, names (repository-wide unique) owned by a set of public
keys, and data, released below a name by an owner.  There are more privileged
keys which can recover from a stuck situation (key lost, owner disappeared, data
is wrong and needs immediate fixes) called maintainers.  An initial set of
maintainers should be distributed out of band.

The layout of a repository, the storage format of metadata, the update
mechanism, the public key format and digest algorithms, are configurable.  This
document describes which pieces of information need to be available, and which
security claims are preserved by a repository and updates to it.  For
simplicity, an example repository layout is presented.

Example terminal sessions are in [examples](https://github.com/hannesm/conex/tree/master/examples).

## Repository layout

- Public keys are stored in the `keys` directory with the key identifier as its
  name, cryptographically signed.
- All data is stored below `data`, where each subdirectory is a name owned by a
  set of keys.
- The ownership information is stored in the `delegate` file in each
  subdirectory, cryptographically signed.
- Digests and length of data is stored in the `checksums` file, again signed by
  a legitimate owner.

```
repository root /
|-- data/
|   |-- a/
|   |   |-- delegate
|   |   |-- a.1/
|   |   |   |-- a
|   |   |   |-- b
|   |   |   |-- c
|   |   |   |-- dir/
|   |   |   |   `-- d
|   |   |   `-- checksums
|   |   `-- a.2/
|   |       `-- checksums
|   `-- b/
|       `-- delegate
`-- keys/
    |-- developer1
    `-- developer2
```

`keys`, `delegate` and `checksums` are cryptographically signed, and do not need
to appear in any `checksums` file.  To prevent rollback attacks, each signed
file contains a monotonic counter.

### Signature

A signature is encoded as a triple `(keyid, algorithm, value)`.

- `keyid` is the key identifier, a string
- `algorithm` is a string
- `value` is the base64-encoded signature (over the concatenation of data ",algorithm:" signature-algorithm ",id" keyid).

### Keys

A public key is a PEM-encoded (of a DER-encoded pkInfo structure, including an
AlgorithmIdentifier and the actual public key) key.  For the remainder of this
document we will assume RSA keys, but it should not matter which public key
algorithm is used.

A public key is defined by a quintuple `(counter, keyid, key, role, signatures)`.

- `counter` is a monotonic counter starting from 0
- `keyid` is a unique identifier (printable ASCII characters, no control
  characters), the contained file has the same name as the keyid.  Uniqueness
  over case-insensitive keyids must be preserved throughout the repository.
- `key` is a PEM-encoded key, or the empty string (revoked key)
- `role` is either "developer" or "maintainer"
- `signatures` is a list of signatures

### Delegation

Each `delegate` file describes the ownership of a name in the repository to the
list of key identifiers.  A `delegate` file consists of a quadruple `(name,
counter, key-ids, signatures)`.

- `name` is the path from the repository root to here, encoded as string
- `counter` is a monotonic counter starting from 0
- `key-ids` is a set of key identifiers who own this directory
- `signatures` is a list of signatures

### Checksums file

A `checksums` file contains checksums of all files in the containing directory,
and subdirectories thereof.  It is a quadruple `(counter, name, files,
signatures)`.

- `counter` is a monotonic counter
- `name` is the name of the data (to preserve uniqueness, it should be prefixed
  by the delegate name (`a` is a prefix of `a.1`) or have another unique mapping
  to its delegate)
- `files` is a list of triples `(filename, byte-size, digest)`
- `signatures` a list of signatures

## Repository verification

There are two approaches to verification of a repository: verifying a snapshot,
and verifying a patch for a repository.  Verifying a snapshot of a repository
can not verify preservation of ownership, because a snapshot does not contain
ownership history.  Verifying an empty snapshot and verifying each patch
individually verifies preservation of ownership.

### Snapshot verification

Given a repository, a set of trusted keys (`TK`) and a quorum, we can verify a
repository and any given time:
- a developer key must be signed by its private key or a quorum of maintainers
- a maintainer key must be signed by its private key and a quorum of maintainers
- a delegation must be signed by an owner or a quorum of maintainers
- a checksum must include all files in the containing directory, and be signed
  by an owner or a quorum of maintainers

To preserve uniqueness of names:
 - no delegation may share its name with any other
 - each checksum name must have the delegate name as prefix

To revoke a key, the empty string is used in the key file.  This file must then
be signed by a quorum of maintainers.  To phase out a name, a delegate file with
an empty key-ids list is used, signed by a quorum of maintainers.  A checksums
file with an empty files list represents the empty data directory.

### Patch verification

Given a repository in state `S`, and a patch `P`, the repository evolves into
state `S'`, which is `S` with `P` applied.  If the snapshot verification
succeeds in state `S`, it will also succeed in `S'` if the patch verification of
`P` succeeded.

Each patch contains an list of hunks. Each hunk can be classified into one of
three categories, for each apply different verification rules: key modification,
delegate modification, data modification.  Only a single hunk which modifies a
key is allowed in a patch, and is verified first.  (There might be subsequent
hunks which change signatures of other keys, though.)  Afterwards, delegate
modifications are verified, and last data modifications.

Hunks (h<sub>n</sub>) are processed in order, leading to `S` &rarr;<sub>h<sub>1</sub></sub> `S`<sub>`1`</sub> &rarr;<sub>h<sub>2</sub></sub> `S`<sub>2</sub> ... &rarr;<sub>h<sub>n-1</sub></sub> `S`<sub>n-1</sub> &rarr;<sub>h<sub>n</sub></sub> `S`<sub>n</sub> where `S'` (mentioned above) is `S`<sub>n</sub>.

Each hunk is verified in the following way (using repository state `S` and
`TK`), depending on its content:
- key modification:
   - key size is sufficiently big
   - `keyid` is unique and matches filename
   - if it adds a new key file:
     - `counter` is 0
     - key is self-signed
   - if it modifies an existing key:
     - `counter` increased
     - either key signed using `S`, or by a quorum
   - deletion of a key (empty key)
     - signed by quorum
   - role <> developer
     - signed by quorum
- `delegate` modification:
   - addition of a delegate is signed by a keyid in the `key-ids` list
   - deletion of a delegate is invalid (but key-ids can be empty)
   - `counter` field increased (or 0 if addition)
   - modification: signed by a key of the list `key-ids` in state `S`, or by a
     quorum
   - name of `delegate` must not clash with any other delegate
- data modification:
   - find the `delegate` file for this piece of data
   - `K` is the set of `key-ids` of this `delegate` file, else empty
   - `counter` field in the `checksums` file increased (or 0 if addition)
   - `checksums` is signed by any key in `K` or a quorum
   - `checksums` name is related to `delegate` name (e.g. prefix)
   - all files in the directory and its subdirectories occur in `files`, and
     have the correct length and checksum.

## FAQ

### Difference to [TUF] target and delegation role

We want to preserve the invariant that once something is owned by a key, it
stays owned by that key.  Therefore we track patches (since delegation is not
signed by RMs).

Python uses `claimed` vs `unclaimed` names instead, which we might want to adapt
(by requiring quorum on `delegate` file for a `claimed` name).

### Is a repository verifyable by itself?

Yes.  Try `conex verify repo`.  There is an issue that a package owner cannot
remove themself as an owner, even if there are multiple owners (a signature on a
delegate must be valid in the new repository, as well as in the old repository).
Preservation of ownership cannot be verified from a repository itself (whether
the owner of a module changed).

### Why not just plain [TUF]?

We have a git-like repository in mind, rather than a directory.  Several people
should be able to change subparts (which they own) of the system in a concurrent
way, without merge conflicts.  This is the reason why we distribute metadata
(keys, delegate, checksums) in a per-identity, per-package way, instead of
having a single global file.

### Why not git signing?

Git signatures do not end up in the git tree, thus freeze and mix-and-match
attacks may apply.  Also, this proposal does not rely on git, and could use any
other distribution mechanism.

[TUF]: https://github.com/theupdateframework/tuf/blob/develop/docs/tuf-spec.txt

### What happened to the original OPAM signing proposal?

It is still around
[here](http://opam.ocaml.org/blog/Signing-the-opam-repository/).  The problem is
that it is too complex, it introduces artificial hierarchy, and does not provide
stronger guarantees than this proposal.  This proposal does not target OPAM
directly, but is a small library which can be used to sign arbitrary
repositories.

The goal is to get this library into shape, and re-develop OPAM signing on top
of it.

### What will change in the current OPAM workflow?

Disclaimer: I'm not in charge of opam, these are just suggestions to opam
developers.

For developers, not much.  The `opam publish` will deal with it.  There will
also be an independent subcommand of opam to generate a key, delegation,
checksum, and sign them.  There will be no requirement to use opam publish.
`opam lint` will be extended to guide users what is needed (and camelus will
remind if something is wrong on the opam-repository).

For users, also not much.  They will receive errors when something goes wrong.

For repository maintainers, quite a bit: they will no longer be able to modify
all packages via git push or filing and merging a PR, but rather either need a
developer who owns the package and submits a PR or need to have a quorum of
other repository maintainers signing a PR.  There will for sure be tools to
handle quorums.  The turnaround time for emergency fixes will be longer, but
then there won't be anymore the compromise of a single person's machine to
compromise the entire repository.
