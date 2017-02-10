## Conex - establish trust in community repositories

%%VERSION%%

NOTE: This is still work in progress, to be deployed with opam 2.0 and the [opam
repository](https://github.com/ocaml/opam-repository).

Conex is a library to verify and attest package release integrity and
authenticity through the use of cryptographic signatures.

A community repository contains packages from authors to provide an index and
allowing cross-references, and is curated by a team of janitors.  Information
about a package stored in a repository includes: license, author, releases,
their dependencies, build instructions, url, tarball checksum.  When someone
publishes a new package, the janitors integrate it into the repository, if it
compiles and passes some validity checks, e.g. its name must not be misleading
and not too general.

Janitors keep an eye on the repository, and fix intermediate failures: a new
compiler release, or a release of a dependent package, may break compilation of
a package, which is usually fixed by janitors who add a small patch or introduce
a dependency conflict in the repository.

*Conex* ensures that each package release has been approved by its author.
Furthermore, once a package release is known to the client, this information
cannot be rolled back by an attacker. A timestamping service periodically
approves a global view of all packages and identities in the repository, and
thus mitigates mix-and-match attacks where someone malicious prevents a client
from getting updates for one package while retrieving other updates.

The trust is rooted in digital signatures by package authors.  The server which
hosts the repository does not need to be trusted.  Neither does the host serving
release tarballs.

Someone needs to vouch for an author: the private part of a public key stored in
the repository should be known to the real author, and solely to that person.
If a single entity would be in control to vouch for authors, this entity would
need to fully trusted by all clients.  Instead, a quorum of a janitor team is
used in conex.  Janitors can together vouch for a public key of an author,
extend and shrink teams, authorise and deauthorise an author or team for a
package, and even checksums of packages and releases - useful for hot fixing
when a core package is updated, and all reverse dependencies need adjustments.

Conex adds metadata to a data repository (viewed as a map where package name is
the domain and releases thereof the codomain) to ensure integrity and
authenticity.  Resources of different kinds are added:

- *Authors*, consisting of a unique identifier, public key(s), accounts.
- *Teams*, sharing the same namespace as authors, containing a set of members.
- *Authorisation*, one for each package, describing which identities are authorised for the package.
- *Package*, for each package, listing all releases.
- *Release*, for each release, listing checksums of all data files.

Modifications to identities and authorisations need to be approved by a quorum
of janitors, package and release files can be modified either by an authorised
author or by a quorum of janitors.

Conex initially ensures that the repository is properly signed - using public
key fingerprints as anchors - and updates to the repository are verified
individually: the repository on disk is trusted, a given patch file is verified
by ensuring all resources modified in the patch result in a valid repository
(sufficient approvals).  Additionally, monotonicity is preserved by embedding
counters in each resource, and enforcing a counter increment after modification.

## Documentation
[![Build Status](https://travis-ci.org/hannesm/conex.svg?branch=master)](https://travis-ci.org/hannesm/conex)

[API documentation](https://hannesm.github.io/conex/doc/) is
available online, also a [coverage
report](https://hannesm.github.io/conex/coverage/).

We presented an [abstract at OCaml
2016](https://github.com/hannesm/conex-paper/raw/master/paper.pdf) about an
earlier design.

Another article on an [earlier design (from
2015)](http://opam.ocaml.org/blog/Signing-the-opam-repository/) is also
available.

Conex is inspired by [the update
framework](https://theupdateframework.github.io/), especially on their [CCS 2010
paper](https://isis.poly.edu/~jcappos/papers/samuel_tuf_ccs_2010.pdf), and
adapted to the opam repository.

The [TUF
spec](https://github.com/theupdateframework/tuf/blob/develop/docs/tuf-spec.txt)
has a good overview of attacks and threat model, which is shared by conex.

## Installation

`opam pin conex https://github.com/hannesm/conex.git` will install this binary,
once you have installed OCaml (>= 4.02.0) and opam (>= 2.0.0beta).

To use conex to ensure trust of the opam repository, you have to configure the
`validation_hook` to use `opam_verify`.

Package authors should additionally get familiar with the `conex_author` command
line utility.
