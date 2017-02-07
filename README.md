## Conex - trusted package repository

%%VERSION%%

Conex is a design and implementation of a trusted data repository, targeting the
community cultured [opam repository](https://github.com/ocaml/opam-repository).
Individual authors sign their package releases including metadata.  Signatures,
together with authorisation, will soon be part of the opam repository.  The
trust is rooted in a team of janitors.

This is still work in progress, to be finished with opam 2.0.

Conex is inspired by [the update
framework](https://theupdateframework.github.io/), especially on their [CCS 2010
paper](https://isis.poly.edu/~jcappos/papers/samuel_tuf_ccs_2010.pdf), and
adapted to the opam repository (a git repository).

The threat model is a malicious attacker: access to the repository server should
not allow the attacker to have a user arbitrary code installed.  A compromise of
an author key can affect only the packages they are authorised to change.  A
quorum of the team of janitors is needed to approve changes (such as public key
enrollment of an author, granting access to a team, initial release of a
package, ...).  Additionally, conex allows a user to detect freeze attacks, and
avoids mix-and-match attacks.

The [TUF
spec](https://github.com/theupdateframework/tuf/blob/develop/docs/tuf-spec.txt)
has a good overview of attacks and threat model, which is shared by conex.

## Documentation
[![Build Status](https://travis-ci.org/hannesm/conex.svg?branch=master)](https://travis-ci.org/hannesm/conex)

There is an [up to date
overview](https://hannesm.github.io/conex/doc/Conex.html).

More detailed [API documentation](https://hannesm.github.io/conex/doc/) is
available online, also a [coverage
report](https://hannesm.github.io/conex/coverage/).

We presented an [abstract at OCaml
2016](https://github.com/hannesm/conex-paper/raw/master/paper.pdf) about an
earlier design.

Another article on an [earlier design (from
2015)](http://opam.ocaml.org/blog/Signing-the-opam-repository/) is also
available.

## Installation

`opam pin conex https://github.com/hannesm/conex.git` will install this binary,
once you have installed OCaml (>= 4.02.0) and opam (>= 2.0.0beta).

To use conex to ensure trust of the opam repository, you have to configure the
`validation_hook` to use `opam_verify`.

Package authors should additionally get familiar with the `conex_author` command
line utility.
