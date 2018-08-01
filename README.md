## Conex - establish trust in community repositories

%%VERSION%%

[Conex](https://github.com/hannesm/conex) is a library to verify and attest release integrity and
authenticity of a community repository through the use of cryptographic signatures.

This is atm WIP, please take a look at [testrepo](https://github.com/hannesm/testrepo) for using conex.

## Documentation

We presented [an earlier design at OCaml
2016](https://github.com/hannesm/conex-paper/raw/master/paper.pdf) about an
earlier design.

Another article on an [even earlier design (from
2015)](http://opam.ocaml.org/blog/Signing-the-opam-repository/) is also
available.

Conex is inspired by [the update
framework](https://theupdateframework.github.io/), especially on their [CCS 2010
paper](https://isis.poly.edu/~jcappos/papers/samuel_tuf_ccs_2010.pdf), and
adapted to the opam repository.

The [TUF
spec](https://github.com/theupdateframework/tuf/blob/develop/docs/tuf-spec.txt)
has a good overview of attacks and threat model, both of which are shared by conex.

## Installation

`opam instal conex` will install this library and tool,
once you have installed OCaml (>= 4.03.0) and opam (>= 2.0.0beta).

A small test repository with two janitors (their private keys), an empty package
`foo` owned by `c` and valid signatures is
[here](https://github.com/hannesm/testrepo) including transcripts of how it was
setup, and how to setup opams `repo validation hook`.
