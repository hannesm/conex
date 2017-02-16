#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let nocrypto = Conf.with_pkg (* ~default:false *) "nocrypto"

let () =
  Pkg.describe "conex" @@ fun c ->
  let nocrypto = Conf.value c nocrypto in
  Ok [
    Pkg.mllib "src/conex.mllib" ;
    Pkg.mllib ~cond:nocrypto "src/nocrypto/conex-nocrypto.mllib" ;
(*    Pkg.bin "analysis/maintainer" ;
      Pkg.bin "analysis/opam_repo_stats" ; *)
    Pkg.bin "app/conex_author" ;
    Pkg.bin "app/conex_verify_openssl" ;
    Pkg.bin "app/conex_verify_nocrypto" ;
    Pkg.test "test/tests"
  ]
