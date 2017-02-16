#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let nocrypto = Conf.with_pkg (* ~default:false *) "nocrypto"
let format = Conf.with_pkg ~default:false "format"

let () =
  Pkg.describe "conex" @@ fun c ->
  let nocrypto = Conf.value c nocrypto
  and format = Conf.value c format
  in
  Ok [
    Pkg.mllib "src/conex.mllib" ;
    Pkg.mllib ~cond:nocrypto "src/nocrypto/conex-nocrypto.mllib" ;
    Pkg.bin ~cond:format ~dst:"conex_maintainer" "analysis/maintainer" ;
    Pkg.bin ~cond:format ~dst:"conex_repo_stats" "analysis/opam_repo_stats" ;
    Pkg.bin "app/conex_author" ;
    Pkg.bin "app/conex_verify_openssl" ;
    Pkg.bin "app/conex_verify_nocrypto" ;
    Pkg.test "test/tests"
  ]
