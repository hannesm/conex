#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let metas = [
  Pkg.meta_file ~install:false "pkg/META" ;
  Pkg.meta_file ~install:false "pkg/META.nocrypto" ;
]

let opams =
  let opam no_lint name =
    Pkg.opam_file ~lint_deps_excluding:(Some no_lint) ~install:false name
  in
  [ opam ["logs";"fmt";"rresult";"cstruct";"nocrypto";"x509";"alcotest"] "opam";
    opam [ "opam-format";"conex" ] "conex-nocrypto.opam" ]

let distrib =
  let exclude_paths () = Pkg.exclude_paths () >>| fun ps -> "analysis" :: ps in
  Pkg.distrib ~exclude_paths ()

let () =
  Pkg.describe ~metas ~opams "conex" @@ fun c ->
  match Conf.pkg_name c with
  | "conex" ->
    Ok [ Pkg.lib "pkg/META" ;
         Pkg.mllib "src/conex.mllib" ;
         Pkg.bin "app/conex_verify_openssl" ]
  | "conex-nocrypto" ->
    Ok [ Pkg.lib "pkg/META.nocrypto" ;
         Pkg.mllib "src/nocrypto/conex-nocrypto.mllib" ;
         Pkg.bin "app/conex_author" ;
         Pkg.bin "app/conex_verify_nocrypto" ;
         Pkg.bin "app/conex_snapshot" ;
         Pkg.test "test/tests" ]
  | other ->
    R.error_msgf "unknown package name: %s" other
