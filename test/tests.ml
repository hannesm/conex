

let () =
  Nocrypto_entropy_unix.initialize () ;
  let more =
    match Conex_openssl.V.check_version () with
    | Error e -> Printf.printf "no openssl tests, version %s\n" e ; []
    | Ok () ->
      (Basics.OC.tests "OpenSSL") @ (Repositorytests.O.tests "OpenSSL")
  in
  Alcotest.run "Conex tests" (Basics.tests @ Repositorytests.tests @ more)
