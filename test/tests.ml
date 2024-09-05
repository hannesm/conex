
let () =
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna) ;
  let more =
    match Conex_openssl.V.check_version () with
    | Error e -> Printf.printf "no openssl tests, version %s\n" e ; []
    | Ok () -> Test_conex.OC.tests ~openssl:true "OpenSSL"
  in
  Alcotest.run "Conex tests" (
    ("Uint", Test_uint.tests) ::
    ("String", Test_string.tests) ::
    ("Path", Test_path.tests) ::
    ("Tree", Test_tree.tests) ::
    ("provider", Test_provider.tests) ::
    Test_conex.tests @ more)
