

let () =
  Nocrypto_entropy_unix.initialize () ;
  Alcotest.run "Conex tests" Basics.tests
