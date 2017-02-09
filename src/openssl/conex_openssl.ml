open Conex_utils

module V = struct
  (*  let good_rsa p = Nocrypto.Rsa.pub_bits p >= 2048*)

  let verify_rsa_pss ~key ~data ~signature =
    match
      Conex_unix_persistency.write_file "/tmp/key.pem" key >>= fun () ->
      Conex_unix_persistency.write_file "/tmp/data.txt" data >>= fun () ->
      let cmd = Printf.sprintf "echo %s | openssl base64 -d" signature in
      let input = Unix.open_process_in cmd in
      let output = input_line input in
      let _ = Unix.close_process_in input in
      Conex_unix_persistency.write_file "/tmp/sig.out" output >>= fun () ->
      if 0 = Sys.command "openssl dgst -sha256 -verify /tmp/key.pem -sigopt rsa_padding_mode:pss -signature /tmp/sig.out /tmp/data.txt" then
        Ok ()
      else
        Error "broken"
    with
    | Ok () -> Ok ()
    | Error _ -> Error `NoSignature
(*
    openssl dgst -sign rsakey.pem -sigopt rsa_padding_mode:pss

        The -pss_saltlen argument to the dgst utility enables it.

 including special saltlens -1 and -2) are available through yet another -sigopt:
  -sigopt rsa_pss_saltlen:N
where N is the saltlen value
*)
  let b64sha256 data =
    let cmd = Printf.sprintf "echo %s | openssl dgst -binary -sha256 | openssl base64" data in
    let input = Unix.open_process_in cmd in
    let output = input_line input in
    let _ = Unix.close_process_in input in
    output
end

module NC_R = Conex_repository.Make (V)
