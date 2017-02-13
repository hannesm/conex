open Conex_utils

module V = struct
  let verify_rsa_pss ~key ~data ~signature =
    match
      let filename = Filename.temp_file "conex" "sig" in
      Conex_unix_persistency.write_replace (filename ^ ".key") key >>= fun () ->
      Conex_unix_persistency.write_replace (filename ^ ".txt") data >>= fun () ->
      let cmd = Printf.sprintf "echo %s | openssl base64 -d -out %s" signature (filename ^ ".sig") in
      (if 0 = Sys.command cmd then Ok () else Error "bas64") >>= fun () ->
      let cmd = Printf.sprintf "openssl dgst -sha256 -verify %s.key -sigopt rsa_padding_mode:pss -signature %s.sig %s.txt" filename filename filename in
      let res = if 0 = Sys.command cmd then Ok () else Error "broken" in
      let _ = Conex_unix_persistency.remove (filename ^ ".txt")
      and _ = Conex_unix_persistency.remove (filename ^ ".key")
      and _ = Conex_unix_persistency.remove (filename ^ ".sig")
      and _ = Conex_unix_persistency.remove filename
      in
      res
    with
    | Ok () -> Ok ()
    | Error x when x = "base64" -> Error `InvalidBase64Encoding
    | Error x when x = "broken" -> Error `InvalidSignature
    | Error _ -> Error `InvalidPublicKey

  let b64sha256 data =
    match
      let filename = Filename.temp_file "conex" "b64" in
      Conex_unix_persistency.write_replace filename data >>= fun () ->
      let cmd = Printf.sprintf "openssl dgst -binary -sha256 %s | openssl base64" filename in
      let input = Unix.open_process_in cmd in
      let output = input_line input in
      let _ = Unix.close_process_in input in
      Ok output
    with
    | Ok s -> s
    | Error e -> invalid_arg e
end

module O_V = Conex_crypto.Make_verify (V)