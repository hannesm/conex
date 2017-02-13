open Conex_utils

module V = struct

  (* good OpenSSL versions:
     "OpenSSL 1.0.2g  1 Mar 2016" ; Ubuntu at cl.cam
     "OpenSSL 1.0.2j-freebsd  26 Sep 2016" ; FreeBSD 11 & -CURRENT
     "OpenSSL 1.0.1e 11 Feb 2013" ; debian 7.11

     bad ones (no PSS):
     "OpenSSL 0.9.8zh-freebsd 3 Dec 2015" ; FreeBSD 9.3
     "OpenSSL 0.9.8o 01 Jun 2010" ; debian 6.0.10
  *)

  let check_version () =
    let cmd = "openssl version" in
    let input = Unix.open_process_in cmd in
    let output = input_line input in
    let _ = Unix.close_process_in input in
    if String.is_prefix ~prefix:"OpenSSL 0." output then
      Error ("need at least OpenSSL 1, found: " ^ output)
    else
      Ok ()

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
      let _ = Conex_unix_persistency.remove filename in
      Ok output
    with
    | Ok s -> s
    | Error e -> invalid_arg e
end

module O_V = Conex_crypto.Make_verify (V)
