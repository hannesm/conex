open Conex_result
open Conex_core

let np = ("", 0, 0)

let rec encode_s = function
  | Conex_data_persistency.Map s ->
    if s = M.empty then
      OpamParserTypes.Ident (np, "emptymap")
    else
      let data = M.fold (fun k v acc ->
          OpamParserTypes.(List (np, [ String (np, k) ; encode_s v ])) :: acc)
          s []
      in
      OpamParserTypes.List (np, data)
  | Conex_data_persistency.List l -> OpamParserTypes.List (np, List.map encode_s l)
  | Conex_data_persistency.String s -> OpamParserTypes.String (np, s)
  | Conex_data_persistency.Int i -> OpamParserTypes.Ident (np, "UINT" ^ Uint.to_string i)

let encode t =
  let file_contents =
    M.fold (fun k v acc ->
        OpamParserTypes.Variable (np, k, encode_s v) :: acc)
      t []
  in
  let file = { OpamParserTypes.file_contents ; file_name = "" } in
  OpamPrinter.Normalise.opamfile file

let rec decode_s = function
  | OpamParserTypes.Ident (_, data) ->
    let l = String.length data in
    if l > 4 && String.sub data 0 4 = "UINT" then
      Ok (Conex_data_persistency.Int (Uint.of_string (String.sub data 4 (l - 4))))
    else if data = "emptymap" then
      Ok (Conex_data_persistency.Map M.empty)
    else
      Error "unexpected ident"
  | OpamParserTypes.String (_, s) -> Ok (Conex_data_persistency.String s)
  | OpamParserTypes.List (_, []) ->
    Ok (Conex_data_persistency.List [])
  | OpamParserTypes.List (_, l) ->
    let is_pair = function
        OpamParserTypes.List (_, [OpamParserTypes.String _ ; _]) -> true
      | _ -> false
    in
    if List.for_all is_pair l then begin
      List.fold_left (fun m xs ->
          m >>= fun m ->
          match xs with
            OpamParserTypes.List (_, [ OpamParserTypes.String (_, k) ; v ]) ->
            (decode_s v >>= fun v -> Ok (M.add k v m))
          | _ -> Error "can not happen")
        (Ok M.empty) l >>= fun map ->
      Ok (Conex_data_persistency.Map map)
    end else
      List.fold_left (fun xs s ->
          xs >>= fun xs ->
          decode_s s >>= fun x ->
          Ok (x :: xs))
          (Ok []) l >>= fun xs ->
      Ok (Conex_data_persistency.List xs)
  | _ -> Error "unexpected thing while decoding"

let decode data =
  (try Ok (OpamParser.string data "noname") with
     Parsing.Parse_error -> Error "parse error") >>= fun file ->
  let items = file.OpamParserTypes.file_contents in
  List.fold_left (fun acc v ->
      acc >>= fun acc ->
      match v with
      | OpamParserTypes.Section _ -> Error "unexpected section"
      | OpamParserTypes.Variable (_, k, v) ->
        decode_s v >>= fun v ->
        Ok (M.add k v acc))
    (Ok M.empty) items
