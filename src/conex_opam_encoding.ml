open Conex_utils
open Conex_resource

let np = ("", 0, 0)

let rec encode_s = function
  | Wire.Map s ->
    if s = M.empty then
      OpamParserTypes.Ident (np, "emptymap")
    else
      let data = M.fold (fun k v acc ->
          OpamParserTypes.(List (np, [ Ident (np, k) ; encode_s v ])) :: acc)
          s []
      in
      OpamParserTypes.List (np, data)
  | Wire.List l -> OpamParserTypes.List (np, List.map encode_s l)
  | Wire.Identifier i -> OpamParserTypes.Ident (np, i)
  | Wire.Data s -> OpamParserTypes.String (np, s)
  | Wire.Bigint i -> OpamParserTypes.Ident (np, "0x" ^ Uint.to_string i)
  | Wire.Smallint i -> OpamParserTypes.Int (np, i)
  | Wire.Pair (i, s) -> OpamParserTypes.Group
                          (np, [ encode_s i ; encode_s s ])
  | Wire.And (a, b) -> OpamParserTypes.Logop (np, `And, encode_s a, encode_s b)
  | Wire.Or (a, b) -> OpamParserTypes.Logop (np, `Or, encode_s a, encode_s b)

let encode t =
  let file_contents =
    M.fold (fun k v acc ->
        OpamParserTypes.Variable (np, k, encode_s v) :: acc)
      t []
  in
  let file = { OpamParserTypes.file_contents ; file_name = "" } in
  (* TODO use OpamPrinter.Preserved.items txt orig f here, requires  old data *)
  OpamPrinter.format_opamfile Format.str_formatter file ;
  Format.flush_str_formatter ()

let rec decode_s = function
  | OpamParserTypes.Ident (_, data) ->
    if String.is_prefix ~prefix:"0x" data then
      match Uint.of_string (String.slice ~start:2 data) with
      | None -> Error "cannot parse unsigned integer"
      | Some x -> Ok (Wire.Bigint x)
    else if data = "emptymap" then
      Ok (Wire.Map M.empty)
    else
      Ok (Wire.Identifier data)
  | OpamParserTypes.String (_, s) -> Ok (Wire.Data (String.trim s))
  | OpamParserTypes.List (_, []) -> Ok (Wire.List [])
  | OpamParserTypes.List (_, l) ->
    let is_pair = function
      | OpamParserTypes.List (_, [OpamParserTypes.Ident _ ; _]) -> true
      | _ -> false
    in
    if List.for_all is_pair l then begin
      List.fold_left (fun m xs ->
          m >>= fun m ->
          match xs with
            OpamParserTypes.List (_, [ OpamParserTypes.Ident (_, k) ; v ]) ->
            (decode_s v >>= fun v -> Ok (M.add (String.trim k) v m))
          | _ -> Error "can not happen")
        (Ok M.empty) l >>= fun map ->
      Ok (Wire.Map map)
    end else
      List.fold_left (fun xs s ->
          xs >>= fun xs ->
          decode_s s >>= fun x ->
          Ok (x :: xs))
          (Ok []) l >>= fun xs ->
      Ok (Wire.List (List.rev xs))
  | OpamParserTypes.Int (_, i) -> Ok (Wire.Smallint i)
  | OpamParserTypes.Group (_, [ OpamParserTypes.Logop (_, op, a, b) ]) ->
    decode_s a >>= fun a ->
    decode_s b >>= fun b ->
    begin match op with
      | `And -> Ok (Wire.And (a, b))
      | `Or -> Ok (Wire.Or (a, b))
    end
  | OpamParserTypes.Logop (_, op, a, b) ->
    decode_s a >>= fun a ->
    decode_s b >>= fun b ->
    begin match op with
      | `And -> Ok (Wire.And (a, b))
      | `Or -> Ok (Wire.Or (a, b))
    end
  | OpamParserTypes.Group (_, [ a ; f]) ->
    decode_s a >>= fun a ->
    decode_s f >>= fun f ->
    Ok (Wire.Pair (a, f))
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
