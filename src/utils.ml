let option none some = function
  | None   -> none
  | Some x -> some x

let to_hex cs =
  let i_to_h i idx s =
    let v_to_h = function
      | x when x < 10 -> char_of_int (x + 48)
      | x -> char_of_int (x + 55)
    in
    let high = (0xf0 land i) lsr 4
    and low = 0x0f land i
    in
    Bytes.set s idx (v_to_h high) ;
    Bytes.set s (succ idx) (v_to_h low)
  in
  let s = Bytes.make (Cstruct.len cs * 2) ' ' in
  for i = 0 to pred (Cstruct.len cs) do
    i_to_h (Cstruct.get_uint8 cs i) (i * 2) s
  done ;
  Bytes.to_string s

let pp_hex ppf x = Format.pp_print_string ppf (to_hex (Cstruct.of_string x))

let rec filter_map ~f = function
  | []    -> []
  | x::xs ->
      match f x with
      | None    ->       filter_map ~f xs
      | Some x' -> x' :: filter_map ~f xs
