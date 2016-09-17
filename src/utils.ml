let option none some = function
  | None   -> none
  | Some x -> some x

let rec filter_map ~f = function
  | []    -> []
  | x::xs ->
      match f x with
      | None    ->       filter_map ~f xs
      | Some x' -> x' :: filter_map ~f xs
