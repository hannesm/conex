open Core

type c = {
  filename : name ;
  bytesize : int64 ;
  checksum : digest ;
}

(*BISECT-IGNORE-BEGIN*)
let pp_checksum ppf c =
  Format.fprintf ppf "%a [%Lu bytes]: %a@ "
    pp_name c.filename c.bytesize pp_digest c.checksum
(*BISECT-IGNORE-END*)

let checksum_equal a b =
  name_equal a.filename b.filename && a.bytesize = b.bytesize && a.checksum = b.checksum

let checksum filename data =
  let bytesize = Int64.of_int (String.length data)
  and checksum = digest data
  in
  { filename ; bytesize ; checksum }

module M = Map.Make(String)

type checksum_map = c M.t

let fold f m acc = M.fold (fun _ c acc -> f c acc) m acc
let find m id = M.find id m

(*BISECT-IGNORE-BEGIN*)
let pp_checksum_map ppf cs =
  pp_list pp_checksum ppf
    (List.sort (fun a b -> String.compare a.filename b.filename)
       (snd (List.split (M.bindings cs))))
(*BISECT-IGNORE-END*)

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  files : checksum_map ;
}

(*BISECT-IGNORE-BEGIN*)
let pp_checksums ppf c =
  Format.fprintf ppf "checksums for %a (counter %Lu) are:@.%a@."
    pp_name c.name c.counter
    pp_checksum_map c.files
(*BISECT-IGNORE-END*)

let checksums ?(counter = 0L) ?(version = 0L) name files =
  let files = List.fold_left (fun m f -> M.add f.filename f m) M.empty files in
  { counter ; version ; name ; files }

let set_counter cs counter = { cs with counter }

let compare_checksums a b =
  guard (name_equal a.name b.name) (`InvalidName (a.name, b.name)) >>= fun () ->
  if M.equal checksum_equal a.files b.files then
    Ok ()
  else
    let invalid, missing =
      M.fold (fun k v (inv, miss) ->
          if M.mem k b.files then
            let c = M.find k b.files in
            if checksum_equal c v then (inv, miss)
            else ((c, v) :: inv, miss)
          else (inv, k :: miss))
        a.files ([], [])
    and toomany =
      M.fold (fun k _ acc ->
          if M.mem k a.files then acc
          else k :: acc)
        b.files []
    in
    Error (`ChecksumsDiff (a.name, missing, toomany, invalid))
