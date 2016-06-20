open Core

type c = {
  filename : name ;
  bytesize : int64 ;
  checksum : digest ;
}

let pp_checksum ppf c =
  Format.fprintf ppf "%a [%Lu bytes]: %a@ "
    pp_name c.filename c.bytesize pp_digest c.checksum

let checksum_equal a b =
  a.filename = b.filename && a.bytesize = b.bytesize && a.checksum = b.checksum

let checksum filename data =
  let bytesize = Int64.of_int (String.length data)
  and checksum = digest data
  in
  { filename ; bytesize ; checksum }

module M = Map.Make(String)

type checksum_map = c M.t

let fold f m acc = M.fold (fun _ c acc -> f c acc) m acc
let find m id = M.find id m

let pp_checksum_map ppf cs =
  pp_list pp_checksum ppf
    (List.sort (fun a b -> String.compare a.filename b.filename)
       (snd (List.split (M.bindings cs))))

type t = {
  counter : int64 ;
  version : int64 ;
  name : name ;
  files : checksum_map ;
  signatures : Signature.t list ;
}

let pp_checksums ppf c =
  Format.fprintf ppf "checksums for %a (counter %Lu) are:@.%a@.sigs: %a"
    pp_name c.name c.counter
    pp_checksum_map c.files
    Signature.pp_signatures c.signatures

let checksums ?(counter = 0L) ?(version = 0L) ?(signatures = []) name files =
  let files = List.fold_left (fun m f -> M.add f.filename f m) M.empty files in
  { counter ; version ; name ; files ; signatures }

let add_sig c s = { c with signatures = s :: c.signatures }

let set_counter cs counter = { cs with counter }

(* XXX: compare_checksums which returns a result type!
   then we can inform the user which checksum is offending *)

let checksums_equal a b =
  a.name = b.name &&
    M.cardinal a.files = M.cardinal b.files &&
      M.for_all
        (fun k c -> if M.mem k b.files then checksum_equal (M.find k b.files) c else false)
        a.files
