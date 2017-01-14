open Conex_result
open Conex_core
open Conex_utils

module Signature = struct
  type t = identifier * Uint.t * string

  let extend_data data id ts =
    String.concat " " [ data ; id ; Uint.to_string ts ]

  (*BISECT-IGNORE-BEGIN*)
  let pp_signature ppf (id, ts, _) =
    Format.fprintf ppf "signature %a created at 0x%s" pp_id id (Uint.to_string ts)
  (*BISECT-IGNORE-END*)
end

module Publickey = struct
  type email = identifier

  type service = [
    | `Email of email
    | `GitHub of identifier
    | `Other of identifier * string
  ]

  type t = {
    counter : Uint.t ;
    version : Uint.t ;
    name : identifier ;
    accounts : service list ;
    key : pub option ;
  }

  let publickey ?(counter = Uint.zero) ?(version = Uint.zero) ?(accounts = []) name key =
    { counter ; version ; accounts ; name ; key }

  (*BISECT-IGNORE-BEGIN*)
  let pp_service ppf = function
    | `Email e -> Format.fprintf ppf "email %s" e
    | `GitHub e -> Format.fprintf ppf "GitHub %s" e
    | `Other (k, v) -> Format.fprintf ppf "%s %s" k v

  let pp_publickey ppf p =
    let pp_opt_key ppf k =
      Format.pp_print_string ppf
        (match k with None -> "no key" | Some (`Pub _) -> "")
    in
    Format.fprintf ppf "publickey #%s %a %a@ %a"
      (Uint.to_string p.counter)
      pp_id p.name
      pp_opt_key p.key
      (pp_list pp_service) p.accounts
  (*BISECT-IGNORE-END*)
end

module Team = struct
  type t = {
    counter : Uint.t ;
    version : Uint.t ;
    name : identifier ;
    members : S.t
  }

  let team ?(counter = Uint.zero) ?(version = Uint.zero) ?(members = S.empty) name =
    { counter ; version ; members ; name }

  let equal a b =
    id_equal a.name b.name && S.equal a.members b.members

  let add t id = { t with members = S.add id t.members }

  let remove t id = { t with members = S.remove id t.members }

  let prep t =
    let carry, counter = Uint.succ t.counter in
    { t with counter }, carry

  (*BISECT-IGNORE-BEGIN*)
  let pp_mems ppf x = pp_list pp_id ppf (List.sort String.compare (S.elements x))

  let pp_team ppf x =
    Format.fprintf ppf "team #%s %a@ %a"
      (Uint.to_string x.counter) pp_id x.name pp_mems x.members
   (*BISECT-IGNORE-END*)
end

module Authorisation = struct
  type t = {
    counter : Uint.t ;
    version : Uint.t ;
    name : name ;
    authorised : S.t ;
  }

  let authorisation
      ?(counter = Uint.zero) ?(version = Uint.zero) ?(authorised = S.empty) name =
    { counter ; version ; name ; authorised }

  let equal a b =
    name_equal a.name b.name && S.equal a.authorised b.authorised

  let add t id = { t with authorised = S.add id t.authorised }

  let remove t id = { t with authorised = S.remove id t.authorised }

  let prep t =
    let carry, counter = Uint.succ t.counter in
    { t with counter }, carry

  (*BISECT-IGNORE-BEGIN*)
  let pp_authorised ppf x = pp_list pp_id ppf (List.sort String.compare (S.elements x))

  let pp_authorisation ppf d =
    Format.fprintf ppf "authorisation #%s %a@ %a"
      (Uint.to_string d.counter) pp_name d.name pp_authorised d.authorised
  (*BISECT-IGNORE-END*)
end

module Releases = struct
  type t = {
    counter : Uint.t ;
    version : Uint.t ;
    name : name ;
    releases : S.t ;
  }

  let releases ?(counter = Uint.zero) ?(version = Uint.zero) ?(releases = S.empty) name =
    let is_release a = match Conex_opam_layout.authorisation_of_item a with
      | Some x -> name_equal name x
      | _ -> false
    in
    if S.for_all is_release releases then
      Ok { counter ; version ; name ; releases }
    else
      Error "all releases must have the same package name"

  let equal a b =
    name_equal a.name b.name && S.equal a.releases b.releases

  let add t i = { t with releases = S.add i t.releases }

  let remove t i = { t with releases = S.remove i t.releases }

  let prep t =
    let carry, counter = Uint.succ t.counter in
    { t with counter }, carry

  (*BISECT-IGNORE-BEGIN*)
  let pp_releases ppf r =
    Format.fprintf ppf "releases #%s %a@ %a"
       (Uint.to_string r.counter) pp_name r.name
      (pp_list pp_name) (S.elements r.releases)
  (*BISECT-IGNORE-END*)
end

module Checksum = struct
  type c = {
    filename : name ;
    bytesize : Uint.t ;
    checksum : digest ;
  }

  (*BISECT-IGNORE-BEGIN*)
  let pp_checksum ppf c =
    Format.fprintf ppf "%a (0x%s bytes) %a"
      pp_name c.filename (Uint.to_string c.bytesize) pp_digest c.checksum
  (*BISECT-IGNORE-END*)

  let checksum_equal a b =
    name_equal a.filename b.filename && a.bytesize = b.bytesize && a.checksum = b.checksum

  let checksum filename data =
    let bytesize = Uint.of_int (String.length data)
    and checksum = Conex_nocrypto.digest data
    in
    { filename ; bytesize ; checksum }

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
    counter : Uint.t ;
    version : Uint.t ;
    name : name ;
    files : checksum_map ;
  }

  (*BISECT-IGNORE-BEGIN*)
  let pp_checksums ppf c =
    Format.fprintf ppf "checksums #%s %a@ %a"
      (Uint.to_string c.counter)
      pp_name c.name
      pp_checksum_map c.files
  (*BISECT-IGNORE-END*)

  let checksums ?(counter = Uint.zero) ?(version = Uint.zero) name files =
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

  let equal a b =
    match compare_checksums a b with
    | Ok () -> true
    | _ -> false

  let prep t =
    let carry, counter = Uint.succ t.counter in
    { t with counter }, carry

end

module Index = struct
  type r = {
    index : Uint.t ;
    rname : string ;
    size : Uint.t ;
    resource : resource ;
    digest : digest ;
  }

  let r index rname size resource digest =
    { index ; rname ; size ; resource ; digest }

  let r_equal a b =
    Uint.compare a.index b.index = 0 &&
    a.rname = b.rname &&
    Uint.compare a.size b.size = 0 &&
    resource_equal a.resource b.resource &&
    a.digest = b.digest

  (*BISECT-IGNORE-BEGIN*)
  let pp_resource ppf { index ; rname ; size ; resource ; digest } =
    Format.fprintf ppf "%a #%s %a@ 0x%s bytes@ %a"
      pp_resource resource
      (Uint.to_string index)
      pp_name rname
      (Uint.to_string size)
      pp_digest digest
  (*BISECT-IGNORE-END*)

  type t = {
    counter : Uint.t ;
    version : Uint.t ;
    name : identifier ;
    resources : r list ;
    signatures : Signature.t list ;
    queued : r list ;
  }

  let index ?(counter = Uint.zero) ?(version = Uint.zero) ?(resources = []) ?(signatures = []) ?(queued = []) name =
    { counter ; version ; name ; resources ; signatures ; queued }

  let equal a b =
    id_equal a.name b.name &&
    List.length a.resources = List.length b.resources &&
    List.length a.queued = List.length b.queued &&
    List.for_all (fun r -> List.exists (r_equal r) a.resources) b.resources &&
    List.for_all (fun r -> List.exists (r_equal r) a.queued) b.queued

  let next_id idx =
    let max =
      let rs = List.map (fun r -> r.index) (idx.resources @ idx.queued) in
      List.fold_left max Uint.zero rs
    in
    let _c, counter = Uint.succ max in
    counter

  let add_resource t r = { t with queued = r :: t.queued }

  let reset t = { t with queued = [] }

  (*BISECT-IGNORE-BEGIN*)
  let pp_index ppf i =
    Format.fprintf ppf "index #%s %a@ resources %a@ queued %a@ signatures %a"
      (Uint.to_string i.counter)
      pp_id i.name
      (pp_list pp_resource) i.resources
      (pp_list pp_resource) i.queued
      (pp_list Signature.pp_signature) i.signatures
  (*BISECT-IGNORE-END*)

  let prep_sig i =
    let signatures = []
    and resources = i.resources @ i.queued
    and queued = []
    and carry, counter = Uint.succ i.counter
    in
    { i with signatures ; resources ; queued ; counter }, carry

  let add_sig i s = { i with signatures = s :: i.signatures }
end
