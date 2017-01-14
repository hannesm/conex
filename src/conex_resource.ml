open Conex_result
open Conex_core
open Conex_utils

module Signature = struct
  type t = identifier * Uint.t * string

  let extend_data data id ts =
    String.concat " " [ data ; id ; Uint.to_string ts ]

  (*BISECT-IGNORE-BEGIN*)
  let pp_signature ppf (id, ts, s) =
    Format.fprintf ppf "signature %a created at %s@ sig:@ %s" pp_id id (Uint.to_string ts) s
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
    | `Email e -> Format.fprintf ppf "email: %s" e
    | `GitHub e -> Format.fprintf ppf "GitHub: %s" e
    | `Other (k, v) -> Format.fprintf ppf "other %s: %s" k v

  let pp_publickey ppf p =
    let pp_opt_key ppf k =
      Format.pp_print_string ppf
        (match k with None -> "none" | Some (`Pub x) -> x)
    in
    Format.fprintf ppf "publickey name: %a@ accounts: %a@ counter: %s@ key: %a"
      pp_id p.name
      (pp_list pp_service) p.accounts
      (Uint.to_string p.counter)
      pp_opt_key p.key
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

  let add t id =
    if S.mem id t.members then
      t
    else
      let _c, counter = Uint.succ t.counter in
      { t with counter ; members = S.add id t.members }

  let remove t id =
    if S.mem id t.members then
      let _c, counter = Uint.succ t.counter in
      { t with counter ; members = S.remove id t.members }
    else
      t

  (*BISECT-IGNORE-BEGIN*)
  let pp_mems ppf x = pp_list pp_id ppf (List.sort String.compare (S.elements x))

  let pp_team ppf x =
    Format.fprintf ppf "team name: %a@ counter: %s@ members:@ %a"
      pp_id x.name (Uint.to_string x.counter) pp_mems x.members
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

  let add t id =
    if S.mem id t.authorised then
      t
    else
      let _c, counter = Uint.succ t.counter in
      { t with counter ; authorised = S.add id t.authorised }

  let remove t id =
    if S.mem id t.authorised then
      let _c, counter = Uint.succ t.counter in
      { t with counter ; authorised = S.remove id t.authorised }
    else
      t

  (*BISECT-IGNORE-BEGIN*)
  let pp_authorised ppf x = pp_list pp_id ppf (List.sort String.compare (S.elements x))

  let pp_authorisation ppf d =
    Format.fprintf ppf "authorisation name: %a@ counter: %s@ authorised:@ %a"
      pp_name d.name (Uint.to_string d.counter) pp_authorised d.authorised
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

  (*BISECT-IGNORE-BEGIN*)
  let pp_releases ppf r =
    Format.fprintf ppf "releases name: %a@ counter %s@ releases %a"
      pp_name r.name (Uint.to_string r.counter)
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
    Format.fprintf ppf "checksum %a [%s bytes]: %a@ "
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
    Format.fprintf ppf "checksums for %a (counter %s) are: %a"
      pp_name c.name
      (Uint.to_string c.counter)
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

  (*BISECT-IGNORE-BEGIN*)
  let pp_resource ppf { index ; rname ; size ; resource ; digest } =
    Format.fprintf ppf "resource %s@ name: %a@ size: %s@ resource: %a@ digest: %a"
      (Uint.to_string index) pp_name rname (Uint.to_string size)
      pp_resource resource
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

  let next_id idx =
    let max = List.fold_left max Uint.zero (List.map (fun r -> r.index) idx.resources) in
    let _c, counter = Uint.succ max in
    counter

  let add_resource t r = { t with queued = r :: t.queued }

  let reset t = { t with queued = [] }

  (*BISECT-IGNORE-BEGIN*)
  let pp_index ppf i =
    Format.fprintf ppf "index name: %a counter: %s resources:@ %a@ queued:@ %a@ signatures: %a"
      pp_id i.name
      (Uint.to_string i.counter)
      (pp_list pp_resource) i.resources
      (pp_list pp_resource) i.queued
      (pp_list Signature.pp_signature) i.signatures
  (*BISECT-IGNORE-END*)

  let prep_sig i =
    let signatures = []
    and resources = i.resources @ i.queued
    and queued = []
    and counter = snd (Uint.succ i.counter)
    in
    { i with signatures ; resources ; queued ; counter }

  let add_sig i s = { i with signatures = s :: i.signatures }
end
