open Core

module Signature = struct
  type t = identifier * int64 * string

  let extend_data data id ts =
    String.concat " " [ data ; id ; Int64.to_string ts ]

  (*BISECT-IGNORE-BEGIN*)
  let pp_signature ppf (id, ts, s) =
    Format.fprintf ppf "%a created at %Lu@ sig:@ %s" pp_id id ts s
  (*BISECT-IGNORE-END*)
end

module Publickey = struct
  type t = {
    counter : int64 ;
    version : int64 ;
    keyid : identifier ;
    key : pub option ;
  }

  let publickey ?(counter = 0L) ?(version = 0L) keyid key =
    { counter ; version ; keyid ; key }

  (*BISECT-IGNORE-BEGIN*)
  let pp_publickey ppf p =
    let pp_opt_key ppf k =
      Format.pp_print_string ppf
        (match k with None -> "none" | Some (`Pub x) -> x)
    in
    Format.fprintf ppf "keyid: %a@ counter: %Lu@ key: %a@."
      pp_id p.keyid
      p.counter
      pp_opt_key p.key
  (*BISECT-IGNORE-END*)
end

module Team = struct
  type t = {
    counter : int64 ;
    version : int64 ;
    name : identifier ;
    members : S.t
  }

  let team ?(counter = 0L) ?(version = 0L) ?(members = S.empty) name =
    { counter ; version ; members ; name }

  let add t id =
    if S.mem id t.members then
      t
    else
      { t with counter = Int64.succ t.counter ; members = S.add id t.members }

  let remove t id =
    if S.mem id t.members then
      { t with counter = Int64.succ t.counter ; members = S.remove id t.members }
    else
      t

  (*BISECT-IGNORE-BEGIN*)
  let pp_mems ppf x = pp_list pp_id ppf (List.sort String.compare (S.elements x))

  let pp_team ppf x =
    Format.fprintf ppf "team name: %a@ counter: %Lu@ members:@ %a@."
      pp_id x.name x.counter pp_mems x.members
   (*BISECT-IGNORE-END*)
end

module Authorisation = struct
  type t = {
    counter : int64 ;
    version : int64 ;
    name : name ;
    authorised : S.t ;
  }

  let authorisation
      ?(counter = 0L) ?(version = 0L) ?(authorised = S.empty) name =
    { counter ; version ; name ; authorised }

  let add t id =
    if S.mem id t.authorised then
      t
    else
      { t with counter = Int64.succ t.counter ;
               authorised = S.add id t.authorised }

  let remove t id =
    if S.mem id t.authorised then
      { t with counter = Int64.succ t.counter ;
               authorised = S.remove id t.authorised }
    else
      t

  (*BISECT-IGNORE-BEGIN*)
  let pp_authorised ppf x = pp_list pp_id ppf (List.sort String.compare (S.elements x))

  let pp_authorisation ppf d =
    Format.fprintf ppf "authorisation name: %a@ counter: %Lu@ authorised:@ %a@."
      pp_name d.name d.counter pp_authorised d.authorised
  (*BISECT-IGNORE-END*)
end

module Releases = struct
  type t = {
    counter : int64 ;
    version : int64 ;
    name : name ;
    releases : S.t ;
  }

  let releases ?(counter = 0L) ?(version = 0L) ?(releases = S.empty) name =
    let is_release a = match Layout.authorisation_of_item a with
      | Some x -> name_equal name x
      | _ -> false
    in
    if S.for_all is_release releases then
      Ok { counter ; version ; name ; releases }
    else
      Error "all releases must have the same package name"

  (*BISECT-IGNORE-BEGIN*)
  let pp_releases ppf r =
    Format.fprintf ppf "name: %a@ counter %Lu@ releases %a@."
      pp_name r.name r.counter
      (pp_list pp_name) (S.elements r.releases)
  (*BISECT-IGNORE-END*)
end

module Checksum = struct
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
end

module Index = struct
  type t = {
    counter : int64 ;
    version : int64 ;
    identifier : identifier ;
    resources : (name * resource * digest) list ; (* should be a Set? should include file size for better error reporting? *)
    signatures : Signature.t list ;
  }

  let index ?(counter = 0L) ?(version = 0L) ?(resources = []) ?(signatures = []) identifier =
    { counter ; version ; identifier ; resources ; signatures }

  let add_resource t r =
    { t with resources = r :: t.resources ; counter = Int64.succ t.counter }

  let add_resources t rs =
    { t with resources = rs @ t.resources ; counter = Int64.succ t.counter }

  (*BISECT-IGNORE-BEGIN*)
  let pp_resource ppf (n, r, digest) =
    Format.fprintf ppf "name: %a@ resource: %a@ digest: %a@."
      pp_name n pp_resource r pp_digest digest

  let pp_index ppf i =
    Format.fprintf ppf "identifier: %a@ counter: %Lu@ resources:@ %a@ %a@."
      pp_id i.identifier
      i.counter
      (pp_list pp_resource) i.resources
      (pp_list Signature.pp_signature) i.signatures
  (*BISECT-IGNORE-END*)

  let add_sig i s = { i with signatures = s :: i.signatures }

  let replace_sig i s = { i with signatures = [ s ] }
end
