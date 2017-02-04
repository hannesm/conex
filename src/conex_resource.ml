open Conex_result
open Conex_core
open Conex_utils

module Wire = struct
  type s =
  | Map of s M.t
  | List of s list
  | String of string
  | Int of Uint.t
  type t = s M.t

  let search t k =
    try Some (M.find k t) with Not_found -> None

  let check_v expect v =
    if Uint.compare v expect = 0 then
      Ok ()
    else
      Error (Printf.sprintf "unknown data version #%s, expected #%s"
               (Uint.to_string v) (Uint.to_string expect))

  let opt_err = function
    | Some x -> Ok x
    | None -> Error "expected some, got none"

  let string = function
    | String x -> Ok x
    | _ -> Error "couldn't find string"

  let int = function
    | Int x -> Ok x
    | _ -> Error "couldn't find int"

  let list = function
    | List x -> Ok x
    | _ -> Error "couldn't find list"

  let map = function
    | Map m -> Ok m
    | _ -> Error "couldn't find map"

  let opt_map = function
    | None -> Ok M.empty
    | Some x -> map x

  let string_set els =
    foldM (fun acc e ->
        match string e with
        | Ok s -> Ok (s :: acc)
        | _ -> Error ("not a string while parsing list"))
      [] els >>= fun els ->
    Ok (s_of_list els)

  let wire_string_set s = List (List.map (fun s -> String s) (S.elements s))

  let opt_list = function
    | None -> Ok []
    | Some xs -> list xs

  let opt_string_set x = opt_list x >>= string_set


  let ncv data =
    opt_err (search data "counter") >>= int >>= fun counter ->
    opt_err (search data "version") >>= int >>= fun version ->
    opt_err (search data "name") >>= string >>= fun name ->
    Ok (name, counter, version)

  let keys ncv additional map =
    let wanted =
      if ncv then
        "counter" :: "version" :: "name" :: additional
      else
        additional
    in
    if S.subset (s_of_list (fst (List.split (M.bindings map)))) (s_of_list wanted) then
      Ok ()
    else
      Error "key sets not compatible"

  let wire_ncv n c v =
    M.add "counter" (Int c)
      (M.add "name" (String n)
         (M.add "version" (Int v)
            M.empty))

  let digest data =
    list data >>= function
    | [ String typ ; String data ] ->
      (match string_to_digestalg typ with
       | Some `SHA256 -> Ok (`SHA256, data)
       | None -> Error ("unknown digest typ " ^ typ))
    | _ -> Error "couldn't parse digest"

  let wire_digest (typ, data) =
    List [ String (digestalg_to_string typ) ; String data ]

  let signature_of_wire data =
    list data >>= function
    | [ Int created ; String typ ; String signame ; String d ] ->
      (match string_to_sigalg typ with
       (* TODO: length check for `d`? *)
       | Some `RSA_PSS_SHA256 -> Ok ({ created ; sigalg = `RSA_PSS_SHA256 ; signame }, d)
       | None -> Error "couldn't parse signature type")
    | _ -> Error "couldn't parse signature"

  let wire_signature (hdr, s) =
    [ Int hdr.created ; String (sigalg_to_string hdr.sigalg) ; String hdr.signame ; String s ]

  let key_of_wire data =
    list data >>= function
    | [ String typ ; String data ] ->
      (match string_to_keyalg typ with
       | Some `RSA -> Ok (`RSA, data)
       | _ -> Error "unknown key type")
    | _ -> Error "unknown key"

  let wire_key (t,k) =
    let typ = keyalg_to_string t in
    List [ String typ ; String k ]

  (* this is exposed *)
  let wire_pub id k =
    M.add "key" (wire_key k)
      (M.add "name" (String id) M.empty)
end

module Team = struct
  let version = Uint.zero
  type t = {
    counter : Uint.t ;
    name : identifier ;
    members : S.t
  }

  let team ?(counter = Uint.zero) ?(members = S.empty) name =
    { counter ; members ; name }

  let of_wire data =
    let open Wire in
    keys true ["members"] data >>= fun () ->
    ncv data >>= fun (name, counter, v) ->
    check_v version v >>= fun () ->
    opt_string_set (search data "members") >>= fun members ->
    Ok (team ~counter ~members name)

  let wire d =
    let open Wire in
    M.add "members" (wire_string_set d.members)
      (wire_ncv d.name d.counter version)

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
  let version = Uint.zero
  type t = {
    counter : Uint.t ;
    name : name ;
    authorised : S.t ;
  }

  let authorisation ?(counter = Uint.zero) ?(authorised = S.empty) name =
    { counter ; name ; authorised }

  let of_wire data =
    let open Wire in
    keys true ["authorised"] data >>= fun () ->
    ncv data >>= fun (name, counter, v) ->
    check_v version v >>= fun () ->
    opt_string_set (search data "authorised") >>= fun authorised ->
    Ok (authorisation ~counter ~authorised name)

  let wire d =
    let open Wire in
    M.add "authorised" (wire_string_set d.authorised)
      (wire_ncv d.name d.counter version)

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
  let version = Uint.zero
  type t = {
    counter : Uint.t ;
    name : name ;
    releases : S.t ;
  }

  let releases ?(counter = Uint.zero) ?(releases = S.empty) name =
    (* TODO: this feels wrong here -- rather do it in verify (it's the only resource constructur which may fail) *)
    let is_release a = match Conex_opam_repository_layout.authorisation_of_item a with
      | Some x -> name_equal name x
      | _ -> false
    in
    if S.for_all is_release releases then
      Ok { counter ; name ; releases }
    else
      Error "all releases must have the same package name"

  let of_wire data =
    let open Wire in
    keys true ["releases"] data >>= fun () ->
    ncv data >>= fun (name, counter, v) ->
    check_v version v >>= fun () ->
    opt_string_set (search data "releases") >>= fun rels ->
    releases ~counter ~releases:rels name

  let wire r =
    let open Wire in
    M.add "releases" (wire_string_set r.releases)
      (wire_ncv r.name r.counter version)

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
    size     : Uint.t ;
    digest   : digest ;
  }

  (*BISECT-IGNORE-BEGIN*)
  let pp_checksum ppf c =
    Format.fprintf ppf "%a (0x%s bytes) %a"
      pp_name c.filename (Uint.to_string c.size) pp_digest c.digest
  (*BISECT-IGNORE-END*)

  let checksum_equal a b =
    name_equal a.filename b.filename && a.size = b.size && digest_eq a.digest b.digest

  let checksum_of_wire data =
    let open Wire in
    map data >>= fun map ->
    keys false ["filename" ; "size" ; "digest"] map >>= fun () ->
    opt_err (search map "filename") >>= string >>= fun filename ->
    opt_err (search map "size") >>= int >>= fun size ->
    opt_err (search map "digest") >>= digest >>= function digest ->
    Ok ({ filename ; size ; digest })

  let wire_checksum c =
    let open Wire in
    M.add "filename" (String c.filename)
      (M.add "size" (Int c.size)
         (M.add "digest" (wire_digest c.digest)
            M.empty))

  type checksum_map = c M.t

  let fold f m acc = M.fold (fun _ c acc -> f c acc) m acc
  let find m id = M.find id m

  (*BISECT-IGNORE-BEGIN*)
  let pp_checksum_map ppf cs =
    pp_list pp_checksum ppf
      (List.sort (fun a b -> String.compare a.filename b.filename)
         (snd (List.split (M.bindings cs))))
  (*BISECT-IGNORE-END*)

  let version = Uint.zero
  type t = {
    counter : Uint.t ;
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

  let checksums ?(counter = Uint.zero) name files =
    let files = List.fold_left (fun m f -> M.add f.filename f m) M.empty files in
    { counter ; name ; files }

  let of_wire data =
    let open Wire in
    keys true ["files"] data >>= fun () ->
    ncv data >>= fun (name, counter, v) ->
    check_v version v >>= fun () ->
    opt_list (search data "files") >>= fun sums ->
    foldM (fun acc v -> checksum_of_wire v >>= fun cs -> Ok (cs :: acc))
      [] sums >>= fun files ->
    Ok (checksums ~counter name files)

  let wire cs =
    let open Wire in
    let csums =
      fold (fun c acc -> Map (wire_checksum c) :: acc) cs.files []
    in
    M.add "files" (List csums)
      (wire_ncv cs.name cs.counter version)

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

  let resource_of_wire data =
    let open Wire in
    map data >>= fun map ->
    keys false ["index" ; "name" ; "size" ; "resource" ; "digest" ] map >>= fun () ->
    opt_err (search map "index") >>= int >>= fun index ->
    opt_err (search map "name") >>= string >>= fun name ->
    opt_err (search map "size") >>= int >>= fun size ->
    opt_err (search map "resource") >>= string >>= fun res ->
    (match string_to_resource res with
     | Some r -> Ok r
     | None -> Error "unknown resource") >>= fun resource ->
    opt_err (search map "digest") >>= digest >>= fun digest ->
    Ok (r index name size resource digest)

  let wire_resource r =
    let open Wire in
    M.add "index" (Int r.index)
      (M.add "name" (String r.rname)
         (M.add "size" (Int r.size)
            (M.add "resource" (String (resource_to_string r.resource))
               (M.add "digest" (wire_digest r.digest)
                  M.empty))))

  let r_equal a b =
    name_equal a.rname b.rname &&
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

  type email = identifier

  type service = [
    | `Email of email
    | `GitHub of identifier
    | `Other of identifier * string
  ]

  let accounts_of_wire map =
    M.fold (fun k v acc ->
        acc >>= fun xs ->
        Wire.string v >>= fun s ->
        let data =
          match k with
          | "email" -> `Email s
          | "github" -> `GitHub s
          | x -> `Other (x, s)
        in
        Ok (data :: xs))
      map (Ok [])

  let wire_account m =
    let open Wire in
    function
    | `Email e -> M.add "email" (String e) m
    | `GitHub g -> M.add "github" (String g) m
    | `Other (k, v) -> M.add k (String v) m

  let service_equal a b = match a, b with
    | `Email a, `Email b -> id_equal a b
    | `GitHub a, `GitHub b -> id_equal a b
    | `Other (a, b), `Other (c, d) -> id_equal a c && String.compare b d = 0
    | _ -> false

  (*BISECT-IGNORE-BEGIN*)
  let pp_service ppf = function
    | `Email e -> Format.fprintf ppf "email %s" e
    | `GitHub e -> Format.fprintf ppf "GitHub %s" e
    | `Other (k, v) -> Format.fprintf ppf "%s %s" k v
  (*BISECT-IGNORE-END*)

  let version = Uint.zero
  type t = {
    accounts : service list ;
    keys : pub list ;
    counter : Uint.t ;
    name : identifier ;
    resources : r list ;
    signatures : signature list ;
    queued : r list ;
  }

  let index ?(accounts = []) ?(keys = []) ?(counter = Uint.zero) ?(resources = []) ?(signatures = []) ?(queued = []) name =
    { accounts ; keys ; counter ; name ; resources ; signatures ; queued }

  let of_wire data =
    let open Wire in
    keys false ["signatures" ; "signed" ; "queued" ; "keys" ; "accounts" ] data >>= fun () ->
    opt_list (search data "signatures") >>= fun sigs ->
    foldM (fun acc v -> signature_of_wire v >>= fun s -> Ok (s :: acc)) [] sigs >>= fun signatures ->
    opt_err (search data "signed") >>= map >>= fun signed ->
    keys true ["resources"] signed >>= fun () ->
    ncv signed >>= fun (name, counter, v) ->
    check_v version v >>= fun () ->
    opt_list (search signed "resources") >>= fun rs ->
    foldM (fun acc v -> resource_of_wire v >>= fun r -> Ok (r :: acc)) [] rs >>= fun resources ->
    opt_list (search data "queued") >>= fun qs ->
    foldM (fun acc v -> resource_of_wire v >>= fun r -> Ok (r :: acc)) [] qs >>= fun queued ->
    opt_list (search data "keys") >>= fun keys ->
    foldM (fun acc k -> key_of_wire k >>= fun r -> Ok (r :: acc)) [] keys >>= fun keys ->
    opt_map (search data "accounts") >>= accounts_of_wire >>= fun accounts ->
    Ok (index ~keys ~accounts ~counter ~resources ~queued ~signatures name)

  let wire_resources i =
    let open Wire in
    let resources = List.map (fun r -> Map (wire_resource r)) i.resources in
    M.add "resources" (List resources)
      (wire_ncv i.name i.counter version)

  let wire i =
    let open Wire in
    M.add "keys" (List (List.map wire_key i.keys))
      (M.add "accounts" (Map (List.fold_left wire_account M.empty i.accounts))
         (M.add "queued" (List (List.map (fun r -> Map (wire_resource r)) i.queued))
            (M.add "signed" (Map (wire_resources i))
               (M.add "signatures" (List (List.map (fun s -> List (wire_signature s)) i.signatures))
                  M.empty))))

  let equal a b =
    id_equal a.name b.name &&
    List.length a.accounts = List.length b.accounts &&
    List.length a.keys = List.length b.keys &&
    List.length a.resources = List.length b.resources &&
    List.length a.queued = List.length b.queued &&
    List.for_all (fun r -> List.exists (service_equal r) a.accounts) b.accounts &&
    List.for_all (fun r -> List.exists (pub_equal r) a.keys) b.keys &&
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
    Format.fprintf ppf "index #%s %a@ accounts %a@ keys %a@ resources %a@ queued %a@ signatures %a"
      (Uint.to_string i.counter)
      pp_id i.name
      (pp_list pp_service) i.accounts
      (pp_list pp_pub) i.keys
      (pp_list pp_resource) i.resources
      (pp_list pp_resource) i.queued
      (pp_list pp_signature) i.signatures
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
