open Conex_utils


type name = string

(*BISECT-IGNORE-BEGIN*)
let pp_name ppf x = Format.pp_print_string ppf x
(*BISECT-IGNORE-END*)

let name_equal a b = String.compare_insensitive a b = 0


type identifier = string

(*BISECT-IGNORE-BEGIN*)
let pp_id ppf x = Format.pp_print_string ppf x
(*BISECT-IGNORE-END*)

let id_equal a b = String.compare_insensitive a b = 0

module Wire = struct
  type s =
    | Map of s M.t
    | List of s list
    | String of string
    | Int of Uint.t

  let rec s_to_string = function
    | Int x -> "0x" ^ Uint.to_string x
    | String s -> "'"^s^"'"
    | List xs -> "[" ^ String.concat "; " (List.map s_to_string xs) ^ "]"
    | Map m ->
      let strs =
        let bindings = M.bindings m in
        let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) bindings in
        List.map (fun (k, v) -> k ^ ": " ^ s_to_string v) sorted
      in
      "{" ^ String.concat "KEY" strs ^ "}"

  type t = s M.t

  let to_string t = s_to_string (Map t)

  let search t k =
    try Some (M.find k t) with Not_found -> None

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

  let wire_string_set s =
    List (List.map (fun s -> String s) (List.sort String.compare (S.elements s)))

  let opt_list = function
    | None -> Ok []
    | Some xs -> list xs

  let opt_string_set x = opt_list x >>= string_set
end

(* they're by no means equal:

  - `Author, `Team, `Authorisation, `Package, `Release are written to individual files
  - `Account, `Signature, and `Key are persistent via `Author
  - `Wrap is writen implicitly with Header.t
  - `Key, `Account, `Author, `Wrap, `Team, `Authorisation, `Package, `Release can be part of resource lists
  - `Signature is never part of any resource list!
 *)
type typ = [
  | `Signature
  | `Key
  | `Account
  | `Author
  | `Wrap
  | `Team
  | `Authorisation
  | `Package
  | `Release
]

let typ_equal a b = match a, b with
  | `Signature, `Signature
  | `Key, `Key
  | `Account, `Account
  | `Author, `Author
  | `Wrap, `Wrap
  | `Team, `Team
  | `Authorisation, `Authorisation
  | `Package, `Package
  | `Release, `Release -> true
  | _ -> false

let typ_to_string = function
  | `Signature -> "signature"
  | `Key -> "key"
  | `Account -> "account"
  | `Author -> "author"
  | `Wrap -> "wrap"
  | `Team -> "team"
  | `Authorisation -> "authorisation"
  | `Package -> "package"
  | `Release -> "release"

let string_to_typ = function
  (*  | "signature" -> Some `Signature -- as mentioned earlier, we'll never read a signature *)
  | "key" -> Some `Key
  | "account" -> Some `Account
  | "author" -> Some `Author
  | "wrap" -> Some `Wrap
  | "team" -> Some `Team
  | "authorisation" -> Some `Authorisation
  | "package" -> Some `Package
  | "release" -> Some `Release
  | _ -> None

(*BISECT-IGNORE-BEGIN*)
let pp_typ ppf typ =
  Format.fprintf ppf (match typ with
      | `Signature -> "signature"
      | `Key -> "key"
      | `Account -> "account"
      | `Author -> "author"
      | `Wrap -> "wrap"
      | `Team -> "team"
      | `Authorisation -> "authorisation"
      | `Package -> "package index"
      | `Release -> "release")
(*BISECT-IGNORE-END*)

let wire_typ typ = Wire.String (typ_to_string typ)
let typ_of_wire = function
  | Wire.String str ->
    (match string_to_typ str with
     | None -> Error "unknown resource type"
     | Some x -> Ok x)
  | _ -> Error "cannot parse resource type"


module Header = struct
  type t = {
    version : Uint.t ;
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : name ;
    typ : typ
  }

  let of_wire data =
    let open Wire in
    opt_err (search data "version") >>= int >>= fun version ->
    opt_err (search data "created") >>= int >>= fun created ->
    opt_err (search data "counter") >>= int >>= fun counter ->
    opt_err (search data "wraps") >>= int >>= fun wraps ->
    opt_err (search data "name") >>= string >>= fun name ->
    opt_err (search data "typ") >>= typ_of_wire >>= fun typ ->
    Ok { version ; created ; counter ; wraps ; name ; typ }

  (*BISECT-IGNORE-BEGIN*)
  let timestamp x = Uint.decimal x

  let counter x wrap =
    "#" ^ (Uint.decimal x) ^
    (if Uint.compare wrap Uint.zero = 0 then ""
     else "[" ^ (Uint.decimal wrap) ^ "]")

  let pp ppf hdr =
    Format.fprintf ppf "%a %a %s created %s"
      pp_typ hdr.typ
      pp_name hdr.name
      (counter hdr.counter hdr.wraps)
      (timestamp hdr.created)
  (*BISECT-IGNORE-END*)

  let wire t =
    let open Wire in
    M.add "version" (Int t.version)
      (M.add "created" (Int t.created)
         (M.add "counter" (Int t.counter)
            (M.add "wraps" (Int t.wraps)
               (M.add "name" (String t.name)
                  (M.add "typ" (wire_typ t.typ) M.empty)))))

  let keys ?(header = true) additional map =
    let wanted =
      if header then
        "created" :: "counter" :: "version" :: "wraps" :: "name" :: "typ" :: additional
      else
        additional
    in
    let have = s_of_list (fst (List.split (M.bindings map))) in
    if S.subset have (s_of_list wanted) then
      Ok ()
    else
      Error (Printf.sprintf "key sets not compatible: have %s want %s"
               (String.concat ";" (S.elements have)) (String.concat ";" wanted))

  let check t v hdr =
    match Uint.compare hdr.version v, typ_equal t hdr.typ with
    | 0, true -> Ok ()
    | _, false -> Error (Printf.sprintf "expected resource type %s, found %s"
                           (typ_to_string t) (typ_to_string hdr.typ))
    | _, true ->
      Error (Printf.sprintf "expected data version #%s, found #%s"
               (Uint.decimal v) (Uint.decimal hdr.version))

end


module Key = struct
  type alg = [ `RSA ]

  let alg_to_string = function `RSA -> "RSA"
  let string_to_alg = function "RSA" -> Some `RSA | _ -> None
  let alg_equal a b = match a, b with
    | `RSA, `RSA -> true

  let version = Uint.zero
  type t = alg * string * Uint.t

  type priv = [ `Priv of alg * string * Uint.t ]

  let equal (alg, data, _) (alg', data', _) =
    alg_equal alg alg' && String.compare data data' = 0

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf (a, x, created) =
    Format.fprintf ppf "%s key (created %s), %d bytes"
      (alg_to_string a) (Header.timestamp created) (String.length x)
  (*BISECT-IGNORE-END*)

  (* stored persistently on disk in an author file containing a list of keys *)
  let of_wire data =
    let open Wire in
    list data >>= function
    | [ String typ ; String data ; Int created ] ->
      (match string_to_alg typ with
       | Some `RSA -> Ok (`RSA, data, created)
       | _ -> Error "unknown key type")
    | _ -> Error "cannot parse key"

  let wire_raw (a, k, created) =
    let open Wire in
    let typ = alg_to_string a in
    List [ String typ ; String k ; Int created ]

  (* this is exposed, used for signing *)
  let wire name (a, k, created) =
    let open Wire in
    let counter = Uint.zero
    and wraps = Uint.zero
    and typ = `Key
    in
    let header = { Header.created ; counter ; version ; wraps ; name ; typ } in
    M.add "keytype" (String (alg_to_string a))
      (M.add "keydata" (String k)
         (Header.wire header))
end

module Signature = struct
  type alg = [ `RSA_PSS_SHA256 ]

  let alg_to_string = function
    | `RSA_PSS_SHA256 -> "RSA-PSS-SHA256"

  let string_to_alg = function
    | "RSA-PSS-SHA256" -> Some `RSA_PSS_SHA256
    | _ -> None

  let version = Uint.zero
  type hdr = alg * Uint.t

  (* to-be-signed data: an identifer, algorithm, timestamp, and the actual data *)
  let wire name (alg, created) data =
    let open Wire in
    let counter = Uint.zero
    and wraps = Uint.zero
    and typ = `Signature
    in
    let header = { Header.created ; counter ; version ; wraps ; name ; typ }
    in
    M.add "sigtype" (String (alg_to_string alg))
      (M.add "data" (String data)
         (Header.wire header))

  type t = hdr * string

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf ((alg, created), data) =
    Format.fprintf ppf "%s signature (created %s), %d bytes"
      (alg_to_string alg) (Header.timestamp created) (String.length data)
  (*BISECT-IGNORE-END*)

  (* again stored on disk as part of author file *)
  let of_wire data =
    let open Wire in
    list data >>= function
    | [ Int created ; String typ ; String s ] ->
      (match string_to_alg typ with
       | Some alg -> Ok ((alg, created), s)
       | None -> Error "couldn't parse signature type")
    | _ -> Error "couldn't parse signature"

  let wire_raw ((alg, created), s) =
    let open Wire in
    List [ Int created ; String (alg_to_string alg) ; String s ]
end

module Digest = struct
  type alg = [ `SHA256 ]
  let alg_to_string = function `SHA256 -> "SHA256"
  let string_to_alg = function
    | "SHA256" -> Some `SHA256
    | _ -> None

  type t = alg * string
  let to_string (a, b) = alg_to_string a ^ b
  let equal (ta, a) (tb, b) = match ta, tb with
  | `SHA256, `SHA256 -> String.compare a b = 0

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf (`SHA256, x) = Format.fprintf ppf "SHA256: %s" x
  (*BISECT-IGNORE-END*)

  let of_wire data =
    let open Wire in
    list data >>= function
    | [ String typ ; String data ] ->
      (match string_to_alg typ with
       | Some `SHA256 -> Ok (`SHA256, data)
       | None -> Error ("unknown digest typ " ^ typ))
    | _ -> Error "couldn't parse digest"

  let wire_raw (typ, data) =
    let open Wire in
    List [ String (alg_to_string typ) ; String data ]
end

module Author = struct
  type r = {
    index : Uint.t ;
    rname : string ;
    rtyp : typ ;
    digest : Digest.t ;
  }

  let r index rname rtyp digest =
    { index ; rname ; rtyp ; digest }

  let resource_of_wire data =
    let open Wire in
    map data >>= fun map ->
    Header.keys ~header:false ["index" ; "name" ; "typ" ; "digest" ] map >>= fun () ->
    opt_err (search map "index") >>= int >>= fun index ->
    opt_err (search map "name") >>= string >>= fun rname ->
    opt_err (search map "typ") >>= typ_of_wire >>= fun rtyp ->
    opt_err (search map "digest") >>= Digest.of_wire >>= fun digest ->
    Ok (r index rname rtyp digest)

  let wire_resource r =
    let open Wire in
    M.add "index" (Int r.index)
      (M.add "name" (String r.rname)
         (M.add "typ" (wire_typ r.rtyp)
            (M.add "digest" (Digest.wire_raw r.digest)
               M.empty)))

  let r_equal a b =
    name_equal a.rname b.rname &&
    typ_equal a.rtyp b.rtyp &&
    Digest.equal a.digest b.digest

  (*BISECT-IGNORE-BEGIN*)
  let pp_r ppf { index ; rname ; rtyp ; digest } =
    Format.fprintf ppf "%a #%s %a@ %a"
      pp_typ rtyp
      (Uint.decimal index)
      pp_name rname
      Digest.pp digest
  (*BISECT-IGNORE-END*)

  type email = identifier

  type account = [
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

  let wire_account_raw m =
    let open Wire in
    function
    | `Email e -> M.add "email" (String e) m
    | `GitHub g -> M.add "github" (String g) m
    | `Other (k, v) -> M.add k (String v) m

  let wire_account name a = wire_account_raw (M.add "name" (Wire.String name) M.empty) a

  let compare_account (a : account) (b : account) = match a, b with
    | `Email a, `Email b -> String.compare_insensitive a b
    | `GitHub a, `GitHub b -> String.compare_insensitive a b
    | `Other (a, v), `Other (a', v') ->
      let r = String.compare_insensitive a a' in
      if r = 0 then String.compare_insensitive v v'
      else r
    | `Email _, _ -> 1
    | _, `Email _ -> -1
    | `GitHub _, _ -> 1
    | _, `GitHub _ -> -1

  let account_equal a b = compare_account a b = 0

  (*BISECT-IGNORE-BEGIN*)
  let pp_account ppf = function
    | `Email e -> Format.fprintf ppf "email %s" e
    | `GitHub e -> Format.fprintf ppf "GitHub %s" e
    | `Other (k, v) -> Format.fprintf ppf "%s %s" k v
  (*BISECT-IGNORE-END*)

  let version = Uint.zero
  type t = {
    (* signed part *)
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : identifier ;
    resources : r list ;
    (* in raw outer shield *)
    accounts : account list ;
    keys : (Key.t * Signature.t) list ;
    queued : r list ;
  }

  let t ?(counter = Uint.zero) ?(wraps = Uint.zero) ?(accounts = []) ?(keys = []) ?(resources = []) ?(queued = []) created name =
    { created ; counter ; wraps ; name ; accounts ; keys ; resources ; queued }

  let contains ?(queued = false) author r =
    let xs = if queued then author.resources @ author.queued else author.resources in
    List.exists (r_equal r) xs

  (* wire representation is as follows:
      accounts: account list
      keys: pub * sig list
      queued: r list
      signed:
        Header.t with typ = `Author
        resources r list (including matching `Key and `Account above)
 *)
  let of_wire data =
    let open Wire in
    Header.keys ~header:false ["signed" ; "queued" ; "keys" ; "accounts" ] data >>= fun () ->
    opt_list (search data "keys") >>= fun keys ->
    foldM (fun acc d ->
        list d >>= function
        | [ key ; signature ] ->
          Key.of_wire key >>= fun key ->
          Signature.of_wire signature >>= fun signature ->
          Ok ((key, signature) :: acc)
        | _ -> Error "expected a key signature pair!")
      []
      keys >>= fun keys ->
    opt_err (search data "signed") >>= map >>= fun signed ->
    Header.keys ["resources"] signed >>= fun () ->
    Header.of_wire signed >>= fun h ->
    Header.check `Author version h >>= fun () ->
    opt_list (search signed "resources") >>= fun rs ->
    foldM (fun acc v -> resource_of_wire v >>= fun r -> Ok (r :: acc)) [] rs >>= fun resources ->
    opt_list (search data "queued") >>= fun qs ->
    foldM (fun acc v -> resource_of_wire v >>= fun r -> Ok (r :: acc)) [] qs >>= fun queued ->
    opt_map (search data "accounts") >>= accounts_of_wire >>= fun accounts ->
    Ok (t ~keys ~accounts ~counter:h.Header.counter ~wraps:h.Header.wraps ~resources ~queued h.Header.created h.Header.name)

  let wire_raw t =
    let open Wire in
    let created = t.created
    and counter = t.counter
    and wraps = t.wraps
    and name = t.name
    and typ = `Author
    in
    let header = { Header.version ; created ; counter ; wraps ; name ; typ } in
    let resources = List.map (fun r -> Map (wire_resource r)) (List.sort (fun a b -> Uint.compare a.index b.index) t.resources) in
    M.add "resources" (List resources) (Header.wire header)

  let wire i =
    let open Wire in
    M.add "keys" (List (List.map (fun (k, s) -> List [ Key.wire_raw k ; Signature.wire_raw s ])
                          (List.sort (fun ((_, _, c), _) ((_, _, c'), _) -> Uint.compare c c') i.keys)))
      (M.add "accounts" (Map (List.fold_left wire_account_raw M.empty (List.sort compare_account i.accounts)))
         (M.add "queued" (List (List.map (fun r -> Map (wire_resource r)) (List.sort (fun a b -> Uint.compare a.index b.index) i.queued)))
            (M.add "signed" (Map (wire_raw i))
               M.empty)))

  let equal a b =
    id_equal a.name b.name &&
    List.length a.accounts = List.length b.accounts &&
    List.length a.keys = List.length b.keys &&
    List.length a.resources = List.length b.resources &&
    List.length a.queued = List.length b.queued &&
    List.for_all (fun r -> List.exists (account_equal r) a.accounts) b.accounts &&
    List.for_all (fun (r, _) -> List.exists (fun (k, _) -> Key.equal k r) a.keys) b.keys &&
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
  let pp_ks ppf (k, s) =
    Format.fprintf ppf "key %a sig %a" Key.pp k Signature.pp s

  let pp ppf i =
    Format.fprintf ppf "author %a %s (created %s)@ accounts %a@ crypto %a@ resources %a@ queued %a"
      pp_id i.name
      (Header.counter i.counter i.wraps)
      (Header.timestamp i.created)
      (pp_list pp_account) i.accounts
      (pp_list pp_ks) i.keys
      (pp_list pp_r) i.resources
      (pp_list pp_r) i.queued
  (*BISECT-IGNORE-END*)

  let prep_sig i =
    let resources = i.resources @ i.queued
    and queued = []
    and carry, counter = Uint.succ i.counter
    in
    { i with resources ; queued ; counter }, carry

  let replace_sig i (k, s) =
    let keys = List.filter (fun (k', _) -> not (Key.equal k k')) i.keys in
    { i with keys = (k, s) :: keys }
end


module Team = struct
  let version = Uint.zero
  type t = {
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : identifier ;
    members : S.t
  }

  let t ?(counter = Uint.zero) ?(wraps = Uint.zero) ?(members = S.empty) created name =
    { created ; counter ; wraps ; members ; name }

  let of_wire data =
    let open Wire in
    Header.keys ["members"] data >>= fun () ->
    Header.of_wire data >>= fun h ->
    Header.check `Team version h >>= fun () ->
    opt_string_set (search data "members") >>= fun members ->
    Ok (t ~counter:h.Header.counter ~wraps:h.Header.wraps ~members h.Header.created h.Header.name)

  let wire t =
    let open Wire in
    let counter = t.counter
    and wraps = t.wraps
    and created = t.created
    and typ = `Team
    and name = t.name
    in
    let header = { Header.version ; created ; counter ; wraps ; name ; typ } in
    M.add "members" (wire_string_set t.members) (Header.wire header)

  let equal a b =
    id_equal a.name b.name && S.equal a.members b.members

  let add t id = { t with members = S.add id t.members }

  let remove t id = { t with members = S.remove id t.members }

  let prep t =
    let carry, counter = Uint.succ t.counter in
    { t with counter }, carry

  (*BISECT-IGNORE-BEGIN*)
  let pp_mems ppf x = pp_list pp_id ppf (List.sort String.compare_insensitive (S.elements x))

  let pp ppf x =
    Format.fprintf ppf "team %a %s (created %s)@ %a"
      pp_id x.name
      (Header.counter x.counter x.wraps)
      (Header.timestamp x.created)
      pp_mems x.members
   (*BISECT-IGNORE-END*)
end

module Authorisation = struct
  let version = Uint.zero
  type t = {
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : name ;
    authorised : S.t ;
  }

  let t ?(counter = Uint.zero) ?(wraps = Uint.zero) ?(authorised = S.empty) created name =
    { created ; counter ; wraps ; name ; authorised }

  let of_wire data =
    let open Wire in
    Header.keys ["authorised"] data >>= fun () ->
    Header.of_wire data >>= fun h ->
    Header.check `Authorisation version h >>= fun () ->
    opt_string_set (search data "authorised") >>= fun authorised ->
    Ok (t ~counter:h.Header.counter ~wraps:h.Header.wraps ~authorised h.Header.created h.Header.name)

  let wire d =
    let open Wire in
    let created = d.created
    and counter = d.counter
    and wraps = d.wraps
    and name = d.name
    and typ = `Authorisation
    in
    let header = { Header.version ;created ; counter ; wraps ; name ; typ } in
    M.add "authorised" (wire_string_set d.authorised) (Header.wire header)

  let equal a b =
    name_equal a.name b.name && S.equal a.authorised b.authorised

  let add t id = { t with authorised = S.add id t.authorised }

  let remove t id = { t with authorised = S.remove id t.authorised }

  let prep t =
    let carry, counter = Uint.succ t.counter in
    { t with counter }, carry

  (*BISECT-IGNORE-BEGIN*)
  let pp_authorised ppf x = pp_list pp_id ppf (List.sort String.compare_insensitive (S.elements x))

  let pp ppf d =
    Format.fprintf ppf "authorisation %a %s (created %s)@ %a"
      pp_name d.name
      (Header.counter d.counter d.wraps)
      (Header.timestamp d.created)
      pp_authorised d.authorised
  (*BISECT-IGNORE-END*)
end

module Package = struct
  let version = Uint.zero
  type t = {
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : name ;
    releases : S.t ;
  }

  let t ?(counter = Uint.zero) ?(wraps = Uint.zero) ?(releases = S.empty) created name =
    { created ; counter ; wraps ; name ; releases }

  let of_wire data =
    let open Wire in
    Header.keys ["releases"] data >>= fun () ->
    Header.of_wire data >>= fun h ->
    Header.check `Package version h >>= fun () ->
    opt_string_set (search data "releases") >>= fun rels ->
    Ok (t ~counter:h.Header.counter ~wraps:h.Header.wraps ~releases:rels h.Header.created h.Header.name)

  let wire r =
    let open Wire in
    let counter = r.counter
    and wraps = r.wraps
    and created = r.created
    and typ = `Package
    and name = r.name
    in
    let header = { Header.version ; created ; counter ; wraps ; name ; typ } in
    M.add "releases" (wire_string_set r.releases) (Header.wire header)

  let equal a b =
    name_equal a.name b.name && S.equal a.releases b.releases

  let add t i = { t with releases = S.add i t.releases }

  let remove t i = { t with releases = S.remove i t.releases }

  let prep t =
    let carry, counter = Uint.succ t.counter in
    { t with counter }, carry

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf r =
    Format.fprintf ppf "package %a %s (created %s)@ %a"
      pp_name r.name
      (Header.counter r.counter r.wraps)
      (Header.timestamp r.created)
      (pp_list pp_name) (List.sort String.compare_insensitive (S.elements r.releases))
  (*BISECT-IGNORE-END*)
end

module Release = struct
  type c = {
    filename : name ;
    digest   : Digest.t ;
  }

  (*BISECT-IGNORE-BEGIN*)
  let pp_c ppf c =
    Format.fprintf ppf "%a: %a"
      pp_name c.filename Digest.pp c.digest
  (*BISECT-IGNORE-END*)

  let checksum_equal a b =
    name_equal a.filename b.filename && Digest.equal a.digest b.digest

  let checksum_of_wire data =
    let open Wire in
    list data >>= function
    | [ digest ; String filename ] ->
      Digest.of_wire digest >>= fun digest ->
      Ok ({ filename ; digest })
    | _ -> Error "cannot parse checksum"

  let wire_checksum c =
    let open Wire in
    List [ Digest.wire_raw c.digest ; String c.filename ]

  type checksum_map = c M.t

  let fold f m acc = M.fold (fun _ c acc -> f c acc) m acc
  let find m id = M.find id m

  (*BISECT-IGNORE-BEGIN*)
  let pp_checksum_map ppf cs =
    pp_list pp_c ppf
      (List.sort (fun a b -> String.compare a.filename b.filename)
         (snd (List.split (M.bindings cs))))
  (*BISECT-IGNORE-END*)

  let version = Uint.zero
  type t = {
    created : Uint.t ;
    counter : Uint.t ;
    wraps : Uint.t ;
    name : name ;
    files : checksum_map ;
  }

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf c =
    Format.fprintf ppf "release %a %s (created %s)@ %a"
      pp_name c.name
      (Header.counter c.counter c.wraps)
      (Header.timestamp c.created)
      pp_checksum_map c.files
  (*BISECT-IGNORE-END*)

  let t ?(counter = Uint.zero) ?(wraps = Uint.zero) created name files =
    let files = List.fold_left (fun m f -> M.add f.filename f m) M.empty files in
    { created ; counter ; wraps ; name ; files }

  let of_wire data =
    let open Wire in
    Header.keys ["files"] data >>= fun () ->
    Header.of_wire data >>= fun h ->
    Header.check `Release version h >>= fun () ->
    opt_list (search data "files") >>= fun sums ->
    foldM (fun acc v -> checksum_of_wire v >>= fun cs -> Ok (cs :: acc)) [] sums >>= fun files ->
    Ok (t ~counter:h.Header.counter ~wraps:h.Header.wraps h.Header.created h.Header.name files)

  let wire cs =
    let open Wire in
    let counter = cs.counter
    and wraps = cs.wraps
    and created = cs.created
    and name = cs.name
    and typ = `Release
    in
    let header = { Header.version ; created ; counter ; wraps ; name ; typ } in
    let csums = fold (fun c acc -> wire_checksum c :: acc) cs.files [] in
    M.add "files" (List csums) (Header.wire header)

  let set_counter cs counter = { cs with counter }

  let compare_t a b =
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
    match compare_t a b with
    | Ok () -> true
    | _ -> false

  let prep t =
    let carry, counter = Uint.succ t.counter in
    { t with counter }, carry
end
