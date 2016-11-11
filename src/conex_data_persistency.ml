open Conex_result
open Conex_core
open Conex_resource

type s =
  | String of string
  | Int of int64

type t =
  | Entry of string * t
  | List of t list
  | Leaf of s

let rec get t key =
  match t with
  | Entry (k, v) when k = key -> Ok v
  | List xs ->
     let isit = function Error _ -> false | Ok _ -> true in
     (try List.find isit (List.map (fun x -> get x key) xs)
      with Not_found -> Error ("couldn't find " ^ key ^ " in List"))
  | Leaf _ -> Error ("couldn't find " ^ key ^ " in Leaf")
  | Entry _ -> Error ("couldn't find " ^ key ^ " in Entry")

let extract_string t key =
  get t key >>= function
  | Leaf (String x) -> Ok x
  | _ -> Error "couldn't find string"

let extract_int t key =
  get t key >>= function
  | Leaf (Int x) -> Ok x
  | _ -> Error "couldn't find int"

let extract_list t key =
  get t key >>= function
  | List x -> Ok x
  | _ -> Error "couldn't find list"

let extract_string_set t key =
  extract_list t key >>= fun els ->
  foldM (fun acc -> function
      | Leaf (String s) -> Ok (s :: acc)
      | _ -> Error ("not a string while parsing list"))
    [] els >>= fun els ->
  Ok (s_of_list els)

let ctr_ver data =
  extract_int data "counter" >>= fun counter ->
  extract_int data "version" >>= fun version ->
  Ok (counter, version)

let t_to_team data =
  ctr_ver data >>= fun (counter, version) ->
  extract_string data "name" >>= fun name ->
  extract_string_set data "members" >>= fun members ->
  Ok (Team.team ~counter ~version ~members name)

let team_to_t d =
  let id s = Leaf (String s) in
  List [ Entry ("name"   , Leaf (String d.Team.name)) ;
         Entry ("counter", Leaf (Int d.Team.counter)) ;
         Entry ("version", Leaf (Int d.Team.version)) ;
         Entry ("members", List (List.map id (S.elements d.Team.members))) ]


let t_to_publickey data =
  ctr_ver data >>= fun (counter, version) ->
  extract_string data "keyid" >>= fun keyid ->
  extract_string data "key" >>= fun key ->
  let key = if key = "NONE" then None else Some (`Pub key) in
  Ok (Publickey.publickey ~counter ~version keyid key)

let publickey_to_t pubkey =
  let pem = match pubkey.Publickey.key with
    | None -> "NONE"
    | Some (`Pub x) -> x
  in
  List [ Entry ("counter", Leaf (Int pubkey.Publickey.counter)) ;
         Entry ("version", Leaf (Int pubkey.Publickey.version)) ;
         Entry ("keyid"  , Leaf (String pubkey.Publickey.keyid)) ;
         Entry ("key"    , Leaf (String pem)) ]


let t_to_releases data =
  ctr_ver data >>= fun (counter, version) ->
  extract_string data "name" >>= fun name ->
  extract_string_set data "releases" >>= fun releases ->
  Releases.releases ~counter ~version ~releases name

let releases_to_t r =
  let id s = Leaf (String s) in
  List [ Entry ("name"     , Leaf (String r.Releases.name)) ;
         Entry ("counter"  , Leaf (Int r.Releases.counter)) ;
         Entry ("version"  , Leaf (Int r.Releases.version)) ;
         Entry ("releases" , List (List.map id (S.elements r.Releases.releases))) ]


let t_to_authorisation data =
  ctr_ver data >>= fun (counter, version) ->
  extract_string data "name" >>= fun name ->
  extract_string_set data "authorised" >>= fun authorised ->
  Ok (Authorisation.authorisation ~counter ~version ~authorised name)

let authorisation_to_t d =
  let id s = Leaf (String s) in
  List [ Entry ("name"      , Leaf (String d.Authorisation.name)) ;
         Entry ("counter"   , Leaf (Int d.Authorisation.counter)) ;
         Entry ("version"   , Leaf (Int d.Authorisation.version)) ;
         Entry ("authorised", List (List.map id (S.elements d.Authorisation.authorised))) ]


let t_to_checksum data =
  extract_string data "filename" >>= fun filename ->
  extract_int data "byte-size" >>= fun bytesize ->
  extract_string data "sha256" >>= fun checksum ->
  Ok ({ Checksum.filename ; bytesize ; checksum })

let t_to_checksums data =
  ctr_ver data >>= fun (counter, version) ->
  extract_string data "name" >>= fun name ->
  extract_list data "files" >>= fun sums ->
  foldM (fun acc v -> t_to_checksum v >>= fun cs -> Ok (cs :: acc))
    [] sums >>= fun files ->
  Ok (Checksum.checksums ~counter ~version name files)

let checksum_to_t c =
  List [ Entry ("filename", Leaf (String c.Checksum.filename)) ;
         Entry ("byte-size", Leaf (Int c.Checksum.bytesize)) ;
         Entry ("sha256", Leaf (String c.Checksum.checksum)) ]

let checksums_to_t cs =
  let csums =
    Checksum.fold (fun c acc -> checksum_to_t c :: acc) cs.Checksum.files []
  in
  List [ Entry ("counter", Leaf (Int cs.Checksum.counter)) ;
         Entry ("version", Leaf (Int cs.Checksum.version)) ;
         Entry ("name", Leaf (String cs.Checksum.name)) ;
         Entry ("files", List csums) ]


let t_to_signature data =
  extract_string data "keyid" >>= fun keyid ->
  extract_int data "created" >>= fun created ->
  extract_string data "sig" >>= fun sigval ->
  Ok (keyid, created, sigval)

let signature_to_t (id, ts, s) =
  List [
    Entry ("keyid", Leaf (String id)) ;
    Entry ("created", Leaf (Int ts)) ;
    Entry ("sig", Leaf (String s))
  ]


let t_to_resource data =
  extract_int data "index" >>= fun index ->
  extract_string data "name" >>= fun name ->
  extract_int data "size" >>= fun size ->
  extract_string data "resource" >>= fun r ->
  (match string_to_resource r with
   | Some r -> Ok r
   | None -> Error "unknown resource") >>= fun resource ->
  extract_string data "digest" >>= fun digest ->
  Ok (Index.r index name size resource digest)

let resource_to_t { Index.index ; name ; size ; resource ; digest } =
  List [
    Entry ("index", Leaf (Int index)) ;
    Entry ("name", Leaf (String name)) ;
    Entry ("size", Leaf (Int size)) ;
    Entry ("resource", Leaf (String (resource_to_string resource))) ;
    Entry ("digest", Leaf (String digest))
  ]


let t_to_index data =
  extract_list data "signatures" >>= fun sigs ->
  foldM (fun acc v -> t_to_signature v >>= fun s -> Ok (s :: acc))
    [] sigs >>= fun signatures ->
  get data "signed" >>= fun signed ->
  ctr_ver signed >>= fun (counter, version) ->
  extract_string signed "identifier" >>= fun identifier ->
  extract_list signed "resources" >>= fun rs ->
  foldM (fun acc v -> t_to_resource v >>= fun r -> Ok (r :: acc))
    [] rs >>= fun resources ->
  Ok (Index.index ~counter ~version ~resources ~signatures identifier)

let index_to_t i =
  let resources = List.map resource_to_t i.Index.resources in
  List [
    Entry ("counter"   , Leaf (Int i.Index.counter)) ;
    Entry ("version"   , Leaf (Int i.Index.version)) ;
    Entry ("identifier", Leaf (String i.Index.identifier)) ;
    Entry ("resources" , List resources) ]

let index_sigs_to_t i =
  List [
    Entry ("signed", index_to_t i) ;
    Entry ("signatures", List (List.map signature_to_t i.Index.signatures))
  ]

