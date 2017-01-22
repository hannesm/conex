open Conex_result
open Conex_core
open Conex_resource

type s =
  | Map of s M.t
  | List of s list
  | String of string
  | Int of Uint.t

type t = s M.t

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

let t_to_team data =
  keys true ["members"] data >>= fun () ->
  ncv data >>= fun (name, counter, version) ->
  opt_string_set (search data "members") >>= fun members ->
  Ok (Team.team ~counter ~version ~members name)

let team_to_t d =
  M.add "members" (wire_string_set d.Team.members)
    (wire_ncv d.Team.name d.Team.counter d.Team.version)


let t_to_releases data =
  keys true ["releases"] data >>= fun () ->
  ncv data >>= fun (name, counter, version) ->
  opt_string_set (search data "releases") >>= fun releases ->
  Releases.releases ~counter ~version ~releases name

let releases_to_t r =
  M.add "releases" (wire_string_set r.Releases.releases)
    (wire_ncv r.Releases.name r.Releases.counter r.Releases.version)


let t_to_authorisation data =
  keys true ["authorised"] data >>= fun () ->
  ncv data >>= fun (name, counter, version) ->
  opt_string_set (search data "authorised") >>= fun authorised ->
  Ok (Authorisation.authorisation ~counter ~version ~authorised name)

let authorisation_to_t d =
  M.add "authorised" (wire_string_set d.Authorisation.authorised)
    (wire_ncv d.Authorisation.name d.Authorisation.counter d.Authorisation.version)


let checksum data =
  map data >>= fun map ->
  keys false ["filename" ; "byte-size" ; "sha256"] map >>= fun () ->
  opt_err (search map "filename") >>= string >>= fun filename ->
  opt_err (search map "byte-size") >>= int >>= fun bytesize ->
  opt_err (search map "sha256") >>= string >>= fun checksum ->
  Ok ({ Checksum.filename ; bytesize ; checksum })

let t_to_checksums data =
  keys true ["files"] data >>= fun () ->
  ncv data >>= fun (name, counter, version) ->
  opt_list (search data "files") >>= fun sums ->
  foldM (fun acc v -> checksum v >>= fun cs -> Ok (cs :: acc))
    [] sums >>= fun files ->
  Ok (Checksum.checksums ~counter ~version name files)

let wire_checksum c =
  M.add "filename" (String c.Checksum.filename)
    (M.add "byte-size" (Int c.Checksum.bytesize)
       (M.add "sha256" (String c.Checksum.checksum)
          M.empty))

let checksums_to_t cs =
  let csums =
    Checksum.fold (fun c acc -> Map (wire_checksum c) :: acc) cs.Checksum.files []
  in
  M.add "files" (List csums)
    (wire_ncv cs.Checksum.name cs.Checksum.counter cs.Checksum.version)


let signature data =
  list data >>= function
    | [ Int created ;  String d ] -> Ok (created, d)
    | _ -> Error "couldn't parse signature"

let wire_signature (ts, s) = [ Int ts ; String s ]

let resource data =
  map data >>= fun map ->
  keys false ["index" ; "name" ; "size" ; "resource" ; "digest" ] map >>= fun () ->
  opt_err (search map "index") >>= int >>= fun index ->
  opt_err (search map "name") >>= string >>= fun name ->
  opt_err (search map "size") >>= int >>= fun size ->
  opt_err (search map "resource") >>= string >>= fun r ->
  (match string_to_resource r with
   | Some r -> Ok r
   | None -> Error "unknown resource") >>= fun resource ->
  opt_err (search map "digest") >>= string >>= fun digest ->
  Ok (Index.r index name size resource digest)

let wire_resource idx =
  M.add "index" (Int idx.Index.index)
    (M.add "name" (String idx.Index.rname)
       (M.add "size" (Int idx.Index.size)
          (M.add "resource" (String (resource_to_string idx.Index.resource))
             (M.add "digest" (String idx.Index.digest)
                M.empty))))

let accounts map =
  M.fold (fun k v acc ->
      acc >>= fun xs ->
      string v >>= fun s ->
      let data =
        match k with
        | "email" -> `Email s
        | "github" -> `GitHub s
        | x -> `Other (x, s)
      in
      Ok (data :: xs))
    map (Ok [])

let wire_account m = function
  | `Email e -> M.add "email" (String e) m
  | `GitHub g -> M.add "github" (String g) m
  | `Other (k, v) -> M.add k (String v) m

let key data =
  list data >>= function
  | [ Int typ ; String data ] ->
    if Uint.compare Uint.zero typ = 0 then
      Ok (`RSA_pub data)
    else
      Error "unknown key type"
  | _ -> Error "unknown key"

let t_to_index data =
  keys false ["signatures" ; "signed" ; "queued" ; "keys" ; "accounts" ] data >>= fun () ->
  opt_list (search data "signatures") >>= fun sigs ->
  foldM (fun acc v -> signature v >>= fun s -> Ok (s :: acc)) [] sigs >>= fun signatures ->
  opt_err (search data "signed") >>= map >>= fun signed ->
  keys true ["resources"] signed >>= fun () ->
  ncv signed >>= fun (name, counter, version) ->
  opt_list (search signed "resources") >>= fun rs ->
  foldM (fun acc v -> resource v >>= fun r -> Ok (r :: acc)) [] rs >>= fun resources ->
  opt_list (search data "queued") >>= fun qs ->
  foldM (fun acc v -> resource v >>= fun r -> Ok (r :: acc)) [] qs >>= fun queued ->
  opt_list (search data "keys") >>= fun keys ->
  foldM (fun acc k -> key k >>= fun r -> Ok (r :: acc)) [] keys >>= fun keys ->
  opt_map (search data "accounts") >>= accounts >>= fun accounts ->
  Ok (Index.index ~keys ~accounts ~counter ~version ~resources ~queued ~signatures name)

let index_to_t i =
  let resources = List.map (fun r -> Map (wire_resource r)) i.Index.resources in
  M.add "resources" (List resources)
    (wire_ncv i.Index.name i.Index.counter i.Index.version)

let publickey_to_t id k =
  let typ, data =
    match k with
    | `RSA_pub k -> (Uint.zero, k)
  in
  M.add "type" (Int typ)
    (M.add "key" (String data)
       (M.add "name" (String id) M.empty))

let wire_key k =
  match k with
  | `RSA_pub k -> List [ Int Uint.zero ; String k ]

let index_sigs_to_t i =
  M.add "keys" (List (List.map wire_key i.Index.keys))
    (M.add "accounts" (Map (List.fold_left wire_account M.empty i.Index.accounts))
       (M.add "queued" (List (List.map (fun r -> Map (wire_resource r)) i.Index.queued))
          (M.add "signed" (Map (index_to_t i))
             (M.add "signatures" (List (List.map (fun s -> List (wire_signature s)) i.Index.signatures))
                M.empty))))

