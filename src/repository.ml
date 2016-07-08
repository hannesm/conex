open Core

module S = Set.Make(String)

module SM = Map.Make(String)
type valid = (name * kind * identifier list) SM.t

type t = {
  store : Keystore.t ;
  quorum : int ;
  data : Provider.t ;
  valid : valid ;
}

type ok = [ `Identifier of identifier | `Quorum of identifier list | `Both of identifier * identifier list ]
let pp_ok ppf = function
  | `Identifier id -> Format.fprintf ppf "id %s" id
  | `Both (id, js) -> Format.fprintf ppf "id %s and quorum %s" id (String.concat ", " js)
  | `Quorum js -> Format.fprintf ppf "quorum %s" (String.concat ", " js)

type res = (ok, error) result

let pp_res ppf = function
  | Ok ok -> pp_ok ppf ok
  | Error e -> pp_error ppf e

let repository ?(store = Keystore.empty) ?(quorum = 3) data =
  { store ; quorum ; data ; valid = SM.empty }

let quorum r = r.quorum

let provider t = t.data

let change_provider t data = { t with data }

let add_trusted_key repo key =
  let store = Keystore.add repo.store key in
  { repo with store }

let add_csums repo janitor rs =
  let add_csum t (name, kind, hash) =
    try
      let (n, k, ids) = SM.find hash t in
      assert (n = name) ; assert (k = kind) ;
      SM.add hash (n, k, janitor :: ids) t
    with Not_found -> SM.add hash (name, kind, [janitor]) t
  in
  let valid = List.fold_left add_csum repo.valid rs in
  { repo with valid }

let id_of_sig (id, _) = id

let has_quorum id repo kind data =
  let csum = digest data in
  let (n, k, js) =
    if SM.mem csum repo.valid then
      SM.find csum repo.valid
    else
      (id, kind, [])
  in
  (* TODO: define equality on kind and names? *)
  match n = id, k = kind, List.length js >= repo.quorum with
  | true, true, true -> Ok (`Quorum js)
  | _ -> Error (`InsufficientQuorum (id, js))

let verify_data repo authorised_ids kind data signatures =
  let ver = Keystore.verify repo.store kind data in
  let ok, errs =
    List.fold_left (fun (b, err) (id, s) ->
        if b = None && List.mem id authorised_ids then
          match ver (id, s) with
          | Ok id -> (Some id, err)
          | Error e -> (None, e :: err)
        else
          (b, `NotAuthorised id :: err))
      (None, [])
      signatures
  in
  (* TODO: revise return type *)
  match ok, errs with
  | None, e::_ -> Error e
  | None, [] -> Error (`NotAuthorised "bla")
  | Some x, _ -> Ok (`Identifier x)

let verify_key repo key =
  let id = key.Publickey.keyid
  and raw = Data.publickey_raw key
  and sigs = key.Publickey.signatures
  in
  verify_data (add_trusted_key repo key) [id] `PublicKey raw sigs >>= fun (`Identifier id) ->
  has_quorum id repo `PublicKey raw >>= fun (`Quorum js) ->
  Ok (`Both (id, js))

let verify_authorisation repo ?authorised auth =
  let raw = Data.authorisation_raw auth
  and signatures = auth.Authorisation.signatures
  in
  let valid = Utils.option auth.Authorisation.authorised (fun x -> x) authorised in
  verify_data repo valid `Authorisation raw signatures <<|>>
    has_quorum auth.Authorisation.name repo `Authorisation raw

let verify_checksum repo a cs =
  let raw = Data.checksums_raw cs
  and signatures = cs.Checksum.signatures
  in
  let nam = Layout.authorisation_of_item cs.Checksum.name in
  guard (nam = a.Authorisation.name) (`InvalidAuthorisation (a.Authorisation.name, cs.Checksum.name)) >>= fun () ->
  verify_data repo a.Authorisation.authorised `Checksum raw signatures <<|>>
    has_quorum cs.Checksum.name repo `Checksum raw

let verify_releases repo a r =
  let raw = Data.releases_raw r
  and signatures = r.Releases.signatures
  in
  guard (r.Releases.name = a.Authorisation.name) (`InvalidReleases (a.Authorisation.name, r.Releases.name)) >>= fun () ->
  verify_data repo a.Authorisation.authorised `Releases raw signatures <<|>>
    has_quorum r.Releases.name repo `Releases raw

let verify_janitorindex repo ji =
  let raw = Data.janitorindex_raw ji
  and signatures = ji.Janitorindex.signatures
  in
  verify_data repo [ji.Janitorindex.identifier] `JanitorIndex raw signatures

type r_err = [ `NotFound of string | `NameMismatch of string * string ]

let pp_r_err ppf = function
  | `NotFound x -> Format.fprintf ppf "%s was not found in repository" x
  | `NameMismatch (should, is) -> Format.fprintf ppf "%s is named %s" should is

type 'a r_res = ('a, r_err) result

let read_key repo keyid =
  match repo.data.Provider.read (Layout.key_path keyid) with
  | Error _ -> Error (`NotFound keyid)
  | Ok data ->
    let pubkey = Data.data_to_publickey (Data.parse data) in
    if pubkey.Publickey.keyid <> keyid then
      Error (`NameMismatch (keyid, pubkey.Publickey.keyid))
    else
      Ok pubkey

let write_key repo key =
  let data = Data.publickey_to_data key in
  repo.data.Provider.write
    (Layout.key_path key.Publickey.keyid)
    (Data.normalise data)

let all_keyids repo = Layout.keys repo.data

let all_janitors repo = Layout.janitors repo.data

let read_janitorindex repo name =
  match repo.data.Provider.read (Layout.janitorindex_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    let r = Data.data_to_janitorindex (Data.parse data) in
    if r.Janitorindex.identifier <> name then
      Error (`NameMismatch (name, r.Janitorindex.identifier))
    else
      Ok r

let write_janitorindex repo j =
  let data = Data.janitorindex_to_data j in
  let name = Layout.janitorindex_path j.Janitorindex.identifier in
  repo.data.Provider.write name (Data.normalise data)

let read_authorisation repo name =
  match repo.data.Provider.read (Layout.authorisation_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    let auth = Data.data_to_authorisation (Data.parse data) in
    if auth.Authorisation.name <> name then
      Error (`NameMismatch (name, auth.Authorisation.name))
    else
      Ok auth

let write_authorisation repo a =
  let data = Data.authorisation_to_data a in
  repo.data.Provider.write
    (Layout.authorisation_path a.Authorisation.name)
    (Data.normalise data)

let all_authorisations repo = Layout.authorisations repo.data

let read_releases repo name =
  match repo.data.Provider.read (Layout.releases_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    let r = Data.data_to_releases (Data.parse data) in
    if r.Releases.name <> name then
      Error (`NameMismatch (name, r.Releases.name))
    else
      Ok r

let write_releases repo r =
  let data = Data.releases_to_data r in
  let name = Layout.releases_path r.Releases.name in
  repo.data.Provider.write name (Data.normalise data)

let read_checksum repo name =
  match repo.data.Provider.read (Layout.checksum_path name) with
  | Error _ -> Error (`NotFound name)
  | Ok data ->
    let csum = Data.data_to_checksums (Data.parse data) in
    if csum.Checksum.name <> name then
      Error (`NameMismatch (name, csum.Checksum.name))
    else
      Ok csum

let write_checksum repo csum =
  let data = Data.checksums_to_data csum in
  let name = Layout.checksum_path csum.Checksum.name in
  repo.data.Provider.write name (Data.normalise data)

let compute_checksum repo name =
  match repo.data.Provider.file_type (Layout.checksum_dir name) with
  | Error _ -> Error (`NotFound name)
  | Ok File -> Error (`NotFound name) (* more precise? *)
  | Ok Directory ->
    let files = Layout.checksum_files repo.data name in
    let d = Layout.checksum_dir name in
    let datas =
      foldM
        (fun acc f ->
           match repo.data.Provider.read (d@f) with
           | Error _ -> Error (`NotFound (path_to_string (d@f)))
           | Ok data -> Ok (data :: acc))
        []
        files
    in
    let names = List.map path_to_string files in
    match datas with
    | Ok datas ->
      let csums = List.map2 Checksum.checksum names (List.rev datas) in
      Ok (Checksum.checksums name csums)
    | Error e -> Error e

(* TODO: can be removed IIRC now that we have JI *)
module Graph = struct
  type node = string
  type edge = node * node

  type t = { nodes : S.t ; edges : edge list }

  let empty = { nodes = S.empty ; edges = [] }

  let contains graph node = S.mem node graph.nodes

  let insert graph node = { graph with nodes = S.add node graph.nodes }

  let connect graph nodea nodeb =
    let graph = insert graph nodea in
    let graph = insert graph nodeb in
    if nodea = nodeb then
      graph
    else
      let edge = (nodea, nodeb) in
      if List.mem edge graph.edges then
        graph
      else
        { graph with edges = edge :: graph.edges }

  let out_nodes graph node =
    let tst (from, _) = from = node in
    List.map (fun (_, x) -> x) (List.filter tst graph.edges)

  (* this is Tarjan's algorithm based on DFS *)
  let topsort graph =
    let rec visit visiting node l =
      if S.mem node visiting then
        invalid_arg "graph has a cycle"
      else
        let out_nodes = out_nodes graph node
        and visiting = S.add node visiting
        in
        let l = List.fold_right (visit visiting) out_nodes l in
        if List.mem node l then l else node :: l
    in
    let rec s l =
      if S.cardinal graph.nodes = List.length l then
        l
      else
        let rest = S.diff graph.nodes (S.of_list l) in
        let l = visit S.empty (S.choose rest) l in
        s l
    in
    s []
end

(* Ignoring read failures may be fine here, in case a publickey is removed, but there are
   still some dangling signatures, which won't verify later *)

let required_keys repo ids =
  let rec load_one (graph, pubkeys) id =
    if Graph.contains graph id then
      (graph, pubkeys)
    else
      match read_key repo id with
      | Error _ -> invalid_arg ("error while loading key " ^ id)
      | Ok pkey ->
         let others =
           let sigs = pkey.Publickey.signatures in
           List.map id_of_sig sigs
         in
         let graph =
           let conn g o = Graph.connect g o id in
           List.fold_left conn graph others
         in
         List.fold_left load_one (graph, SM.add id pkey pubkeys) others
  in
  let graph, pubkeys = List.fold_left load_one (Graph.empty, SM.empty) ids in
  let sorted = Graph.topsort graph in
  List.map (fun n -> SM.find n pubkeys) sorted


let maybe_load repo ids =
  let keys = required_keys repo ids in
  List.fold_left
    (fun repo key ->
       match verify_key repo key with
       | Ok _ -> add_trusted_key repo key
       | Error e -> pp_error Format.std_formatter e ; repo)
    repo keys

(* TODO: invalid_arg are bad! *)
let load_keys ?(verify = false) repo ids =
  if verify then
    maybe_load repo ids
  else
    List.fold_left
      (fun repo id ->
       match read_key repo id with
       | Error _ -> invalid_arg ("could not find key " ^ id)
       | Ok k -> add_trusted_key repo k)
      repo ids

let load_janitor ?(verify = false) repo janitor =
  match read_janitorindex repo janitor with
  | Ok ji ->
    if verify then
      let repo = maybe_load repo [janitor] in
      match verify_janitorindex repo ji with
      | Ok _ -> add_csums repo janitor ji.Janitorindex.resources
      | Error _ -> invalid_arg "verification failed"
    else
      add_csums repo janitor ji.Janitorindex.resources
  | Error _ -> invalid_arg "unlikely to happen"

let load_janitors ?(verify = false) repo =
  let all = all_janitors repo in
  List.fold_left (load_janitor ~verify) repo all
