open Core

module S = Set.Make(String)

type t = {
  store : Keystore.t ;
  quorum : int ;
  data : Provider.t ;
}

type res = ([ `Identifier of identifier | `Quorum ], error) result

let pp_res ppf = function
  | Ok (`Identifier id) -> Format.fprintf ppf "id %s" id
  | Ok `Quorum -> Format.fprintf ppf "quorum"
  | Error e -> pp_error ppf e

let repository ?(store = Keystore.empty) ?(quorum = 3) data =
  { store ; quorum ; data }

let quorum r = r.quorum

let provider t = t.data

let change_provider t data = { t with data }

let add_key repo key =
  let store = Keystore.add repo.store key in
  { repo with store }

let id_of_sig (id, _) = id

let has_quorum id repo kind data signatures =
  let verify = Keystore.verify repo.store Janitor kind data in
  let sigs = List.map verify signatures in
  let ids = Utils.filter_map ~f:(function Ok id -> Some id | Error _ -> None) sigs in
  if S.cardinal (S.of_list ids) >= repo.quorum then
    Ok `Quorum
  else
    let errs = Utils.filter_map ~f:(function Error e -> Some e | Ok _ -> None) sigs in
    Error (`InsufficientQuorum (id, ids, errs))

let verify_data id repo authorised_ids ?(role = Author) kind data signatures =
  let valids, _others =
    let tst s = List.mem (id_of_sig s) authorised_ids in
    List.partition tst signatures
  in
  let sigs = List.map (Keystore.verify repo.store role kind data) valids in
  let errs = Utils.filter_map ~f:(function Error e -> Some e | Ok _ -> None) sigs in
  Error (`InvalidSignatures (id, errs))
(* XXX: re-enable *)
(* match anyM (Keystore.verify repo.store role data) valids with
  | Ok x -> Ok (`Identifier x)
  | Error () -> has_quorum id repo data others *)

let verify_key repo key =
  let id = key.Publickey.keyid
  and raw = Data.publickey_raw key
  and sigs = key.Publickey.signatures
  and role = key.Publickey.role
  in
  (if Keystore.mem repo.store id then
     let old = Keystore.find repo.store id in
     if Publickey.equal key old then
       Ok ()
     else
       guard (key.Publickey.counter > old.Publickey.counter)
         (`InvalidCounter (id, old.Publickey.counter, key.Publickey.counter)) >>= fun () ->
       verify_data id repo [id] ~role:old.Publickey.role PublicKey raw sigs >>= fun _ ->
       Ok ()
   else
     Ok ()) >>= fun _ ->
  verify_data id (add_key repo key) [id] ~role PublicKey raw sigs >>= fun ok ->
  if key.Publickey.role = Author then
    Ok ok
  else
    has_quorum id repo PublicKey raw sigs >>= fun _ -> Ok ok

let verify_authorisation repo ?authorised auth =
  let raw = Data.authorisation_raw auth
  and signatures = auth.Authorisation.signatures
  in
  let valid = Utils.option auth.Authorisation.authorised (fun x -> x) authorised in
  verify_data auth.Authorisation.name repo valid Authorisation raw signatures

let verify_checksum repo a cs =
  let raw = Data.checksums_raw cs
  and signatures = cs.Checksum.signatures
  in
  let nam = Layout.authorisation_of_item cs.Checksum.name in
  guard (nam = a.Authorisation.name) (`InvalidAuthorisation (a.Authorisation.name, cs.Checksum.name)) >>= fun () ->
  verify_data cs.Checksum.name repo a.Authorisation.authorised Checksum raw signatures

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
  let files = Layout.checksum_files repo.data name in
  let del = Layout.authorisation_of_item name in
  let datas =
    foldM
      (fun acc f ->
         match repo.data.Provider.read ([del;name]@f) with
         | Error _ -> Error (`NotFound (path_to_string ([del;name]@f)))
         | Ok data -> Ok (data :: acc))
      []
      files
  in
  let names = List.map path_to_string files in
  match datas with
  | Ok datas ->
    let csums = List.map2 Checksum.checksum names datas in
    Ok (Checksum.checksums name csums)
  | Error e -> Error e


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

module SM = Map.Make(String)

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

let load_keys repo ?(verify = false) ids =
  if verify then
    let keys = required_keys repo ids in
    List.fold_left
      (fun repo key ->
         match verify_key repo key with
         | Ok _ -> add_key repo key
         | Error e -> pp_error Format.std_formatter e ; repo)
      repo keys
  else
    List.fold_left
      (fun repo id ->
       match read_key repo id with
       | Error _ -> invalid_arg ("could not find key " ^ id)
       | Ok k -> add_key repo k)
      repo ids
