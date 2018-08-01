open Conex_utils


type name = string

(*BISECT-IGNORE-BEGIN*)
let pp_name = Format.pp_print_string
(*BISECT-IGNORE-END*)

let name_equal a b = String.compare_insensitive a b = 0


type identifier = string

(*BISECT-IGNORE-BEGIN*)
let pp_id = Format.pp_print_string
(*BISECT-IGNORE-END*)

let id_equal a b = String.compare_insensitive a b = 0


type timestamp = string

(*BISECT-IGNORE-BEGIN*)
let pp_timestamp = Format.pp_print_string
(*BISECT-IGNORE-END*)

let timestamp_equal a b = String.compare a b = 0


module Wire = struct
  type s =
    | Map of s M.t
    | List of s list
    | Identifier of identifier
    | Data of string
    | Bigint of Uint.t
    | Smallint of int
    | Pair of s * s
    | And of s * s
    | Or of s * s

  let rec s_to_string = function
    | Bigint x -> "0x" ^ Uint.to_string x
    | Smallint i -> string_of_int i
    | Data s -> string_of_int (String.length s) ^ "'" ^ s
    | Identifier i -> i
    | List xs -> "[" ^ String.concat ";" (List.map s_to_string xs) ^ "]"
    | Map m ->
      let strs =
        let bindings = M.bindings m in
        let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) bindings in
        List.map (fun (k, v) -> k ^ ":" ^ s_to_string v) sorted
      in
      "{" ^ String.concat ";" strs ^ "}"
    | Pair (i, s) -> "(" ^ s_to_string i ^ s_to_string s ^ ")"
    | And (a, b) -> "(" ^ s_to_string a ^ "&&" ^ s_to_string b ^ ")"
    | Or (a, b) -> "(" ^ s_to_string a ^ "||" ^ s_to_string b ^ ")"

  type t = s M.t

  let to_string t = s_to_string (Map t)

  let opt_err = function
    | Some x -> Ok x
    | None -> Error "expected some, got none"

  let pdata = function
    | Data x -> Ok x
    | _ -> Error "couldn't find data"

  let puint = function
    | Bigint x -> Ok x
    | _ -> Error "couldn't find int"

  let pint = function
    | Smallint x -> Ok x
    | _ -> Error "couldn't find int"

  let plist = function
    | List x -> Ok x
    | _ -> Error "couldn't find list"

  let pmap = function
    | Map m -> Ok m
    | _ -> Error "couldn't find map"
end

type typ = [
  | `Root
  | `Targets
]

let typ_equal a b = match a, b with
  | `Root, `Root
  | `Targets, `Targets -> true
  | _ -> false

let typ_to_string = function
  | `Root -> "root"
  | `Targets -> "targets"

let string_to_typ = function
  | "root" -> Some `Root
  | "targets" -> Some `Targets
  | _ -> None

(*BISECT-IGNORE-BEGIN*)
let pp_typ ppf typ = Format.pp_print_string ppf (typ_to_string typ)
(*BISECT-IGNORE-END*)

let wire_typ typ = Wire.Identifier (typ_to_string typ)
let typ_of_wire = function
  | Wire.Identifier str ->
    (match string_to_typ str with
     | None -> Error "unknown resource type"
     | Some x -> Ok x)
  | _ -> Error "cannot parse resource type"

type err = [
  | `Parse of string
  | `Unknown_alg of string
  | `Malformed
]

(*BISECT-IGNORE-BEGIN*)
let pp_err ppf = function
  | `Parse err -> Format.fprintf ppf "parse error %s" err
  | `Unknown_alg alg -> Format.fprintf ppf "unknown algorithm %s" alg
  | `Malformed -> Format.pp_print_string ppf "malformed"
(*BISECT-IGNORE-END*)

module Header = struct
  type t = {
    version : int ;
    created : timestamp ;
    counter : Uint.t ;
    epoch : Uint.t ;
    name : name ;
    typ : typ
  }

  let of_wire data =
    let open Wire in
    opt_err (M.find "version" data) >>= pint >>= fun version ->
    opt_err (M.find "created" data) >>= pdata >>= fun created ->
    opt_err (M.find "counter" data) >>= puint >>= fun counter ->
    opt_err (M.find "epoch" data) >>= puint >>= fun epoch ->
    opt_err (M.find "name" data) >>= pdata >>= fun name ->
    opt_err (M.find "typ" data) >>= typ_of_wire >>= fun typ ->
    Ok { version ; created ; counter ; epoch ; name ; typ }

  (*BISECT-IGNORE-BEGIN*)
  let counter x epoch =
    "#" ^ (Uint.decimal x) ^
    (if Uint.compare epoch Uint.zero = 0 then ""
     else "[" ^ (Uint.decimal epoch) ^ "]")

  let pp ppf hdr =
    Format.fprintf ppf "%a %a %s created %a"
      pp_typ hdr.typ
      pp_name hdr.name
      (counter hdr.counter hdr.epoch)
      pp_timestamp hdr.created
  (*BISECT-IGNORE-END*)

  let wire t =
    let open Wire in
    M.add "version" (Smallint t.version)
      (M.add "created" (Data t.created)
         (M.add "counter" (Bigint t.counter)
            (M.add "epoch" (Bigint t.epoch)
               (M.add "name" (Data t.name)
                  (M.add "typ" (wire_typ t.typ) M.empty)))))

  let split_signed map =
    match M.find "signed" map with
    | None -> Error "couldn't find signed part"
    | Some signed ->
      Wire.pmap signed >>= fun signed ->
      (match M.find "signatures" map with
      | None -> Ok []
      | Some sigs -> Wire.plist sigs) >>= fun sigs ->
      Ok (signed, sigs)

  let keys ?(header = true) additional map =
    let wanted =
      if header then
        "created" :: "counter" :: "version" :: "epoch" :: "name" :: "typ" :: additional
      else
        additional
    in
    let have = S.of_list (fst (List.split (M.bindings map))) in
    if S.subset have (S.of_list wanted) then
      Ok ()
    else
      Error (Printf.sprintf "key sets not compatible: have %s want %s"
               (String.concat ";" (S.elements have)) (String.concat ";" wanted))

  let check t v hdr =
    guard (hdr.version = v)
      (Printf.sprintf "expected data version #%d, found #%d" v hdr.version) >>= fun () ->
    guard (typ_equal t hdr.typ)
      (Printf.sprintf "expected resource type %s, found %s"
         (typ_to_string t) (typ_to_string hdr.typ))
end

module Digest = struct
  type alg = [ `SHA256 ]
  let alg_to_string = function `SHA256 -> "sha256"
  let string_to_alg = function
    | "sha256" -> Some `SHA256
    | _ -> None

  type t = alg * string

  let compare (ta, a) (tb, b) = match ta, tb with
    | `SHA256, `SHA256 -> String.compare a b

  let equal a b = compare a b = 0

  let to_string (typ, data) = alg_to_string typ ^ "=" ^ data

  let of_string str = match String.cut '=' str with
    | None -> Error `Malformed
    | Some (alg, data) ->
      match string_to_alg alg with
      | Some hash -> Ok (hash, data)
      | None -> Error (`Unknown_alg alg)

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf t = Format.pp_print_string ppf (to_string t)
  (*BISECT-IGNORE-END*)

  let of_wire = function
    | Wire.Data dgst -> of_string dgst
    | _ -> Error `Malformed

  let wire_raw t = Wire.Data (to_string t)
end

module Digest_map = struct
  include Map.Make(Digest)

  let find k m = try Some (find k m) with Not_found -> None

  let pp pp_e ppf t =
    iter (fun k v -> Format.fprintf ppf "%a -> %a@ " Digest.pp k pp_e v) t
end

module Key = struct
  type alg = [ `RSA ]

  let alg_to_string = function `RSA -> "rsa"
  let string_to_alg = function "rsa" -> Some `RSA | _ -> None
  let alg_equal a b = match a, b with
    | `RSA, `RSA -> true

  type t = identifier * timestamp * alg * string

  let equal (id, ts, alg, data) (id', ts', alg', data') =
    id_equal id id' && timestamp_equal ts ts' &&
    alg_equal alg alg' && String.compare data data' = 0

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf (id, created, a, x) =
    Format.fprintf ppf "%a (created %a) %s key %d bytes"
      pp_id id pp_timestamp created
      (alg_to_string a) (String.length x)
  (*BISECT-IGNORE-END*)

  (* stored persistently on disk inside Root and Targets *)
  let of_wire data =
    let open Wire in
    match plist data with
    | Error str -> Error (`Parse str)
    | Ok [ Identifier id ; Data ts ; Data raw ] ->
      begin match String.cut '=' raw with
        | None -> Error `Malformed
        | Some (alg, data) ->
          match string_to_alg alg with
          | Some alg -> Ok (id, ts, alg, data)
          | None -> Error (`Unknown_alg alg)
      end
    | _ -> Error `Malformed

  let many_of_wire keys =
    foldM (fun (acc, msgs) k ->
        match of_wire k with
        | Ok ((id, _, _, _) as key) ->
          if M.mem id acc then begin
            let msg = "key with id " ^ id ^ " already present, ignoring" in
            Ok (acc, msg :: msgs)
          end else
            Ok (M.add id key acc, msgs)
        | Error (`Unknown_alg alg) ->
          let msg = "couldn't parse key algorithm " ^ alg ^ ", ignoring" in
          Ok (acc, msg :: msgs)
        | Error e -> Error (str_pp pp_err e))
      (M.empty, []) keys

  let to_string (_, _, a, k) = alg_to_string a ^ "=" ^ k

  let wire_raw k =
    let (id, created, _, _) = k in
    Wire.List [ Wire.Identifier id ; Wire.Data created ; Wire.Data (to_string k) ]

  let wire k = M.add "keys" (wire_raw k) M.empty

  let keyid hash t = hash (to_string t)
end

module Signature = struct
  type alg = [ `RSA_PSS_SHA256 ]

  let alg_to_string = function
    | `RSA_PSS_SHA256 -> "rsapss-sha256"

  let alg_equal a b = match a, b with
    | `RSA_PSS_SHA256, `RSA_PSS_SHA256 -> true

  let string_to_alg = function
    | "rsapss-sha256" -> Some `RSA_PSS_SHA256
    | _ -> None

  type t = identifier * timestamp * alg * string

  let equal (id, ts, alg, data) (id', ts', alg', data') =
    id_equal id id' && timestamp_equal ts ts' &&
    alg_equal alg alg' && String.compare data data' = 0

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf (id, created, alg, data) =
    Format.fprintf ppf "%s signature by %a (created %a), %d bytes"
      (alg_to_string alg) pp_id id pp_timestamp created (String.length data)
  (*BISECT-IGNORE-END*)

  (* again stored on disk as part of other files *)
  let of_wire data =
    let open Wire in
    match plist data with
    | Error str -> Error (`Parse str)
    | Ok [ Identifier id ; Data created ; Data s ] ->
      begin match String.cut '=' s with
        | None -> Error `Malformed
        | Some (alg, data) ->
          match string_to_alg alg with
          | None -> Error (`Unknown_alg alg)
          | Some alg -> Ok (id, created, alg, data)
      end
    | _ -> Error `Malformed

  let many_of_wire sigs =
    foldM (fun (acc, msgs) s ->
        match of_wire s with
        | Ok ((id, _, _, _) as s) ->
          if M.mem id acc then begin
            let msg =  "signature from " ^ id ^ " already present, ignoring" in
            Ok (acc, msg :: msgs)
          end else
            Ok (M.add id s acc, msgs)
        | Error (`Unknown_alg a) ->
          let msg = "no support for signature algorithm " ^ a ^ ", ignoring" in
          Ok (acc, msg :: msgs)
        | Error e -> Error (str_pp pp_err e))
      (M.empty, []) sigs

  let wire_raw (id, created, alg, s) =
    let value = alg_to_string alg ^ "=" ^ s in
    let open Wire in
    List [ Identifier id ; Data created ; Data value ]
end

let to_be_signed data created id alg =
  let open Wire in
  M.add "created" (Data created)
    (M.add "identifier" (Identifier id)
       (M.add "sigtype" (Identifier (Signature.alg_to_string alg))
          (M.add "data" (Map data) M.empty)))

module Expression = struct

  type keyref =
    | Remote of identifier * Digest.t * Uint.t
    | Local of identifier

  let keyref_compare a b =
    match a, b with
    | Local id, Local id' -> String.compare_insensitive id id'
    | Local _, Remote _ -> -1
    | Remote _, Local _ -> 1
    | Remote (id, dgst, e), Remote (id', dgst', e') ->
      match Uint.compare e e', String.compare_insensitive id id', Digest.compare dgst dgst' with
      | 0, 0, x -> x
      | 0, x, _ -> x
      | x, _, _ -> x

  module KS = Set.Make(struct
      type t = keyref
      let compare = keyref_compare
    end)

  (*BISECT-IGNORE-BEGIN*)
  let pp_keyref ppf = function
    | Local id -> Format.fprintf ppf "local %a" pp_id id
    | Remote (id, digest, epoch) ->
      Format.fprintf ppf "remote %a %a %s" pp_id id Digest.pp digest (Uint.to_string epoch)
  (*BISECT-IGNORE-END*)

  let keyref_of_wire data =
    let open Wire in
    match data with
    | Identifier id ->
      Ok (Local id)
    | List [ Identifier id ; digest ; Bigint epoch ] ->
      Digest.of_wire digest >>= fun dgst ->
      Ok (Remote (id, dgst, epoch))
    | _ -> Error `Malformed

  let keyref_to_wire =
    let open Wire in
    function
    | Remote (id, digest, epoch) ->
      List [ Identifier id ; Digest.wire_raw digest ; Bigint epoch ]
    | Local id -> Identifier id

  type t =
    | Quorum of int * KS.t
    | And of t * t
    | Or of t * t

  let int_compare : int -> int -> int = compare

  (* compare + equality should use a 'normalised' form:
     'a | b' and 'b | a' are equal
     'a & b' and 'b & a' are equal
     '(a | b) & c' is equal to: 'c & (a | b)' and 'c & (b | a)' and '(b | a) & c' and '(c & a) | (c & b)' and ...

     in addition, if there are "a | b" and "a" delegations for sth, no need to check the latter if the former failed
      same for (2, [ a, b, c]) (should be "better" than) and (3, [ a, b, c ]) or (2, [ a, b ])

     --> this may all be slight overengineering... let's not to it for starters
 *)
  let rec compare a b =
    match a, b with
    | Or (a, b), Or (a', b') ->
      begin match compare a a', compare b b' with
        | 0, x -> x
        | x, _ -> x
      end
    | Or _, _ -> 1 | _, Or _ -> -1
    | And (a, b), And (a', b') ->
      begin match compare a a', compare b b' with
        | 0, x -> x
        | x, _ -> x
      end
    | And _, _ -> 1 | _, And _ -> -1
    | Quorum (n, s), Quorum (n', s') ->
      match int_compare n n' with
      | 0 ->
        begin match int_compare (KS.cardinal s) (KS.cardinal s') with
          | 0 -> KS.compare s s'
          | x -> x
        end
      | x -> x

  let rec equal a b =
    match a, b with
    | Quorum (n, s), Quorum (n', s') -> n = n' && KS.equal s s'
    | And (a, b), And (a', b') -> equal a a' && equal b b'
    | Or (a, b), Or (a', b') -> equal a a' && equal b b'
    | _ -> false

  (*BISECT-IGNORE-BEGIN*)
  let rec pp ppf = function
    | Quorum (quorum, keys) ->
      Format.fprintf ppf "(%d %a)" quorum (pp_list pp_keyref) (KS.elements keys)
    | And (a, b) -> Format.fprintf ppf "(%a & %a)" pp a pp b
    | Or (a, b) -> Format.fprintf ppf "(%a | %a)" pp a pp b
  (*BISECT-IGNORE-END*)

  let rec keys m = function
    | Quorum (_, keyrefs) ->
      KS.fold
        (fun keyref m -> match keyref with
           | Local _ -> m
           | Remote (id, d, e) -> match M.find id m with
             | None -> M.add id (d, e) m
             | Some (d', e') when Digest.equal d d' && Uint.compare e e' = 0 -> m
             | Some (d', e') ->
               Format.printf "WARN: key %s already needed in (epoch %a, digest %a), now (epoch %a, digest %a) [selected higher epoch]\n"
                 id Uint.pp e' Digest.pp d' Uint.pp e Digest.pp d ;
               if Uint.compare e' e = 1 then begin
                 Format.printf "WARN: replacing existing digest with newer epoch\n" ;
                 M.add id (d', e') m
               end else
                 m)
        keyrefs m
    | And (a, b) -> keys (keys m a) b
    | Or (a, b) -> keys (keys m a) b

  let rec of_wire =
    let single w = match keyref_of_wire w with
      | Ok kr -> Ok (KS.singleton kr)
      | Error (`Unknown_alg a) ->
        Printf.printf "WARN: unknown algorithm %s while parsing key (ignoring entry)\n" a ;
        Ok KS.empty
      | Error e -> Error (str_pp pp_err e)
    in
    let multi ws =
      foldM (fun acc k -> single k >>= fun kr -> Ok (KS.union kr acc))
        KS.empty ws
    in
    function
    | Wire.Pair (i, s) ->
      Wire.pint i >>= fun i ->
      begin match s with
        | Wire.List [] -> Ok KS.empty
        | Wire.List (Wire.List _ :: _ as keyrefs) -> multi keyrefs
        | Wire.Identifier _ -> single s
        | Wire.List [ Wire.Identifier _ as e ] -> single e
        | Wire.List (Wire.Identifier _ :: Wire.Data _ :: _) -> single s
        | Wire.List (Wire.Identifier _ :: _ as ids) -> multi ids
        | _ -> Error "malformed"
      end >>= fun keyrefs ->
      guard (i <= KS.cardinal keyrefs) ("insufficient keys for quorum") >>= fun () ->
      Ok (Quorum (i, keyrefs))
    | Wire.And (a, b) ->
      of_wire a >>= fun a -> of_wire b >>= fun b -> Ok (And (a, b))
    | Wire.Or (a, b) ->
      of_wire a >>= fun a -> of_wire b >>= fun b -> Ok (Or (a, b))
    (* short of Quorum (1, [ x ]) is just x *)
    | Wire.Identifier _
    | Wire.List (Wire.Identifier _ :: _) as e ->
      single e >>= fun ks -> Ok (Quorum (1, ks))
    | _ -> Error "malformed"

  let rec to_wire = function
    | Quorum (1, s) when KS.cardinal s = 1 -> keyref_to_wire (KS.choose s)
    | Quorum (i, s) -> Wire.Pair (Wire.Smallint i, Wire.List (List.map keyref_to_wire (KS.elements s)))
    | And (a, b) -> Wire.And (to_wire a, to_wire b)
    | Or (a, b) -> Wire.Or (to_wire a, to_wire b)

  (* hash : f -> string M.t -> t -> (Digest.t, string) result *)
  let hash f id_m expr =
    let map = function
      | Remote (id, hash, epoch) ->
        Ok (id ^ ":" ^ Digest.to_string hash ^ ":" ^ Uint.to_string epoch)
      | Local id -> match M.find id id_m with
        | None -> Error (id ^ " was not found in provided map")
        | Some x -> Ok x
    in
    let rec to_hash = function
      | And (a, b) ->
        to_hash a >>= fun ha ->
        to_hash b >>= fun hb ->
        Ok ("(" ^ ha ^ "&" ^ hb ^ ")")
      | Or (a, b) ->
        to_hash a >>= fun ha ->
        to_hash b >>= fun hb ->
        Ok ("(" ^ ha ^ "|" ^ hb ^ ")")
      | Quorum (1, s) when KS.cardinal s = 1 ->
        map (KS.choose s)
      | Quorum (n, s) ->
        foldM (fun acc kr ->
            map kr >>= fun s ->
            Ok (s :: acc))
          [] (KS.elements s) >>= fun ks ->
        Ok ("(" ^ string_of_int n ^ "[" ^ String.concat "," ks ^ "])")
    in
    to_hash expr >>= fun s ->
    Ok (f s)

  let rec eval t keys sigs =
    match t with
    | Quorum (quorum, ids) ->
      let good = KS.fold
          (fun keyref acc ->
             match keyref with
             | Local id -> if S.mem id sigs then succ acc else acc
             | Remote (id, digest, e) ->
               match Digest_map.find digest keys with
               | Some (id', e') when id_equal id id' && Uint.compare e e' = 0 -> succ acc
               | _ -> acc)
          ids 0
      in
      good >= quorum
    | And (a, b) -> eval a keys sigs && eval b keys sigs
    | Or (a, b) -> eval a keys sigs || eval b keys sigs
end

module Root = struct
  let version = 1

  let supported_roles = [ "root" ; "timestamp" ; "snapshot" ; "janitor" ]

  type t = {
    created : timestamp ;
    counter : Uint.t ;
    epoch : Uint.t ;
    name : identifier ;
    datadir : path ;
    keydir : path ;
    keys : Key.t M.t ;
    roles : Expression.t M.t ;
    signatures : Signature.t M.t ;
  }

  let t ?(counter = Uint.zero) ?(epoch = Uint.zero) ?(name = "root")
      ?(datadir = [ "packages" ]) ?(keydir = [ "keys" ]) ?(keys = M.empty)
      ?(roles = M.empty) ?(signatures = M.empty) created =
    { created ; counter ; epoch ; name ; datadir ; keydir ; keys ; roles ; signatures }

  let add_signature t id s =
    let signatures = M.add id s t.signatures in
    { t with signatures }

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf t =
    Format.fprintf ppf
      "root %s (created %a), datadir %a keydir %a@.@[<2>keys %a@]@.@[<2>roles %a@]@.@[<2> signatures %a@]"
      (Header.counter t.counter t.epoch)
      pp_timestamp t.created
      pp_path t.datadir pp_path t.keydir
      (M.pp Key.pp) t.keys
      (M.pp Expression.pp) t.roles
      (M.pp Signature.pp) t.signatures
  (*BISECT-IGNORE-END*)

  let safe_role s = guard (String.is_ascii s) ("invalid role")

  let of_wire data =
    Header.split_signed data >>= fun (signed, sigs) ->
    let keys = [ "datadir" ; "keydir" ; "keys" ; "roles" ] in
    Header.keys keys signed >>= fun () ->
    Header.of_wire signed >>= fun h ->
    Header.check `Root version h >>= fun () ->
    let open Wire in
    opt_err (M.find "datadir" signed) >>= pdata >>= string_to_path >>= fun datadir ->
    opt_err (M.find "keydir" signed) >>= pdata >>= string_to_path >>= fun keydir ->
    opt_err (M.find "keys" signed) >>= plist >>= Key.many_of_wire >>= fun (keys, w) ->
    opt_err (M.find "roles" signed) >>= pmap >>= fun roles ->
    Header.keys ~header:false supported_roles roles >>= fun () ->
    M.fold (fun k v acc ->
        acc >>= fun (map, msgs) ->
        safe_role k >>= fun () ->
        Expression.of_wire v >>= fun expr ->
        if M.mem k map then begin
          let msg = "roles with name " ^ k ^ " already present, ignoring" in
          Ok (map, msg :: msgs)
        end else
          Ok (M.add k expr map, msgs))
      roles (Ok (M.empty, [])) >>= fun (roles, w') ->
    Signature.many_of_wire sigs >>= fun (signatures, w'') ->
    let warns = w @ w' @ w'' in
    Ok ({ created = h.Header.created ; counter = h.Header.counter ;
          epoch = h.Header.epoch ; name = h.Header.name ;
          datadir ; keydir ; keys ; roles ; signatures }, warns)

  let wire_raw t =
    let open Wire in
    let created = t.created
    and counter = t.counter
    and epoch = t.epoch
    and name = t.name
    and typ = `Root
    in
    let header = { Header.version ; created ; counter ; epoch ; name ; typ } in
    let roles =
      M.fold (fun k v acc -> M.add k (Expression.to_wire v) acc) t.roles M.empty
    in
    M.add "datadir" (Data (path_to_string t.datadir))
      (M.add "keydir" (Data (path_to_string t.keydir))
         (M.add "keys" (List (M.fold (fun _ key acc -> Key.wire_raw key :: acc) t.keys []))
            (M.add "roles" (Map roles)
               (Header.wire header))))

  let wire t =
    let open Wire in
    M.add "signed" (Map (wire_raw t))
      (M.add "signatures" (List (M.fold (fun _ s acc -> Signature.wire_raw s :: acc) t.signatures []))
         M.empty)
end

module Delegation = struct
  (* TODO paths should be a path set!
     - of_wire should pick a default for terminating, and make its orccurence optional
 *)
  type t = {
    paths : path list ;
    keys : Expression.t ;
    terminating : bool
  }

  let equal a b =
    List.length a.paths = List.length b.paths &&
    List.for_all (fun p -> List.exists (path_equal p) b.paths) a.paths &&
    Expression.equal a.keys b.keys &&
    a.terminating = b.terminating

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf t =
    Format.fprintf ppf "delegation (terminating %b) paths %a@.keys %a@."
      t.terminating
      (pp_list pp_path) t.paths
      Expression.pp t.keys
  (*BISECT-IGNORE-END*)

  let of_wire wire =
    let open Wire in
    pmap wire >>= fun delegation ->
    Header.keys ~header:false [ "paths" ; "keys" ; "terminating" ] delegation >>= fun () ->
    opt_err (M.find "paths" delegation) >>= plist >>= fun paths ->
    opt_err (M.find "keys" delegation) >>= Expression.of_wire >>= fun keys ->
    (match M.find "terminating" delegation with
     | None -> Ok false
     | Some x -> pdata x >>= function
       | "true" | "yes" -> Ok true
       | "false" | "no" -> Ok false
       | x -> Error ("unkown value for terminating " ^ x)) >>= fun terminating ->
    foldM (fun acc p -> pdata p >>= string_to_path >>= fun s -> Ok (s :: acc)) [] paths >>= fun paths ->
    let paths = List.rev paths in
    Ok { paths ; keys ; terminating }

  let wire_raw t =
    let open Wire in
    let map =
      M.add "paths" (List (List.map (fun p -> Data (path_to_string p)) t.paths))
        (M.add "keys" (Expression.to_wire t.keys)
           (M.add "terminating" (Data (if t.terminating then "true" else "false"))
              M.empty))
    in
    Map map
end

module Target = struct
  type t = {
    filename : path ;
    digest : Digest.t list ;
    size : Uint.t ;
  }

  let equal t t' =
    path_equal t.filename t'.filename && Uint.compare t.size t'.size = 0 &&
    List.length t.digest = List.length t'.digest &&
    List.for_all (fun d -> List.exists (Digest.equal d) t'.digest) t.digest

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf t =
    Format.fprintf ppf "%a (%s bytes) %a"
      pp_path t.filename
      (Uint.decimal t.size)
      (pp_list Digest.pp) t.digest
  (*BISECT-IGNORE-END*)

  let of_wire wire =
    let open Wire in
    pmap wire >>= fun target ->
    Header.keys ~header:false [ "filename" ; "digest" ; "size" ] target >>= fun () ->
    opt_err (M.find "filename" target) >>= pdata >>= string_to_path >>= fun filename ->
    opt_err (M.find "size" target) >>= puint >>= fun size ->
    opt_err (M.find "digest" target) >>= plist >>= fun digest ->
    foldM (fun acc d -> match Digest.of_wire d with
        | Ok d -> Ok (d :: acc)
        | Error (`Unknown_alg a) -> (* TODO *)
          Printf.printf "WARN: ignoring unknown digest algorithm %s\n" a ;
          Ok acc
        | Error e -> Error (str_pp pp_err e))
      [] digest >>= fun digest ->
    let digest = List.rev digest in
    Ok { filename ; size ; digest }

  let wire_raw t =
    let open Wire in
    let map =
      M.add "filename" (Data (path_to_string t.filename))
        (M.add "size" (Bigint t.size)
           (M.add "digest" (List (List.map Digest.wire_raw t.digest))
              M.empty))
    in
    Map map
end

module Targets = struct
  let version = 0

  type t = {
    created : timestamp ;
    counter : Uint.t ;
    epoch : Uint.t ;
    name : identifier ;
    keys : Key.t M.t ;
    valid : Expression.t ;
    delegations : Delegation.t list ;
    targets : Target.t list ;
    signatures : Signature.t M.t ;
  }

  let t ?(counter = Uint.zero) ?(epoch = Uint.zero) ?(keys = M.empty)
      ?(delegations = []) ?(targets = []) ?(signatures = M.empty) created name valid
    = { created ; counter ; epoch ; name ; keys ; valid ; delegations ;
        targets ; signatures }

  let add_signature t id s =
    let signatures = M.add id s t.signatures in
    { t with signatures }

  let equal t t' =
    timestamp_equal t.created t'.created &&
    Uint.compare t.counter t'.counter = 0 &&
    Uint.compare t.epoch t'.epoch = 0 &&
    name_equal t.name t'.name &&
    M.equal Key.equal t.keys t'.keys &&
    Expression.equal t.valid t'.valid &&
    List.length t.delegations = List.length t'.delegations &&
    List.for_all (fun d -> List.exists (Delegation.equal d) t'.delegations) t.delegations &&
    List.length t.targets = List.length t'.targets &&
    List.for_all (fun t -> List.exists (Target.equal t) t'.targets) t.targets &&
    M.equal Signature.equal t.signatures t'.signatures

  (*BISECT-IGNORE-BEGIN*)
  let pp ppf t =
    Format.fprintf ppf "targets %a %s (created %a)@.@[<2>keys %a@]@.@[<2>valid %a@]@.@[<2>delegations %a@]@.@[<2>targets %a@]@.@[<2>signatures %a@]"
      pp_name t.name
      (Header.counter t.counter t.epoch)
      pp_timestamp t.created
      (M.pp Key.pp) t.keys
      Expression.pp t.valid
      (pp_list Delegation.pp) t.delegations
      (pp_list Target.pp) t.targets
      (M.pp Signature.pp) t.signatures
  (*BISECT-IGNORE-END*)

  let of_wire data =
    Header.split_signed data >>= fun (signed, sigs) ->
    let keys = [ "keys" ; "valid" ; "delegations" ; "targets" ] in
    Header.keys keys signed >>= fun () ->
    Header.of_wire signed >>= fun h ->
    Header.check `Targets version h >>= fun () ->
    let open Wire in
    (match M.find "keys" signed with
     | None -> Ok (M.empty, [])
     | Some keys -> plist keys >>= Key.many_of_wire) >>= fun (keys, w) ->
    opt_err (M.find "valid" signed) >>= Expression.of_wire >>= fun valid ->
    opt_err (M.find "delegations" signed) >>= plist >>= fun delegations ->
    opt_err (M.find "targets" signed) >>= plist >>= fun targets ->
    (* preserve order! *)
    foldM (fun acc d -> Delegation.of_wire d >>= fun d -> Ok (d :: acc)) [] delegations >>= fun delegations ->
    let delegations = List.rev delegations in
    foldM (fun acc t -> Target.of_wire t >>= fun t -> Ok (t :: acc)) [] targets >>= fun targets ->
    let targets = List.rev targets in
    Signature.many_of_wire sigs >>= fun (signatures, w') ->
    let warn = w @ w' in
    Ok ({ created = h.Header.created ; counter = h.Header.counter ;
          epoch = h.Header.epoch ; name = h.Header.name ;
          keys ; valid ; delegations ; targets ; signatures }, warn)

  let wire_raw t =
    let open Wire in
    let created = t.created
    and counter = t.counter
    and epoch = t.epoch
    and name = t.name
    and typ = `Targets
    in
    let header = { Header.version ; created ; counter ; epoch ; name ; typ } in
    M.add "keys" (List (M.fold (fun _ key acc -> Key.wire_raw key :: acc) t.keys []))
      (M.add "valid" (Expression.to_wire t.valid)
         (M.add "delegations" (List (List.map Delegation.wire_raw t.delegations))
            (M.add "targets" (List (List.map Target.wire_raw t.targets))
               (Header.wire header))))

  let wire t =
    let open Wire in
    M.add "signed" (Map (wire_raw t))
      (M.add "signatures" (List (M.fold (fun _ s acc -> Signature.wire_raw s :: acc) t.signatures []))
         M.empty)
end
