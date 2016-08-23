type ('a, 'b) result = Ok of 'a | Error of 'b

module S = Set.Make(String)

let pp_list pe ppf xs =
  Format.pp_print_string ppf "[" ;
  let rec p1 = function
    | [] -> Format.pp_print_string ppf "]"
    | [x] -> Format.fprintf ppf "%a]" pe x
    | x::xs -> Format.fprintf ppf "%a,@ " pe x ; p1 xs
  in
  p1 xs

type file_type = File | Directory

type path = string list

let path_to_string path =
  let skip x = List.mem x [ "." ; "" ; "/" ] in
  List.fold_left (fun d f ->
                  match d, f with
                  | "..", _ -> invalid_arg "there's no escape!"
                  | _, ".." -> invalid_arg "no escape for files!"
                  | d, f when skip d -> f
                  | d, f when skip f -> d
                  | d, f -> Filename.concat d f)
                 "" path

let string_to_path str = Strhelper.cuts '/' str


type name = string

let pp_name ppf x = Format.pp_print_string ppf x

let name_equal a b = String.compare a b = 0


type identifier = string

let pp_id ppf x = Format.pp_print_string ppf x

let id_equal a b = String.compare a b = 0


type digest = string

let pp_digest ppf x = Format.pp_print_string ppf x

let digest data =
  let cs = Cstruct.of_string data in
  let check = Nocrypto.Hash.digest `SHA256 cs in
  let b64 = Nocrypto.Base64.encode check in
  Cstruct.to_string b64


type resource = [
  | `PublicKey
  | `Checksum
  | `Releases
  | `Authorisation
]

let resource_equal a b = match a, b with
  | `PublicKey, `PublicKey
  | `Checksum, `Checksum
  | `Releases, `Releases
  | `Authorisation, `Authorisation -> true
  | _ -> false

let resource_to_string = function
  | `PublicKey -> "publickey"
  | `Checksum -> "checksum"
  | `Releases -> "releases"
  | `Authorisation -> "authorisation"

let string_to_resource = function
  | "publickey" -> Some `PublicKey
  | "checksum" -> Some `Checksum
  | "releases" -> Some `Releases
  | "authorisation" -> Some `Authorisation
  | _ -> None

let pp_resource ppf k = Format.pp_print_string ppf (resource_to_string k)


type role = [ `Author | `Janitor | `Other of string ]

let role_to_string = function
  | `Author -> "author"
  | `Janitor -> "janitor"
  | `Other x -> x

let string_to_role = function
  | "author" -> `Author
  | "janitor" -> `Janitor
  | x -> `Other x

let pp_role pp r = Format.pp_print_string pp (role_to_string r)

type verification_error = [
  | `InvalidBase64Encoding of identifier * string
  | `InvalidSignature of identifier * string * string
  | `InvalidPublicKey of identifier
  | `InvalidIdentifier of identifier
  | `NotAuthorised of identifier * identifier
  | `NoSignature of identifier
]

let pp_verification_error ppf = function
  | `InvalidBase64Encoding (id, data) -> Format.fprintf ppf "%a signature is not in valid base64 encoding %s" pp_id id data
  | `InvalidSignature (id, signature, data) -> Format.fprintf ppf "%a signature is not valid %a, data %s" pp_id id Utils.pp_hex signature data
  | `InvalidPublicKey id -> Format.fprintf ppf "keystore contained no valid public key for %s" id
  | `InvalidIdentifier id -> Format.fprintf ppf "identifier %s was not found in keystore" id
  | `NotAuthorised (auth, sign) -> Format.fprintf ppf "only %a is authorised to sign this index, but it is signed by %a" pp_id auth pp_id sign
  | `NoSignature s -> Format.fprintf ppf "no signature found on index %a" pp_id s

type error = [
  | `InvalidName of name * name
  | `InvalidResource of resource * resource
  | `NotSigned of name * resource
  | `InsufficientQuorum of identifier * S.t
  | `MissingSignature of identifier
]

let pp_error ppf = function
  | `InvalidName (w, h) -> Format.fprintf ppf "invalid name, looking for %a but got %a" pp_name w pp_name h
  | `InvalidResource (w, h) -> Format.fprintf ppf "invalid resource, looking for %a but got %a" pp_resource w pp_resource h
  | `NotSigned (n, r) -> Format.fprintf ppf "missing signature on %a, a %a" pp_name n pp_resource r
  | `InsufficientQuorum (id, goods) ->
    Format.fprintf ppf "quorum for %a not reached (valid: %s)" pp_id id (String.concat ", " (S.elements goods))
  | `MissingSignature id -> Format.fprintf ppf "missing signature from %a" pp_id id

let (>>=) a f =
  match a with
  | Ok x -> f x
  | Error e -> Error e

let (<<|>>) a b =
  match a, b with
  | Ok x, _ -> Ok x
  | _, Ok x -> Ok x
  | Error e, _ -> Error e

let guard p err = if p then Ok () else Error err

let rec foldM f n = function
  | [] -> Ok n
  | x::xs -> f n x >>= fun n' -> foldM f n' xs
