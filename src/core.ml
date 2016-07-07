type ('a, 'b) result = Ok of 'a | Error of 'b

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

type identifier = string

let pp_id ppf x = Format.pp_print_string ppf x

type digest = string

let pp_digest ppf x = Format.pp_print_string ppf x

let digest data =
  let cs = Cstruct.of_string data in
  let check = Nocrypto.Hash.digest `SHA256 cs in
  let b64 = Nocrypto.Base64.encode check in
  Cstruct.to_string b64

type kind = [
  | `PublicKey
  | `Checksum
  | `Releases
  | `JanitorIndex
  | `Authorisation
]

let kind_to_string = function
  | `PublicKey -> "publickey"
  | `Checksum -> "checksum"
  | `Releases -> "releases"
  | `JanitorIndex -> "janitorindex"
  | `Authorisation -> "authorisation"

let string_to_kind = function
  | "publickey" -> Some `PublicKey
  | "checksum" -> Some `Checksum
  | "releases" -> Some `Releases
  | "janitorindex" -> Some `JanitorIndex
  | "authorisation" -> Some `Authorisation
  | _ -> None

let pp_kind ppf k = Format.pp_print_string ppf (kind_to_string k)

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

type error = [
  | `InvalidBase64Encoding of identifier * string
  | `InvalidSignature of identifier * kind * string * string
  | `InvalidPublicKey of identifier
  | `InvalidIdentifier of identifier
  | `InvalidCounter of string * int64 * int64
  | `InsufficientQuorum of string * identifier list
  | `InvalidAuthorisation of string * string
  | `InvalidReleases of string * string
  | `NotAuthorised of string
]

let pp_error ppf = function
  | `InvalidBase64Encoding (id, data) -> Format.fprintf ppf "%s signature is not in valid base64 encoding %s" id data
  | `InvalidSignature (id, kind, signature, data) -> Format.fprintf ppf "%s signature (a %s) is not valid %a, data %s" id (kind_to_string kind) Utils.pp_hex signature data
  | `InvalidPublicKey id -> Format.fprintf ppf "keystore contained no valid key for %s" id
  | `InvalidIdentifier id -> Format.fprintf ppf "identifier %s was not found in keystore" id
  | `InvalidCounter (id, old, nev) -> Format.fprintf ppf "key %s did not increase counter (old %Lu new %Lu)" id old nev
  | `InsufficientQuorum (id, goods) ->
    Format.fprintf ppf "quorum for %s not reached (valid: %s)" id (String.concat ", " goods)
  | `InvalidAuthorisation (anam, nam) -> Format.fprintf ppf "invalid authorisation for %s, responsible for %s" nam anam
  | `InvalidReleases (anam, rnam) -> Format.fprintf ppf "invalid releases for %s, releases for %s" rnam anam
  | `NotAuthorised name -> Format.fprintf ppf "%s is not authorised to sign this resource" name

let (>>=) a f =
  match a with
  | Ok x -> f x
  | Error e -> Error e

let (<<?>>) a b =
  match a, b with
  | Ok x, _ -> Ok x
  | _, Ok x -> Ok x
  | Error e, _ -> Error e


let guard p err = if p then Error err else Ok ()

let rec foldM f n = function
  | [] -> Ok n
  | x::xs -> f n x >>= fun n' -> foldM f n' xs
