type ('a, 'b) result = Ok of 'a | Error of 'b

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

type identifier = string

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
  | `InvalidRole of role * role
  | `InvalidPublicKey of identifier
  | `InvalidIdentifier of identifier
  | `InvalidCounter of string * int64 * int64
  | `InsufficientQuorum of string * identifier list * error list
  | `InvalidAuthorisation of string * string
  | `InvalidSignatures of string * error list
]

let rec pp_error ppf = function
  | `InvalidBase64Encoding (id, data) -> Format.fprintf ppf "%s signature is not in valid base64 encoding %s" id data
  | `InvalidSignature (id, kind, signature, data) -> Format.fprintf ppf "%s signature (a %s) is not valid %a, data %s" id (kind_to_string kind) Utils.pp_hex signature data
  | `InvalidRole (want, have) -> Format.fprintf ppf "role mismatch, wanted %a but public key has %a" pp_role want pp_role have
  | `InvalidPublicKey id -> Format.fprintf ppf "keystore contained no valid key for %s" id
  | `InvalidIdentifier id -> Format.fprintf ppf "identifier %s was not found in keystore" id
  | `InvalidCounter (id, old, nev) -> Format.fprintf ppf "key %s did not increase counter (old %Lu new %Lu)" id old nev
  | `InsufficientQuorum (id, goods, errs) ->
    Format.fprintf ppf "quorum for %s not reached (valid: %s), errors:" id (String.concat ", " goods) ;
    Format.pp_print_list pp_error ppf errs
  | `InvalidAuthorisation (anam, nam) -> Format.fprintf ppf "invalid authorisation for %s, responsible for %s" nam anam
  | `InvalidSignatures (id, errs) ->
    Format.fprintf ppf "bad signatures for %s, errors:@." id ;
    Format.pp_print_list pp_error ppf errs

let (>>=) a f =
  match a with
  | Ok x -> f x
  | Error e -> Error e

let guard p err = if p then Error err else Ok ()

let rec foldM f n = function
  | [] -> Ok n
  | x::xs -> f n x >>= fun n' -> foldM f n' xs

let rec anyM f = function
  | [] -> Error ()
  | x::xs -> match f x with
    | Ok x -> Ok x
    | Error _ -> anyM f xs
