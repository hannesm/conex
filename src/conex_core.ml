open Conex_result

module S = Set.Make(String)
module M = Map.Make(String)

(*BISECT-IGNORE-BEGIN*)
let pp_list pe ppf xs =
  Format.pp_print_string ppf "[" ;
  let rec p1 = function
    | [] -> Format.fprintf ppf "]@ "
    | [x] -> Format.fprintf ppf "%a]@ " pe x
    | x::xs -> Format.fprintf ppf "%a,@ " pe x ; p1 xs
  in
  p1 xs
(*BISECT-IGNORE-END*)

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


type pub = [ `Pub of string ]
type priv = [ `Priv of string ]


type name = string

(*BISECT-IGNORE-BEGIN*)
let pp_name ppf x = Format.pp_print_string ppf x
(*BISECT-IGNORE-END*)

let name_equal a b = String.compare a b = 0


type identifier = string

(*BISECT-IGNORE-BEGIN*)
let pp_id ppf x = Format.pp_print_string ppf x
(*BISECT-IGNORE-END*)

let id_equal a b = String.compare a b = 0


type digest = string

(*BISECT-IGNORE-BEGIN*)
let pp_digest ppf x = Format.pp_print_string ppf x
(*BISECT-IGNORE-END*)


type resource = [
  | `PublicKey
  | `Team
  | `Checksum
  | `Releases
  | `Authorisation
]

let resource_equal a b = match a, b with
  | `PublicKey, `PublicKey
  | `Team, `Team
  | `Checksum, `Checksum
  | `Releases, `Releases
  | `Authorisation, `Authorisation -> true
  | _ -> false

let resource_to_string = function
  | `PublicKey -> "publickey"
  | `Team -> "team"
  | `Checksum -> "checksum"
  | `Releases -> "releases"
  | `Authorisation -> "authorisation"

let string_to_resource = function
  | "publickey" -> Some `PublicKey
  | "team" -> Some `Team
  | "checksum" -> Some `Checksum
  | "releases" -> Some `Releases
  | "authorisation" -> Some `Authorisation
  | _ -> None

(*BISECT-IGNORE-BEGIN*)
let pp_resource ppf k = Format.pp_print_string ppf (resource_to_string k)
(*BISECT-IGNORE-END*)


type verification_error = [
  | `InvalidBase64Encoding of identifier
  | `InvalidSignature of identifier
  | `InvalidPublicKey of identifier
  | `InvalidIdentifier of identifier
  | `NotAuthorised of identifier * identifier
  | `NoSignature of identifier
]

(*BISECT-IGNORE-BEGIN*)
let pp_verification_error ppf = function
  | `InvalidBase64Encoding id -> Format.fprintf ppf "%a signature is not in valid base64 encoding" pp_id id
  | `InvalidSignature id -> Format.fprintf ppf "%a signature is not valid data" pp_id id
  | `InvalidPublicKey id -> Format.fprintf ppf "keystore contained no valid public key for %s" id
  | `InvalidIdentifier id -> Format.fprintf ppf "identifier %s was not found in keystore" id
  | `NotAuthorised (auth, sign) -> Format.fprintf ppf "only %a is authorised to sign this index, but it is signed by %a" pp_id auth pp_id sign
  | `NoSignature s -> Format.fprintf ppf "no signature found on index %a" pp_id s
(*BISECT-IGNORE-END*)

type base_v_err = [ `InvalidBase64 | `InvalidPubKey | `InvalidSig ]

let (>>=) a f =
  match a with
  | Ok x -> f x
  | Error e -> Error e

let guard p err = if p then Ok () else Error err

let rec foldM f n = function
  | [] -> Ok n
  | x::xs -> f n x >>= fun n' -> foldM f n' xs
