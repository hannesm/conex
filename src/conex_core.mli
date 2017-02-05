open Conex_result
open Conex_utils
(** Conex core definitions *)


(** {1 File system types} *)

(** The sum type of possible file types we expect *)
type file_type = File | Directory

(** A [path] is a list of strings *)
type path = string list

(** [path_to_string path] is [String.concat "/" path]. *)
val path_to_string : path -> string

(** [string_to_path str] is [String.cut "/" str]. *)
val string_to_path : string -> path


(** {1 Verification errors} *)

(** Potential error case when verifying a signature *)
type verification_error = [
  | `InvalidBase64Encoding
  | `InvalidSignature
  | `InvalidPublicKey
  | `NoSignature
]

(** [pp_verification_error pp] is a pretty printer for [verification_error]. *)
val pp_verification_error : Format.formatter -> verification_error -> unit


(** {1 Result combinators} *)

(** [r >>= f] is [f a] unless [r] is an [Error], which is propagated.  Monadic bind. *)
val (>>=) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result

(** [guard pred err] is either [Ok ()] (if [pred] holds), [Error err] otherwise. *)
val guard : bool -> 'a -> (unit, 'a) result

(** [foldM f a xs] applies [f] to each element of [xs], returns either [Ok] and
    the produced value, or [Error]. *)
val foldM : ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result

(** [foldS f a s] applies [f] to each element of the set [s], returns either
    [Ok] and the produced value, or [Error]. *)
val foldS : ('a -> string -> ('a, 'c) result) -> 'a -> S.t -> ('a, 'c) result
