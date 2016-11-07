open Conex_core
open Conex_resource

type t = Publickey.t M.t

let empty = M.empty

let size = M.cardinal

let mem store identifier = M.mem identifier store

let find store identifier = M.find identifier store

let add store pub = M.add pub.Publickey.keyid pub store

let remove store identifier = M.remove identifier store
