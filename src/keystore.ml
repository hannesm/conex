open Core

module K = Map.Make(String)
type t = Publickey.t K.t

let empty = K.empty

let size = K.cardinal

let mem store identifier = K.mem identifier store

let find store identifier = K.find identifier store

let add store pub = K.add pub.Publickey.keyid pub store

let remove store identifier = K.remove identifier store

let verify store data (id, ts, sigval) =
  if mem store id then
    let key = find store id in
    Publickey.verify key data (id, ts, sigval)
  else
    Error (`InvalidIdentifier id)
