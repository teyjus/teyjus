module IntMap = Map.Make(struct type t = int let compare = Pervasives.compare end)
type 'a intmap = 'a IntMap.t

let empty = IntMap.empty

let find key table =
  try
    Some(IntMap.find key table)
  with
    Not_found -> None

let add key value table = IntMap.add key value table

let iter f table = IntMap.iter f table

let fold f v table = IntMap.fold f v table
