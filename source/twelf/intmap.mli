module IntMap : Map.S with type key = int

type 'a intmap = 'a IntMap.t

val empty : 'a intmap

val find : IntMap.key -> 'a IntMap.t -> 'a option
val add : IntMap.key -> 'a -> 'a IntMap.t -> 'a IntMap.t
val iter : (IntMap.key -> 'a -> unit) -> 'a IntMap.t -> unit
val fold : (IntMap.key -> 'a -> 'b -> 'b) -> 'a IntMap.t -> 'b -> 'b
