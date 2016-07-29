(** Provides symbol tables. *)

(** The type of tables. *)
type 'a table 

(** Construct an empty table. *)
val empty : 'a table

(** Insert an item into a table. *)
val insert : 'a table -> Symbol.symbol -> 'a -> 'a table

(** Find an item in a table. *)
val lookup : 'a table -> Symbol.symbol -> 'a option

(** Remove an item from a table. *)
val remove : 'a table -> Symbol.symbol -> 'a table

(** Apply a function to each item in a table. *)
val fold : 'a table -> (Symbol.symbol -> 'a -> 'b -> 'b) -> 'b -> 'b

(** Apply a function to each item in a table. *)
val iter : 'a table -> (Symbol.symbol -> 'a -> unit) -> unit
