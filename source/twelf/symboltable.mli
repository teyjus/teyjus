(** Provides symbol tables. *)

(** The type of tables. *)
type 'a table 

(** Construct an empty table. *)
val empty : 'a table

(** Insert an item into a table. *)
val insert : 'a table -> Symb.symbol -> 'a -> 'a table

(** Find an item in a table. *)
val lookup : 'a table -> Symb.symbol -> 'a option

(** Remove an item from a table. *)
val remove : 'a table -> Symb.symbol -> 'a table

(** Apply a function to each item in a table. *)
val fold : 'a table -> (Symb.symbol -> 'a -> 'b -> 'b) -> 'b -> 'b

(** Apply a function to each item in a table. *)
val iter : 'a table -> (Symb.symbol -> 'a -> unit) -> unit
