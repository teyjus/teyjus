(** Maps strings to identifiers. *)

(** The type for unique identifiers. *)
type symbol

(** Provides the symbol for the given string. *)
val symbol : string -> symbol

(** Returns the name for the given symbol. *)
val name : symbol -> string

(** Returns the print name for the given symbol. *)
val printName : symbol -> string

(** Returns the unique integer identifying the symbol. *)
val id : symbol -> int

