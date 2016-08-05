(** Metadata generated during translation. *)

type metadata

val getLP : metadata -> Symb.symbol -> Symbol.symbol option
val getLF : metadata -> Symbol.symbol -> Symb.symbol option

(** Map an LF symbol to a generated LP symbol. *)
val new_mapping : metadata -> Symb.symbol -> metadata

(** Map an LF symbol to a specified LP symbol. *)
val set_mapping : metadata -> Symb.symbol -> Symbol.symbol -> metadata

(** Construct a string representation of the metadata for printing. *)
val string_of_metadata : metadata -> string
