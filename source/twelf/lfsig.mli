(** Describes a representation for fully explicit LF signatures. *)

(** An LF signature.
    (file names, table of type-level declarations) *)
type signature = Signature of (string list ref* Lfabsyn.typefam Symboltable.table)

val string_of_sig : signature -> string

val get_filenames : signature -> string list
val get_typetable : signature -> Lfabsyn.typefam Symboltable.table
