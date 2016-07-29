type signature = Signature of (string * Lpabsyn.kinddecl Symboltable.table * Lpabsyn.typedecl Symboltable.table)

val print_signature : signature -> (string * string)

val get_filename : signature -> string
val get_kinds : signature -> Lpabsyn.kinddecl Symboltable.table
val get_types : signature -> Lpabsyn.typedecl Symboltable.table
