(** translate an LP term to the corresponding LF term.
    requires (1) the LF signature (just types for constants)
             (2) the mapping from LP constants to LF constants
             (3) LF types for the logic variables that may appear in the term
             (4) LF type corresponding to term being translated
             (5) LF types for the bound variables that may appear in the term 
                 (not an argument, collected while processing term) **)						   

(** a solution consists of a substitution and a disagreement set. *)
type lpsolution = (Absyn.atypesymbol * Absyn.aterm) list * (Absyn.aterm * Absyn.aterm) list
type lfsolution = (Lfabsyn.id * Lfabsyn.term) list * (Lfabsyn.term * Lfabsyn.term) list

val invert : Lfsig.signature -> Metadata.metadata -> Lfabsyn.typ Table.SymbolTable.t -> lpsolution -> lfsolution
