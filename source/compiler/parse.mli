type pos = Errormsg.pos
type symbol = Symbol.symbol

(*  Functions indicating what to do when a new constant or kind is encountered. *)
type ptnewconstant = symbol -> Absyn.aconstant Table.SymbolTable.t -> 
                                       Absyn.aconstant Table.SymbolTable.t

type ptnewkind = symbol -> int -> pos -> Absyn.akind Table.SymbolTable.t -> 
                                       Absyn.akind Table.SymbolTable.t

val translateTerm : Preabsyn.pterm -> Absyn.amodule ->
  Absyn.aterm * Types.typemolecule * Absyn.atypesymbol list * Absyn.atype list
    
val translateClause : Preabsyn.pterm -> Absyn.amodule -> 
                                    ptnewconstant -> ptnewkind -> Absyn.aterm

val removeNestedAbstractions : Absyn.aterm -> Absyn.aterm


val illegalConstant : Absyn.aconstant -> pos -> bool

val unitTests : unit -> unit
