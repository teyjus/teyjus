type pos = Errormsg.pos
type symbol = Symbol.symbol

(*  Terms and Terms with Free Variables *)
type ptvarlist = Absyn.atypesymbol Table.SymbolTable.t
type ptterm = Term of (Absyn.aterm * Types.typemolecule)
type ptfixedterm = FixedTerm of (Absyn.afixedterm * Types.typemolecule)
type pttermandvariables = TermAndVariables of (ptterm * ptvarlist)
type ptfixedtermandvariables = FixedTermAndVariables of (ptfixedterm * ptvarlist)

(*  Stack and stack items.  *)
type stackdata =
  StackTerm of (ptterm)
| StackOp of (Absyn.aconstant * Absyn.atype list)

type stack = stackdata list

val topStack : stack -> stackdata
val popStack : stack -> stack

(*  Signifies an Error  *)
val errorTerm : ptterm

(*  Functions indicating what to do when a new constant or kind is encountered. *)
type ptnewconstant = Absyn.aconstant -> (Absyn.aconstant Table.SymbolTable.t) -> (Absyn.aconstant Table.SymbolTable.t)
type ptnewkind = Absyn.akind -> Absyn.akind Table.SymbolTable.t -> Absyn.akind Table.SymbolTable.t


val parseTerm : (Preabsyn.pterm->
                (Absyn.aconstant Table.SymbolTable.t) -> (Absyn.akind Table.SymbolTable.t) ->
                ptnewconstant -> ptnewkind ->
                ptfixedtermandvariables)

val translateTerm : (Preabsyn.pterm ->
                    ptvarlist -> ptvarlist ->
                    pttermandvariables)

val normalizeTerm : (Absyn.aterm -> Absyn.aterm)

val fixTerm : (Absyn.aterm -> ptvarlist -> Absyn.afixedterm)
