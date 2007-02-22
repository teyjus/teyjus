type pos = Errormsg.pos
type symbol = Symbol.symbol

(*  Terms and Terms with Free Variables *)
type ptvarlist = Absyn.atypesymbol Table.SymbolTable.t
type ptterm = Term of (Absyn.aterm * Types.typemolecule)
type pttermandvariables = TermAndVariables of (ptterm * ptvarlist)

(*  Stack and stack items.  *)
type ptstackdata =
  StackTerm of (ptterm)
| StackOp of (Absyn.aconstant * Absyn.atype list * pos)
| StackError

(*  Parser State  *)
type ptparsestate =
  PrefixState
| PrefixrState
| PrefixWithArgState
| PrefixrWithArgState
| InfixState
| InfixrState
| InfixWithArgState
| InfixrWithArgState
| PostfixState
| TermState
| NoneState
| ErrorState

type ptstack = Stack of (ptstackdata list * ptparsestate * int * Absyn.afixity)

(*  Signifies an Error  *)
val errorTerm : ptterm

(*  Functions indicating what to do when a new constant or kind is encountered. *)
type ptnewconstant = Absyn.aconstant -> (Absyn.aconstant Table.SymbolTable.t) -> (Absyn.aconstant Table.SymbolTable.t)
type ptnewkind = Absyn.akind -> Absyn.akind Table.SymbolTable.t -> Absyn.akind Table.SymbolTable.t

val translateTerm : Preabsyn.pterm -> Absyn.amodule -> Absyn.aterm
val translateClause : Preabsyn.pterm -> Absyn.amodule -> Absyn.aclause list

val andTerm : Absyn.aterm
val implicationTerm : Absyn.aterm
