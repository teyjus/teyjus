type pos = Errormsg.pos
type symbol = Symbol.symbol

(*  Terms and Terms with Free Variables *)
type ptvarlist = Absyn.atypesymbol Table.SymbolTable.t
type ptterm = Term of (Absyn.aterm * Types.typemolecule)
type ptfixedterm = FixedTerm of (Absyn.afixedterm * Types.typemolecule)
type pttermandvariables = TermAndVariables of (ptterm * ptvarlist)
type ptfixedtermandvariables = FixedTermAndVariables of (ptfixedterm * ptvarlist)

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


val translateTerm : Preabsyn.pterm -> Absyn.amodule -> Absyn.afixedterm
val translateClauses : Preabsyn.pterm list -> Absyn.amodule -> Absyn.aclause list

