type pos = Errormsg.pos
type symbol = Symbol.symbol

(*	Terms and Terms with Free Variables	*)
type ptvarlist = Absyn.atypesymbol Table.SymbolTable.t
type ptterm = Term of (Absyn.afixedterm * Types.typemolecule * pos)
type ptfixedterm = FixedTerm of (Absyn.afixedterm * Types.typemolecule * pos)
type pttermandvariables = TermAndVariables of (ptterm * Type.typemolecule * ptvarlist)
type ptfixedtermandvariables = TermAndVariables of (ptfixedterm * Type.typemolecule * ptvarlist)

(*	Signifies an Error	*)
val errorTerm : ptterm

(*	Functions indicating what to do when a new constant or kind is encountered	*)
type ptnewconstant : Absyn.aconstant -> Absyn.aconstant Table.SymbolTable.t -> Absyn.aconstant Table.SymbolTable.t
type ptnewkind : Absyn.akind -> Absyn.akind Table.SymbolTable.t -> Absyn.akind Table.SymbolTable.t

val parseTerm : (Preabsyn.pterm->
								(Absyn.aconstant Table.SymbolTable.t) -> (Absyn.akind Table.SymbolTable.t) ->
								ptnewconstant -> ptnewkind ->
								ptfixedtermandvariables)
							
val translateTerm : (Preabsyn.pterm ->
										ptvarlist -> ptvarlist
										pttermandvariables)

val normalizeTerm : (Absyn.aterm -> Absyn.aterm)

val fixTerm : (Absyn.aterm -> ptvarlist -> Absyn.afixedterm)
