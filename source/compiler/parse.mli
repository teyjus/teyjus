type pos = Errormsg.pos
type symbol = Symbol.symbol

(*	Terms and Terms with Free Variables	*)
type ptterm = Term of (Absyn.aterm * Types.typemolecule * pos)
type pttermandvariables = TermAndVariables of (ptterm )

(*	Signifies an Error	*)
val errorTerm : ptterm

(*	Operators	*)
type ptop = Op of (pos * Absyn.afixity * int * Absyn.aconstant * Absyn.atype list)

(*	The State of the parser	*)
type ptparsestate =
	PrefixState
|	PrefixrState
|	PrefixWithArgState
|	PrefixrWithArgState
|	InfixState
|	InfixrState
|	InfixWithArgState
|	InfixrWithArgState
|	PostfixState
|	NoneState
|	TermState
