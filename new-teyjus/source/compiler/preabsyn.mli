(**********************************************************************
*Preabsyn Module:
*	The prebstract syntax for Teyjus.  This is more or less a direct
*	translation from the original C source.  I am sure there are numerous
*	places where it could be made better.
**********************************************************************)
type symbol = Symbol.symbol
type pos = Errormsg.pos

(*	Kinds of Identifiers	*)
type pidkind =
		CVID
	|	ConstID
	|	AVID
	|	VarID

type pfixitykind =
		Infix of pos
	|	Infixl of pos
	|	Infixr of pos
	|	Prefix of pos
	|	Prefixr of pos
	|	Postfix of pos
	|	Postfixl of pos

		
(*	Symbols	*)
type psymbol = Symbol of (symbol * pidkind * pos)

(*	Type Symbols	*)
and ptypesymbol = TypeSymbol of (symbol * ptype option * pidkind * pos)


(*	Types	*)
and ptype =
		Atom of (symbol * pidkind * pos)
	|	App of (ptype * ptype * pos)
	|	Arrow of (ptype * ptype * pos)
	|	TypeAbbrevCall of (symbol * ptype list * pos)
	|	ErrorType

and ptypeabbrev = TypeAbbrev of (symbol * symbol list * ptype * pos)

(*	Terms	*)
and pterm =
		SeqTerm	of (pterm list * pos)
	|	ListTerm of (pterm list * pos)
	|	ConsTerm of (pterm list * pterm * pos)
	|	IdTerm of (symbol * ptype option * pidkind * pos)
	|	RealTerm of (float * pos)
	|	IntTerm of (int * pos)
	|	StringTerm of (string * pos)
	|	OpTerm of (poperation * pos)

(*	Constants	*)
and pconstant = Constant of (psymbol list * ptype option * pos)

(*	Kinds	*)
and pkind = Kind of (psymbol list * int option * pos)

(*	Fixity	*)
and pfixity = Fixity of (psymbol list * pfixitykind * int * pos)

and poperation =
		COMMA
	|	PLUS
	|	MINUS
	|	TIMES
	|	LT
	|	LE
	|	GT
	|	GE
	|	UMINUS

(********************************************************************
*Module:
*	This type stores information about a preabsyn module.
*	Its fields are:
*		Name: string
*		Global Constants: pconstant list
*		Local Constants: pconstant list
*		Closed Constants: pconstant list
*
*		Accumulated Modules:
*		Accumulated Signatures:
********************************************************************)
type pmodule =
		Module of (string * pconstant list * pconstant list * pconstant list * pterm list)
	|	Signature of (string * pconstant list)

val printPreAbsyn : pmodule -> unit

(*	Accessors	*)
val fixityGetPos : pfixitykind -> pos

