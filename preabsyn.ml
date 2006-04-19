(**********************************************************************
*Preabsyn Module:
*	The prebstract syntax for Teyjus.  This is more or less a direct
*	translation from the original C source.  I am sure there are numerous
*	places where it could be made better.
**********************************************************************)

module type PREABSYN =
sig
	type symbol
	type pos = int
	
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
	type psymbol = Symbol of (symbol * pos)
	
	(*	Type Symbols	*)
	and ptypesymbol = TypeSymbol of (symbol * ptype ref * pidkind * pos)
	

	(*	Types	*)
	and ptype =
			Atom of (symbol * pidkind * pos)
		|	App of (ptype * ptype * pos)
		|	Arrow of (ptype * ptype * pos)
		
	(*	Terms	*)
	and pterm =
			SeqTerm	of (pterm list * pos)
		|	ListTerm of (pterm list * pos)
		|	ConsTerm of (pterm list * pterm * pos)
		|	IdTerm of (symbol * ptype ref * pidkind * pos)
		|	RealTerm of (float * pos)
		|	IntTerm of (int * pos)
		|	StringTerm of (string * pos)
		|	OpTerm of (poperation * pos)

	(*	Constants	*)
	and pconstant = Constant of (psymbol list * ptype ref * pos)
	
	(*	Kinds	*)
	and pkind = Kind of (psymbol list * int ref * pos)
	
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
	
end

module PreAbsyn(Symbol : SYMBOL) : PREABSYN =
struct
	type symbol = Symbol.symbol
	type pos = int
	
	type symbol
	type pos = int
	
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
	type psymbol = Symbol of (symbol * pos)
	
	(*	Type Symbols	*)
	and ptypesymbol = TypeSymbol of (symbol * ptype ref * pidkind * pos)
	

	(*	Types	*)
	and ptype =
			Atom of (symbol * pidkind * pos)
		|	App of (ptype * ptype * pos)
		|	Arrow of (ptype * ptype * pos)
		
	(*	Terms	*)
	and pterm =
			SeqTerm	of (pterm list * pos)
		|	ListTerm of (pterm list * pos)
		|	ConsTerm of (pterm list * pterm * pos)
		|	IdTerm of (symbol * ptype ref * pidkind * pos)
		|	RealTerm of (float * pos)
		|	IntTerm of (int * pos)
		|	StringTerm of (string * pos)
		|	OpTerm of (poperation * pos)

	(*	Constants	*)
	and pconstant = Constant of (psymbol list * ptype ref * pos)
	
	(*	Kinds	*)
	and pkind = Kind of (psymbol list * int ref * pos)
	
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
end
