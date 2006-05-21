type symbol = Symbol.symbol
type pos = Errormsg.pos

(********************************************************************
*atypevardata
*	Information about a type variable.
********************************************************************)
type atypevar = 
		TypeVar of (atype * atype * bool * bool * bool * int * int * int)


(********************************************************************
*Kinds:
********************************************************************)
and akindmap =
		KindIndex of int
	|	KindMapping of akindmap

and akind =
		LocalKind of (int * symbol * akindmap)
	|	GlobalKind of (int * symbol * akindmap)
	|	PervasiveKind of (int * symbol * akindmap)
	|	NewKind of (int * symbol * akindmap)
	|	HiddenKind of (int * symbol * akindmap)
	|	AnonymousKind of (int * symbol * akindmap)

(********************************************************************
*Type Abbreviations:
********************************************************************)
and atypeabbrev =
		TypeAbbrev of (symbol * atype list * atype)

(********************************************************************
*Type Skeleton:
********************************************************************)
and askeleton = Skeleton of (atype * int * bool)

(********************************************************************
*Types:
********************************************************************)
and atype =
		ArrowType of (atype * atype list)
	|	TypeVarType of (atypevar * bool)
	| FuncType of (int * akind * atype list)
	|	SkeletonVarType of (int)
	|	TypeRefType of (atype)
	|	ErrorType

and afixity =
		Infix
	|	Infixl
	|	Infixr
	|	Prefix
	|	Prefixl
	|	Postfix
	|	Postfixr

(********************************************************************
*Constants:
********************************************************************)
and aconstantmap =
		ConstantIndex of int
	|	ConstantMapping of aconstantmap

and acodeinfo =
		BuiltinIndex of int
	|	Clauses of aclause list
	
and aconstant =
		GlobalConstant of (afixity * bool * bool * bool * string * int * askeleton list * int * atype list * aconstantmap * acodeinfo)
	|	LocalConstant of (afixity * bool * bool * bool * string * int * askeleton list * int * atype list * aconstantmap * acodeinfo)
	|	PervasiveConstant of (afixity * bool * bool * bool * string * int * askeleton list * int * atype list * aconstantmap * acodeinfo)
	|	NewConstant of (afixity * bool * bool * bool * string * int * askeleton list * int * atype list * aconstantmap * acodeinfo)
	| HiddenConstant of (afixity * bool * bool * bool * string * int * askeleton list * int * atype list * aconstantmap * acodeinfo)
	|	AnonymousConstant of (afixity * bool * bool * bool * string * int * askeleton list * int * atype list * aconstantmap * acodeinfo)


and asymboltype =
		RawType of atype
	|	SkeletonType of (askeleton list * atype list * int)

and atypesymbol =
		ImplicitTypeSymbol of (bool * aconstant * symbol * asymboltype)
	|	AnonymousTypeSymbol of (bool * aconstant * symbol * asymboltype)
	|	BoundTypeSymbol of (bool * aconstant * symbol * asymboltype)

(********************************************************************
*Variables:
*	Representation of explicitly or implicitly quantified variables in 
*	clauses; this is used during code generation for clauses.
********************************************************************)
and avar = Var of (bool * bool * bool * bool * int * askeleton list * int * int * int * aterm)

(********************************************************************
*Terms:
********************************************************************)
and aterm =
		IntTerm of (int * pos)
	|	FixedIntTerm of (int)
	|	StringTerm of (string * pos)
	|	FixedStringTerm of (string list)
	|	RealTerm of (float * pos)
	|	FixedRealTerm of (float)
	
	|	AbstractionTerm of (atypesymbol * aterm * pos)
	|	SansAbstractionTerm of (atypesymbol * aterm)
	|	FixedAbstractionTerm of (atypesymbol * aterm)
	|	NNAbstractionTerm of (aterm)
	
	|	ConstTerm of (aconstant * atype list * pos)
	|	SansConstTerm of (aconstant * atype list)
	|	FixedConstTerm of (aconstant * atype list)
	
	|	FreeVarTerm of (atypesymbol * pos)
	|	SansFreeVarTerm of (atypesymbol)
	
	|	BoundVarTerm of (atypesymbol * pos)
	|	SansBoundVarTerm of (atypesymbol)
	|	FixedBoundVarTerm of (int)
	
	|	ApplyTerm of (aterm * aterm list * int * pos)
	|	SansApplyTerm of (aterm * aterm list * int)
	|	FixedApplyTerm of (aterm * aterm list * int)
	
	|	ErrorTerm

and ahcvarpair = HCVarPair of (avar * atype list * aconstant)

and adefinition = (aconstant * aclause list)

(********************************************************************
*
********************************************************************)
and avarinit = (avar * atype list)

(********************************************************************
*Goals:
********************************************************************)
and agoal =
		AtomicGoal of (aconstant * int * int * aterm list * atype list)
	|	ImplicationGoal of (adefinition list * avarinit list * agoal)
	|	AndGoal of (agoal * agoal)
	|	AllGoal of (ahcvarpair list * agoal)
	|	SomeGoal of (avar * atype list * agoal)

(********************************************************************
*
********************************************************************)
and atermvarmap =  TermVarMap of (avar * avar) list

(********************************************************************
*Imported/Accumulated Module Information:
********************************************************************)
and aannimpmodule = (int * aimpmodule list)
and aimpmodule = (int * amodule)

(********************************************************************
*Clauses:
********************************************************************)
and aclause =
		Clause of (aconstant * aterm list * atype list * int * int *
			agoal * int * agoal list * atermvarmap * atermvarmap * bool *
			aannimpmodule list)

(********************************************************************
*String:
********************************************************************)
and astring = (string * int * bool)

(********************************************************************
*Module:
********************************************************************)
and amodule =
		Module of (string * amodule list * atype Table.SymbolTable.t * atype Table.SymbolTable.t *
			astring list * akind list * aconstant list * askeleton list *
			askeleton list * aclause list)
