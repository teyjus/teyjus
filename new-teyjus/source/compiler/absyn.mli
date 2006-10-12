(**********************************************************************
*Absyn:
* The abstract syntax representation.
**********************************************************************)
type pos = Errormsg.pos
type symbol = Symbol.symbol

(********************************************************************
*atypevar:
* Information about a type variable.
********************************************************************)
type atypevar = 
    TypeVar of (atype * atype * bool * bool * bool * int * int * int)


(********************************************************************
*Kinds:
********************************************************************)
and akindmap =
    KindIndex of int
  | KindMapping of akindmap

and akind =
    LocalKind of (symbol * int option * akindmap * pos)
  | GlobalKind of (symbol * int option * akindmap * pos)
  | PervasiveKind of (symbol * int option * akindmap * pos)


(********************************************************************
*Type Abbreviations:
********************************************************************)
and atypeabbrev =
  TypeAbbrev of (symbol * symbol list * atype * pos)

(********************************************************************
*Type Skeleton:
********************************************************************)
and askeleton = Skeleton of (atype * int * bool)


(********************************************************************
*Types:
********************************************************************)
and atype =
    ArrowType of (atype * atype)
  | TypeVarType of (atype option ref * bool)
  | AppType of (akind * atype list)
  | SkeletonVarType of (int)
  | TypeSetType of (atype * atype list ref)
  | TypeRefType of (atype)
  | ErrorType

(********************************************************************
*Constants:
* Symbol
* Fixity
* Precedence
* Export
* Use Only
* No Defs
* Closed
* Type Preserving
* Skeleton
* Type
* Code Info
* Constant Type
********************************************************************)
and acodeinfo =
    Builtin
  | Clauses of aclause list
  
and aconstant =
    Constant of (symbol * afixity * int * bool * bool * bool * bool * bool * askeleton list * atype list * acodeinfo * int * aconstanttype * pos)

and aconstanttype = 
    GlobalConstant
  | LocalConstant
  | PervasiveConstant of bool (*  Can this constant be redefined? *)
  | NewConstant
  | HiddenConstant
  | AnonymousConstant

and atypesymboltype =
    RawType of atype
  | SkeletonType of (askeleton list * atype list * int)

and atypesymbol =
    ImplicitTypeSymbol of (bool * aconstant option * symbol * atypesymboltype)
  | AnonymousTypeSymbol of (bool * aconstant option * symbol * atypesymboltype)
  | BoundTypeSymbol of (bool * aconstant option * symbol * atypesymboltype)

and afixity =
    Infix
  | Infixl
  | Infixr
  | Prefix
  | Prefixr
  | Postfix
  | Postfixl
  | NoFixity

(********************************************************************
*Variables:
* Representation of explicitly or implicitly quantified variables in 
* clauses; this is used during code generation for clauses.
********************************************************************)
and avar = Var of (bool * bool * bool * bool * int * askeleton list * int * int * int * aterm)

(*  Kinds of variables  *)
and avarkind =
    ImplicitVar
  | AnonImplicitVar
  | BoundVar

(********************************************************************
*Terms:
********************************************************************)
and aterm =
    IntTerm of (int * pos)
  | StringTerm of (string * pos)
  | RealTerm of (float * pos)
  | AbstractionTerm of (atypesymbol * aterm * pos)
  | ConstantTerm of (aconstant * atype list * pos)
  | FreeVarTerm of (atypesymbol * pos)
  | BoundVarTerm of (atypesymbol * pos)  
  | ApplyTerm of (aterm * aterm * pos)
  | ErrorTerm

and afixedterm =
  | FixedIntTerm of (int)
  | FixedStringTerm of (string list)
  | FixedRealTerm of (float)
  | FixedAbstractionTerm of (atypesymbol list * afixedterm)
  | FixedConstTerm of (aconstant * atype list)
  | FixedBoundVarTerm of (int)
  | FixedApplyTerm of (aterm * aterm list * int)
  | FixedErrorTerm

and ahcvarpair = HCVarPair of (avar * atype list * aconstant)

(********************************************************************
*Goals and Definitions:
********************************************************************)
and agoal =
    AtomicGoal of (aconstant * int * int * aterm list * atype list)
  | ImplicationGoal of (adefinition list * avarinit list * agoal)
  | AndGoal of (agoal * agoal)
  | AllGoal of (ahcvarpair list * agoal)
  | SomeGoal of (avar * atype list * agoal)

and adefinition = Definition of (aconstant * aclause list)
and avarinit = VarInit of (avar * atype list)

(********************************************************************
*
********************************************************************)
and atermvarmap =  TermVarMap of (avar * avar) list

(********************************************************************
*Clauses:
********************************************************************)
and aclause =
    Clause of (aconstant * aterm list * atype list * int * int *
      agoal * int * agoal list * atermvarmap * atermvarmap * bool)

(********************************************************************
*String:
********************************************************************)
and astring = (string * int * bool)

(********************************************************************
*Module:
********************************************************************)
and amodule =
    Module of (string * aconstant Table.SymbolTable.t *
      akind Table.SymbolTable.t * atypeabbrev Table.SymbolTable.t * 
      astring list * aconstant list * 
      aconstant list * aconstant list * akind list * akind list *
      askeleton list * askeleton list * aclause list)
|   Signature

val printAbsyn : amodule -> out_channel -> unit

val getKindArity : akind -> int
val getKindPos : akind -> pos
val getKindName : akind -> string
val string_of_kind : akind -> string

val getConstantPos : aconstant -> pos
val getConstantFixity : aconstant -> afixity
val getConstantPrec : aconstant -> int
val getConstantSymbol : aconstant -> symbol
val getConstantSkeleton : aconstant -> askeleton
val getConstantName : aconstant -> string

val getSkeletonType : askeleton -> atype
val getSkeletonSize : askeleton -> int

val string_of_fixity : afixity -> string
val isFixityPrefix : afixity -> bool
val isFixityPostfix : afixity -> bool

val errorType : atype
val getArrowTypeTarget : atype -> atype
val getArrowTypeArguments : atype -> atype list
val getTypeVariableReference : atype -> atype option ref
val getTypeSetSet : atype -> atype list ref
val getTypeSetDefault : atype -> atype
val getTypeArguments : atype -> atype list
val dereferenceType : atype -> atype
val isArrowType : atype -> bool
val isVariableType : atype -> bool
val isTypeSetType : atype -> bool
val isConstantType : atype -> bool
val makeArrowType : atype -> atype list -> atype
val string_of_type : atype -> string
val isSkeletonVariableType : atype -> bool
val getSkeletonVariableIndex : atype -> int

val errorTerm : aterm
val getTermPos : aterm -> pos
val string_of_term : aterm -> string
val makeFreeVarTerm : atypesymbol -> pos -> aterm
val makeBoundVarTerm : atypesymbol -> pos -> aterm
val getTermAbstractionVar : aterm -> atypesymbol

val errorFixedTerm : afixedterm
val string_of_fixedterm : afixedterm -> string

val maxPrec : int

val getTypeSymbolType : atypesymbol -> atypesymboltype
val getTypeSymbolRawType : atypesymbol -> atype
val getTypeSymbolSymbol : atypesymbol -> symbol

val getModuleConstantTable : amodule -> aconstant Table.SymbolTable.t
val getModuleKindTable : amodule -> akind Table.SymbolTable.t
val getModuleTypeAbbrevTable : amodule -> atypeabbrev Table.SymbolTable.t
val getModuleClauses : amodule -> aclause list