(*****************************************************************************
*Absyn:
* The abstract syntax representation.
*****************************************************************************)
type pos = Errormsg.pos
type symbol = Symbol.symbol

(*****************************************************************************
*Kinds:
* (symbol, arity, index, position)
*****************************************************************************)
type akindinfo = (symbol * int option * int ref * pos) 

and akind = 
    LocalKind of akindinfo
  | GlobalKind of akindinfo
  | PervasiveKind of akindinfo

(*****************************************************************************
*Type Variable Data:
* (firstuse, lastuse, perm, safety, heapvar, offset, firstgoal, lastgoal)
*****************************************************************************)
and atypevar = 
  TypeVar of (atype option ref * atype option ref * bool ref * bool ref * 
				bool ref * int option ref * int ref * int ref)


(****************************************************************************
*Type Var Information:
*****************************************************************************)
and atypevarinfo =
	BindableTypeVar of atype option ref
  |	FreeTypeVar of (atypevar option ref * bool option ref)


(*****************************************************************************
*Type:
*****************************************************************************)
and atype = 
    SkeletonVarType of (int ref)
  | TypeVarType of (atypevarinfo ref)
  |	ArrowType of (atype * atype)
  | ApplicationType of (akind * atype list)
  | TypeSetType of (atype * atype list ref)
  | ErrorType

(*****************************************************************************
*Type Skeleton: (type, tyskel table index, adjust)
*****************************************************************************)
and askeleton = Skeleton of (atype * int option ref * bool ref)

(*****************************************************************************
*Type Abbreviations:
*****************************************************************************)
and atypeabbrev =
  TypeAbbrev of (symbol * symbol list * atype * pos)

(****************************************************************************
*Constants:
* Symbol
* Fixity
* Precedence
* Export Def
* Use Only
* No Defs
* Closed
* (Type Preserving)
* Reducible (true for exportdef and anonymous constants or local constants
*            marked as useonly in the imported and accumulated modules)
* Skeleton
* Type Environment Size
* Neededness
* Code Info
* Constant Type
* Index
* Position
***************************************************************************)
and aconstant = 
  Constant of (symbol * afixity ref * int ref * bool ref * bool ref *
    bool ref * bool ref * bool ref * bool ref * askeleton option ref * int ref *
    bool array option ref * acodeinfo option ref *
    aconstanttype ref * int ref * pos)

and aconstanttype =
    GlobalConstant
  | LocalConstant
  | PervasiveConstant of bool (*  Can this constant be redefined? *)
  | HiddenConstant
  | AnonymousConstant

and afixity = 
    Infix
  | Infixl
  | Infixr
  | Prefix
  | Prefixr
  | Postfix
  | Postfixl
  | NoFixity

and acodeinfo = 
    Builtin of int
  | Clauses of aclausesblock

(*****************************************************************************
*Variables (name based):
*   (symbol, hidden constant, newtysy, type)
*****************************************************************************)
and atypesymbolinfo = (symbol * aconstant option ref * bool ref * atype option ref)
 
and atypesymbol = 
    ImplicitVar of atypesymbolinfo
  | AnonymousImplicitVar of atypesymbolinfo
  | BoundVar of atypesymbolinfo

(*****************************************************************************
*Variables (logic variables):
*   (oneuse, perm, safety, heapvar, offset, firstgoal, lastgoal, lastuse)
*****************************************************************************)
and avar =
  Var of (bool option ref * bool ref * bool ref * bool ref * int option ref * 
			int ref * int ref * aterm option ref)

(*****************************************************************************
*Variable occurrences:
*****************************************************************************)
and afreevarinfo = 
    NamedFreeVar of (atypesymbol)
  | FreeVar of (avar * bool option ref)

and aboundvarinfo =
    NamedBoundVar of (atypesymbol)
  | DBIndex of int
	
(*****************************************************************************
*Strings:
*****************************************************************************)
and astringdata = (string * int option ref * bool option ref)

and astringinfo =
    StringLiteral of (string)
  | StringData of (astringdata)

(*****************************************************************************
*Abstractions:
*****************************************************************************)
and aabstractioninfo =
    NestedAbstraction of (atypesymbol * aterm)
  | UNestedAbstraction of (atypesymbol list * int * aterm)

(*****************************************************************************
*Applications:
*****************************************************************************)
and aapplicationinfo =
    CurriedApplication of (aterm * aterm)
  | FirstOrderApplication of (aterm * aterm list * int)

(*****************************************************************************
*Terms:
*****************************************************************************)
and aterm =
    IntTerm of (int * bool * pos)
  | RealTerm of (float * bool * pos)
  | StringTerm of (astringinfo * bool * pos)
  | ConstantTerm of (aconstant * atype list * bool * pos)
  | FreeVarTerm of (afreevarinfo * bool * pos)
  | BoundVarTerm of (aboundvarinfo * bool * pos)
  | AbstractionTerm of (aabstractioninfo * bool * pos)
  | ApplicationTerm of (aapplicationinfo * bool * pos)
  | ErrorTerm

(*****************************************************************************
*Goals:
*****************************************************************************)
and agoal = 
    AtomicGoal of (aconstant * int * int * aterm list * atype list)
  | AndGoal of (agoal * agoal)
  | ImpGoal of (adefinitions * avarinits * agoal)
  | AllGoal of (ahcvarassoc * agoal)
  | SomeGoal of (avar * agoal)
(*| OrGoal of (agoal * agoal) *)

and adefinitions = Definitions of ((aconstant * aclausesblock) list)
and avarinits = VarInits of (avar list)
and ahcvarassoc = HCVarAssocs of ((avar * aconstant) list)


(****************************************************************************
 *Clauses:
 * (head, args, tyargs, numargs, numtargs, body, offset, varmap, tyvarmap   *
 *gesplist, cutvar, hasenv, impmods)                                      *
 ***************************************************************************)
and aclause = 
    Fact of (aconstant * aterm list * atype list * int * int * 
			   atermvarmap * atypevarmap * avar list * int option ref * 
			   aimportedmodule list)
  | Rule of (aconstant * aterm list * atype list * int * int * atermvarmap *
			   atypevarmap * avar list * agoal * agoalenvassoc ref * 
			   avar option ref * bool ref * aimportedmodule list)

(* Goal number and environment size association list*)
and agoalenvassoc =  GoalEnvAssoc of ((int * int) list)

(* term variable map list *)
and atermvarmap  = TermVarMap of ((avar * avar) list)

(* type variable map list *)
and atypevarmap  = TypeVarMap of ((atypevar * atypevar) list)

(* clauses block: (clauses, last, offset, nextclause, closed, mapped) *)	  
and aclausesblock = (aclause list ref * aclause ref * int ref * 
					   int option ref * bool ref * bool ref)

(*****************************************************************************
*Modules:
* (modname, imported, accumulated, kind table, constant table,
* type abbre table, string list, local kind list, global kind list,
* local constant list, global constant list, hidden constant list,
* skeleton list, hskeleton list, clauses blocks list)
*****************************************************************************)
and amodule = 
    Module of (string * aimportedmodule list * aaccummulatedmodule list *
      aconstant Table.SymbolTable.t ref * akind Table.SymbolTable.t ref *
      atypeabbrev Table.SymbolTable.t * astringinfo list * akind list *
      akind list * aconstant list * aconstant list * aconstant list *
      askeleton list * askeleton list * aclauseinfo ref)
  | Signature of (string * akind list * aconstant list)
  | ErrorModule

and aimportedmodule = 
  ImportedModule of (string * int * amodule)

and aaccummulatedmodule =
  AccummulatedModule of (string * amodule)

and aclauseinfo = 
    ClauseBlocks of aclausesblock list
  | PreClauseBlocks of adefinitions

(*****************************************************************************
*Auxiliary functions:
*****************************************************************************)
val printAbsyn : amodule -> out_channel -> unit

val makeKindType : akind -> atype
val getKindArity : akind -> int
val getKindArityOption : akind -> int option
val getKindPos : akind -> pos
val getKindName : akind -> string
val string_of_kind : akind -> string

val makeAnonymousConstant : int -> aconstant
(*  val makeConstantType : aconstant -> atype *)
val makeConstantTerm : aconstant -> pos -> aterm
val getConstantRedefinable : aconstant -> bool
val getConstantPos : aconstant -> pos
val getConstantFixity : aconstant -> afixity
val getConstantFixityRef : aconstant -> afixity ref
val getConstantPrec : aconstant -> int
val getConstantPrecRef : aconstant -> int ref
val getConstantSymbol : aconstant -> symbol
val getConstantSkeleton : aconstant -> askeleton option
val getConstantSkeletonRef : aconstant -> askeleton option ref
val getConstantName : aconstant -> string
val getConstantType : aconstant -> aconstanttype
val getConstantTypeRef : aconstant -> aconstanttype ref
val getConstantCodeInfo : aconstant -> acodeinfo option ref
val getConstantTypeEnvSize : aconstant -> int
val getConstantTypeEnvSizeRef : aconstant -> int ref
val getConstantNoDefs : aconstant -> bool
val getConstantNoDefsRef : aconstant -> bool ref
val getConstantClosed : aconstant -> bool
val getConstantClosedRef : aconstant -> bool ref
val getConstantUseOnly : aconstant -> bool
val getConstantUseOnlyRef : aconstant -> bool ref
val makeHiddenConstant : askeleton -> aconstant

val getSkeletonType : askeleton -> atype
val getSkeletonIndex : askeleton -> int
val makeSkeleton : atype -> askeleton

val string_of_fixity : afixity -> string
val isFixityPrefix : afixity -> bool
val isFixityPostfix : afixity -> bool

val errorType : atype
val getArrowTypeTarget : atype -> atype
val getArrowTypeArguments : atype -> atype list
val makeTypeVariable : unit -> atype
val getTypeVariableReference : atype -> atype option ref
val getTypeSetSet : atype -> atype list ref
val getTypeSetDefault : atype -> atype
val getTypeArguments : atype -> atype list
val getTypeKind : atype -> akind
val dereferenceType : atype -> atype
val isArrowType : atype -> bool
val isVariableType : atype -> bool
val isTypeSetType : atype -> bool
val isConstantType : atype -> bool
val makeArrowType : atype -> atype list -> atype
val string_of_type : atype -> string
val isSkeletonVariableType : atype -> bool
val getSkeletonVariableIndex : atype -> int
val getSkeletonVariableIndexRef : atype -> int ref

val errorTerm : aterm
val string_of_term : aterm -> string
val makeFreeVarTerm : atypesymbol -> pos -> aterm
val makeBoundVarTerm : atypesymbol -> pos -> aterm
val getTermPos : aterm -> pos
val getTermConstant : aterm -> aconstant
val getTermTypeEnv : aterm -> atype list
val getTermAbstractionVar : aterm -> atypesymbol
val getTermAbstractionBody : aterm -> aterm

val maxPrec : int

(* val getTypeSymbolType : atypesymbol -> atypesymboltype *)
val getTypeSymbolName : atypesymbol -> string
val getTypeSymbolRawType : atypesymbol -> atype
val getTypeSymbolSymbol : atypesymbol -> symbol
val getTypeSymbolHiddenConstant : atypesymbol -> aconstant
val getTypeSymbolHiddenConstantRef : atypesymbol -> aconstant option ref
val getTypeSymbolType : atypesymbol -> atype

val getModuleConstantTable : amodule -> aconstant Table.SymbolTable.t
val getModuleKindTable : amodule -> akind Table.SymbolTable.t
val getModuleTypeAbbrevTable : amodule -> atypeabbrev Table.SymbolTable.t
val getModuleClauses : amodule -> aclauseinfo
val getModuleClausesRef : amodule -> aclauseinfo ref
val makeTypeEnvironment : int -> atype list
val makeTypeSetVariable : atype -> atype list -> atype

(* added by Xiaochu *)
val getTypeSymbolHiddenConst : atypesymbol -> aconstant
val getTypeSymbolType : atypesymbol -> atype

val setVariableDataLastGoal : avar -> int -> unit
val setTypeVariableDataLastGoal : atypevar -> int -> unit
val setVariableDataOffset : avar -> int -> unit
val setTypeVariableDataOffset : atypevar -> int -> unit

val isPermanentVariable : avar -> bool
val isPermanentTypeVariable : atypevar -> bool
val getVariableLastGoal : avar -> int
val getTypeVariableLastGoal : atypevar -> int
