(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Nathan Guermond, Steven Holte, 
*  Gopalan Nadathur, Xiaochu Qi, Zach Snow
****************************************************************************)
(****************************************************************************
* This file is part of Teyjus.
*
* Teyjus is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Teyjus is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************)

(*****************************************************************************
*Absyn:
* The abstract syntax representation.
*****************************************************************************)
type pos = Errormsg.pos
type symbol = Symbol.symbol

(*****************************************************************************
*Kinds:
*****************************************************************************)
type akindtype =
    LocalKind
  | GlobalKind
  | PervasiveKind
    
(* Kind(symbol, arity, index, kindtype, position) 
*
* The arity preserved when translating from the preabsract syntax.
* It is set to None if the kind declaration and its local declaration 
* are separated.
* After mergeGlobalKinds, no kind arity equals None (otherwise this would
* mean that a non existing kind is declared to be local) 
* 
* The index is set to 0 until the code generation. At this point a unique
* index will be assigned to each kind *)
and akind = Kind of (symbol * int option * int ref * akindtype * pos) 

(*****************************************************************************
*Type Variable Data:
* (firstuse, lastuse, perm, safety, heapvar, offset, firstgoal, lastgoal)
*
* These informations will be used during variables annotation. 
* Until this step, all references are useless and invalid.
* Fields are filled in Annvariables.initTypeVarData
*****************************************************************************)
and atypevar = 
  TypeVar of (atype option ref * atype option ref * bool ref * bool ref * 
              bool ref * int option ref * int ref * int ref)


(****************************************************************************
*Type Var Information:
* A BindableTypeVar is a type variable which, when set to None,
* can be bound during unification to any other type (including another
* BindableTypeVar)
* When they are created, they are thus set to None.
* They can be created during translation from preabsyn to absyn syntax and
* come from:
* - a lambda expression which does not have any type annotation. In this case
*   a type variable is created to denote the type of this abstracted variable
* - a free term variable (capital letter or underscore followed
*   by some lettres) or an anonymous, _without_ a type annotation.
*   The creation is only performed when the variable is met for the first time.
*   In this case a type variable is created to denote the type of this 
*   term variable
* - a type annotation of some term. In this case, all the variables appearing 
*   inside the type are translated as BindableTypeVar.
* They can also be used later when checking that an application 
* f t is well typed: if f has as typa variable X, then an arrow type 
* is created with two BindableTypeVar.
* 
* If during an unification at top level a BindableTypeVar is bound and then
* an error occurs, it has to be reset to None 
* 
* 
* FreeTypeVar are created only in Processclauses.transtype
* At this point, all BindableTypeVar are translated into FreeTypeVar,
* which is a form suitable for variable annotation.
* The first field represents all the information needed for the annotation,
* including the reference to the type.   
* The second field indicates if it is the first occurence of this variable.
* Initially, the second field is set to None until we know if it is the first
* or not.
*****************************************************************************)
and atypevarinfo =
    BindableTypeVar of atype option ref
  | FreeTypeVar of (atypevar option ref * bool option ref)


(*****************************************************************************
*Type:
* There are two places where preabsyn ptypes are translated in atype:
* - In the translation of the declaration of a constant's type.
*   In this case, every absyn type is formed of Application, Arrow 
*   and SkeletonVarType. The SkeletonVarType is just a convenient way to name
*   the different identifiers appearing in the preabsyn.
* - In the translation of a type annotation.
*   In this case every absyn type is formed of Application, Arrow and 
*   TypeVarType(BindableTypeVar()) 
*
*
*****************************************************************************)
and atype = 
    SkeletonVarType of (int ref)
  | TypeVarType of (atypevarinfo)
  |	ArrowType of (atype * atype)
  | ApplicationType of (akind * atype list)
  | TypeSetType of (atype * atype list ref * atype option ref)
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
* Skeleton Neededness
* Neededness
* Code Info
* Constant Type
* Index
* Position
***************************************************************************)
and aconstant = 
  Constant of (symbol * afixity ref * int ref * bool ref * bool ref *
	  bool ref * bool ref * bool ref * bool ref * askeleton option ref * 
    int ref * bool array option ref * bool array option ref *
    acodeinfo option ref * aconstanttype ref * int ref * pos)

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
and atypesymbolinfo = (symbol * aconstant option ref * bool ref * 
						 atype option ref)
 
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

(* After Processclauses.transTermBoundVar, only the second category remains *)
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
    IntTerm of (int * pos)
  | RealTerm of (float * pos)
  | StringTerm of (astringinfo * pos)
  | ConstantTerm of (aconstant * atype list * pos)
  | FreeVarTerm of (afreevarinfo * pos)
  | BoundVarTerm of (aboundvarinfo * pos)
  | AbstractionTerm of (aabstractioninfo * pos)
  | ApplicationTerm of (aapplicationinfo * pos)
  | ErrorTerm

(*****************************************************************************
*Goals:
*****************************************************************************)
and agoal = 
    AtomicGoal of (aconstant * int * int * aterm list * atype list)
  | AndGoal of (agoal * agoal)
  | ImpGoal of (adefinitions * avarinits * atypevarinits * agoal)
  | AllGoal of (ahcvarassoc * agoal)
  | SomeGoal of (avar * agoal)
  | CutFailGoal
(*| OrGoal of (agoal * agoal) *)

and adefinitions = Definitions of ((aconstant * aclausesblock) list)
and avarinits = VarInits of (avar list)
and atypevarinits = TypeVarInits of (atypevar list)
and ahcvarassoc = HCVarAssocs of ((avar * aconstant) list)


(**********************************************************************
*Clauses:
* (predicate, term args, type args, number term args, number type args, 
* term variable map, type variable map, logic variables, offset,  
* [body, gesplist, cut var, hasenv,] 
* imported modules)
*Note: term/type variable maps are always empty for top-level clauses
**********************************************************************)
and aclause = 
    Fact of (aconstant * aterm list * atype list * int * int * 
             atermvarmap * atypevarmap * avar list * int option ref * 
             aimportedmodule list)
  | Rule of (aconstant * aterm list * atype list * int * int * 
             atermvarmap * atypevarmap * avar list * int option ref * 
             agoal * agoalenvassoc ref * avar option ref * bool ref * 
             aimportedmodule list)

(* Goal number and environment size association list*)
and agoalenvassoc =  GoalEnvAssoc of ((int * int) list)

(* term variable map list *)
and atermvarmap  = TermVarMap of ((avar * avar) list)

(* type variable map list *)
and atypevarmap  = TypeVarMap of ((atypevar * atypevar) list)

(* clauses block: (clauses, closed, offset, nextclause)*)
and aclausesblock = (aclause list ref * bool ref * int ref * int option ref) 

(*****************************************************************************
*Modules:
* (modname, imported, accumulated, constant table, kind table,
* type abbre table, string list, global kind list, local kind list,
* global constant list, local constant list, hidden constant list,
* skeleton list, hskeleton list, clauses blocks list)
*****************************************************************************)
and amodule = 
    Module of (string * aimportedmodule list * aaccumulatedmodule list *
      aconstant Table.SymbolTable.t ref * akind Table.SymbolTable.t ref *
      atypeabbrev Table.SymbolTable.t * astringinfo list * akind list *
      akind list * aconstant list * aconstant list * aconstant list ref *
      askeleton list * askeleton list ref * aclauseinfo ref)
  | Signature of (string * akind list * aconstant list)
  | ErrorModule

and aimportedmodule = 
  ImportedModule of (string * amodule)

and aaccumulatedmodule =
  AccumulatedModule of (string * amodule)

and aclauseinfo = 
    ClauseBlocks of aclausesblock list
  | PreClauseBlocks of adefinitions

(*************************************************************************)
(*  akind:                                                               *)
(*************************************************************************)
val makeKindType : akind -> atype
val kinds_equal : akind -> akind -> bool
val getKindType : akind -> akindtype
val getKindArity : akind -> int
val getKindArityOption : akind -> int option
val getKindPos : akind -> pos
val getKindName : akind -> string
val getKindSymbol : akind -> symbol
val getKindIndex : akind -> int
val setKindIndex : akind -> int -> unit
val isGlobalKind : akind -> bool
val isLocalKind : akind -> bool
val isPervasiveKind : akind -> bool
val string_of_kind : akind -> string
val string_of_kind' : akind -> string

val makeGlobalKind : symbol -> int -> int -> akind
val makeLocalKind : symbol -> int -> int -> akind
(*************************************************************************)
(*  atypevar:                                                            *)
(*************************************************************************)
val getTypeVariableDataFirstGoal : atypevar -> int
val setTypeVariableDataFirstGoal : atypevar -> int -> unit
val getTypeVariableDataLastGoal : atypevar -> int
val setTypeVariableDataLastGoal : atypevar -> int -> unit
val getTypeVariableDataOffset : atypevar -> int
val setTypeVariableDataOffset : atypevar -> int -> unit
val getTypeVariableDataPerm   : atypevar -> bool
val setTypeVariableDataPerm   : atypevar -> bool -> unit
val getTypeVariableDataLastUse : atypevar -> atype
val getTypeVariableDataFirstUseOpt : atypevar -> atype option 
val getTypeVariableDataHeapVar : atypevar -> bool
val setTypeVariableDataHeapVar : atypevar -> bool -> unit
val getTypeVariableDataSafety  : atypevar -> bool
val setTypeVariableDataSafety  : atypevar -> bool -> unit

val makeNewTypeVariableData : unit -> atypevar
(*************************************************************************)
(*  atype:                                                               *)
(*************************************************************************)
val errorType : atype
val isErrorType : atype -> bool

val getArrowTypeTarget : atype -> atype
val getArrowTypeArguments : atype -> atype list
val getArrowTypeArity : atype -> int
val isArrowType : atype -> bool
val makeArrowType : atype -> atype list -> atype

val getTypeArguments : atype -> atype list
val getTypeKind : atype -> akind
val isConstantType : atype -> bool

val getTypeSetSet : atype -> atype list ref
val getTypeSetRef : atype -> atype option ref
val getTypeSetDefault : atype -> atype
val isTypeSetType : atype -> bool
val makeTypeSetVariable : atype -> atype list -> atype

val getTypeVariableReference : atype -> atype option ref
val dereferenceType : atype -> atype
val isVariableType : atype -> bool

val getTypeFreeVariableVariableData : atype -> atypevar
val getTypeFreeVariableFirstRef : atype -> bool option ref 
val getTypeFreeVariableFirst : atype -> bool
val setTypeFreeVariableFirst : atype -> bool -> unit
val isTypeFreeVariable : atype -> bool

(* Creates a BindableTypeVar *)
val makeTypeVariable : unit -> atype

val makeNewTypeVariable : atypevar -> atype 

val getSkeletonVariableIndex : atype -> int
val getSkeletonVariableIndexRef : atype -> int ref
val isSkeletonVariableType : atype -> bool

val getApplicationTypeHead : atype -> akind
val getApplicationTypeArgs : atype -> atype list
val isApplicationType : atype -> bool

val makeTypeEnvironment : int -> atype list

val string_of_type : atype -> string
val string_of_type_ast : atype -> string
val types_equal : atype -> atype -> bool
(*************************************************************************)
(*  askeleton:                                                           *)
(*************************************************************************)
val getSkeletonType : askeleton -> atype
val makeSkeleton : atype -> askeleton
val getSkeletonIndex : askeleton -> int
val setSkeletonIndex : askeleton -> int -> unit
val getSkeletonNew : askeleton -> bool
val setSkeletonNew : askeleton -> bool -> unit

val string_of_skeleton : askeleton -> string
val string_of_skeleton_ast : askeleton -> string

(*************************************************************************)
(* atypeabbrev                                                           *)
(*************************************************************************)
val getTypeAbbrevPos : atypeabbrev -> pos

(*************************************************************************)
(*  afixity:                                                             *)
(*************************************************************************)
val string_of_fixity : afixity -> string
val isFixityPrefix : afixity -> bool
val isFixityPostfix : afixity -> bool

(*************************************************************************)
(*  aconstant:                                                           *)
(*************************************************************************)
val getConstantRedefinable : aconstant -> bool
val getConstantReducible : aconstant -> bool
val getConstantPos : aconstant -> pos
val getConstantFixity : aconstant -> afixity
val getConstantFixityRef : aconstant -> afixity ref
val getConstantPrec : aconstant -> int
val getConstantPrecRef : aconstant -> int ref
val getConstantSymbol : aconstant -> symbol
val getConstantSkeletonValue : aconstant -> askeleton
val getConstantSkeleton : aconstant -> askeleton option
val getConstantSkeletonRef : aconstant -> askeleton option ref
val getConstantSkeletonNeededness : aconstant -> bool array option
val getConstantSkeletonNeedednessRef : aconstant -> bool array option ref
val getConstantNeededness : aconstant -> bool array option
val getConstantNeedednessRef : aconstant -> bool array option ref
val getConstantNeedednessValue : aconstant -> bool array
val getConstantName : aconstant -> string
val getConstantPrintName : aconstant -> string
val getConstantType : aconstant -> aconstanttype
val getConstantTypeRef : aconstant -> aconstanttype ref
val getConstantCodeInfo : aconstant -> acodeinfo option ref
val setConstantCodeInfo : aconstant -> acodeinfo option -> unit
val getConstantTypeEnvSize : bool -> aconstant -> int
val getConstantTypeEnvSizeRef : aconstant -> int ref
val getConstantNoDefs : aconstant -> bool
val getConstantNoDefsRef : aconstant -> bool ref
val getConstantClosed : aconstant -> bool
val getConstantClosedRef : aconstant -> bool ref
val getConstantUseOnly : aconstant -> bool
val getConstantUseOnlyRef : aconstant -> bool ref
val getConstantExportDef : aconstant -> bool
val getConstantExportDefRef : aconstant -> bool ref
val getConstantIndex : aconstant -> int
val setConstantIndex : aconstant -> int -> unit
val getConstantCodeInfoBuiltinIndex : aconstant -> int
(* retrieve the offset field in the clausesBlock of the pred *)
val getConstantCodeInfoClausesIndex : aconstant -> int

val constantHasCode  : aconstant -> bool
val isGlobalConstant : aconstant -> bool
val isLocalConstant : aconstant -> bool
val isPervasiveConstant : aconstant -> bool
val isAnonymousConstant : aconstant -> bool

val makeGlobalConstant : symbol -> afixity -> int -> bool -> bool -> int 
  -> askeleton -> int -> aconstant
val makeLocalConstant : symbol -> afixity -> int -> int -> askeleton ->
  int -> aconstant
val makeAnonymousConstant : int -> askeleton -> aconstant
val makeHiddenConstant : askeleton -> int -> aconstant
val makeConstantTerm : aconstant -> atype list -> pos -> aterm
(*  val makeConstantType : aconstant -> atype *)
val string_of_constant: aconstant -> string                                                           

(*************************************************************************)
(*  atypesymbol:                                                         *)
(*************************************************************************)
val getTypeSymbolName : atypesymbol -> string
val getTypeSymbolRawType : atypesymbol -> atype
val getTypeSymbolSymbol : atypesymbol -> symbol
val getTypeSymbolHiddenConstant : atypesymbol -> aconstant
val getTypeSymbolHiddenConstantRef : atypesymbol -> aconstant option ref
val getTypeSymbolType : atypesymbol -> atype
val copyTypeSymbol : symbol -> atypesymbol -> atypesymbol

(* val getTypeSymbolType : atypesymbol -> atypesymboltype *)

(*************************************************************************)
(*  avar:                                                                *)
(*************************************************************************)
val getVariableDataOffset : avar -> int
val setVariableDataOffset : avar -> int -> unit
val getVariableDataPerm   : avar -> bool
val setVariableDataPerm   : avar -> bool -> unit
val getVariableDataLastUse : avar -> aterm
val setVariableDataLastUse : avar -> aterm -> unit
val setVariableDataOneUse  : avar -> bool -> unit
val getVariableDataHeapVar : avar -> bool
val setVariableDataHeapVar : avar -> bool -> unit
val getVariableDataSafety  : avar -> bool
val setVariableDataSafety  : avar -> bool -> unit
val getVariableDataFirstGoal : avar -> int
val getVariableDataLastGoal : avar -> int
val setVariableDataLastGoal : avar -> int -> unit

val makeNewVariableData : unit -> avar
val makeCutVariableData : int -> avar
(*************************************************************************)
(*  aterm:                                                               *)
(*************************************************************************)
val string_of_term : aterm -> string
val string_of_term_ast : aterm -> string
val maxPrec : int

val getTermPos : aterm -> pos

val errorTerm : aterm

val getTermFreeVariableVariableData : aterm -> avar
val getTermFreeVariableFirst : aterm -> bool
val setTermFreeVariableFirst : aterm -> bool -> unit
val getTermFreeVariableTypeSymbol : aterm -> atypesymbol
val isTermFreeVariable : aterm -> bool
(* make a name based free variable *)
val makeFreeVarTerm : atypesymbol -> pos -> aterm

val getTermBoundVariableDBIndex : aterm -> int
(* make a name based bound variable *)
val makeBoundVarTerm : atypesymbol -> pos -> aterm

val getTermConstant : aterm -> aconstant
val getTermMoleculeEnv : aterm -> atype list
val getTermConstantTypeEnv : aterm -> atype list
val isTermConstant     : aterm -> bool

val getTermAbstractionVar : aterm -> atypesymbol
val getTermAbstractionVars : aterm -> atypesymbol list
val getTermAllAbstractionVars : atypesymbol list -> aterm -> atypesymbol list
val getTermAbstractionBody : aterm -> aterm
val getTermAbstractionNumberOfLambda : aterm -> int

val getTermApplicationHeadAndArguments : aterm -> (aterm * (aterm list))
val getTermApplicationHead : aterm -> aterm
val getTermApplicationArity : aterm -> int
val getTermApplicationArguments : aterm -> aterm list

val sameTermStructure : aterm -> aterm -> bool
(*************************************************************************)
(*  astringinfo:                                                         *)
(*************************************************************************)
val getStringInfoNew : astringinfo -> bool
val setStringInfoNew : astringinfo -> bool -> unit
val setStringInfoIndex : astringinfo -> int -> unit
val getStringInfoIndex : astringinfo -> int
val getStringInfoString : astringinfo -> string

(*************************************************************************)
(*  agoal:                                                               *)
(*************************************************************************)
val getAtomicGoalNumberOfArgs     : agoal -> int
val getAtomicGoalNumberOfTermArgs : agoal -> int
val getAtomicGoalTermArgs         : agoal -> aterm list
val getAtomicGoalTypeArgs         : agoal -> atype list
val getAtomicGoalPredicate        : agoal -> aconstant

val getAndGoalLeftOperand         : agoal -> agoal
val getAndGoalRightOperand        : agoal -> agoal

val getAllGoalHCVarAssocs         : agoal -> ahcvarassoc
val getAllGoalBody                : agoal -> agoal

val getSomeGoalQuantVar           : agoal -> avar
val getSomeGoalBody               : agoal -> agoal

val getImpGoalVarInits            : agoal -> avarinits
val getImpGoalTypeVarInits        : agoal -> atypevarinits
val getImpGoalClauses             : agoal -> adefinitions
val getImpGoalBody                : agoal -> agoal

(*************************************************************************)
(*  aclause:                                                             *)
(*************************************************************************)
val isClauseFact : aclause -> bool

val getClausePred : aclause -> aconstant
val getClauseNumberOfArgs : aclause -> int
val getClauseNumberOfTermArgs : aclause -> int
val getClauseTermArgs : aclause -> aterm list
val getClauseTypeArgs : aclause -> atype list
val getClauseOffset : aclause -> int
val setClauseOffset : aclause -> int -> unit
val getClauseGoal : aclause -> agoal
val getClauseCutVarOption : aclause -> avar option
val getClauseCutVar : aclause -> avar
val getClauseImports : aclause -> aimportedmodule list
val getClauseGespList : aclause -> agoalenvassoc
val setClauseGespList : aclause -> agoalenvassoc -> unit
val getClauseHasEnv : aclause -> bool
val getClauseTypeVarMaps : aclause -> atypevarmap
val getClauseTermVarMaps : aclause -> atermvarmap	

(*************************************************************************)
(*  agoalenvassoc:                                                       *)
(*************************************************************************)
(* get the nth entry in this list *) 
val getGoalEnvAssocNth : agoalenvassoc -> int -> (int * int)
(* get envsize in the nth entry in this list; 0 is returned if empty list*)
val getGoalEnvAssocNthEnvSize : agoalenvassoc -> int -> int

(*************************************************************************)
(*  amodule:                                                             *)
(*************************************************************************)
val getModuleName : amodule -> string
val setModuleName : amodule -> string -> amodule
val getModuleGlobalKindsList : amodule -> akind list
val getModuleGlobalConstantsList : amodule -> aconstant list
val getModuleLocalConstantsList : amodule -> aconstant list
val getModuleHiddenConstantsRef : amodule -> aconstant list ref
val getModuleHiddenConstantSkeletonsRef : amodule -> askeleton list ref
val getModuleHiddenConstantSkeletons : amodule -> askeleton list
val getModuleConstantTable : amodule -> aconstant Table.SymbolTable.t
val getModuleKindTable : amodule -> akind Table.SymbolTable.t
val getModuleTypeAbbrevTable : amodule -> atypeabbrev Table.SymbolTable.t

val getModuleClausesRef : amodule -> aclauseinfo ref
val getModuleClauses : amodule -> aclauseinfo
val setModuleClauses : amodule -> aclauseinfo -> unit

val getSignatureName : amodule -> string
val getSignatureGlobalKindsList : amodule -> akind list
val getSignatureGlobalConstantsList : amodule -> aconstant list

(*************************************************************************)
(*  aclauseinfo:                                                         *)
(*************************************************************************)
val getClauseInfoClauseBlocks : aclauseinfo -> aclausesblock list


(*************************************************************************)
(*  aclausesblock:                                                       *)
(*************************************************************************)
val getClauseBlockClauses : aclausesblock -> aclause list
val getClauseBlockClose : aclausesblock -> bool
val setClauseBlockClose : aclausesblock -> bool -> unit
val getClauseBlockNextClause : aclausesblock -> int
val setClauseBlockNextClause : aclausesblock -> int -> unit
val getClauseBlockOffset : aclausesblock -> int
val setClauseBlockOffset : aclausesblock -> int -> unit
(* if the given clause has a body as CutFailGoal, then set the second   *)
(* field true and add an empty list as the clauses list                 *)
val makeNewClauseBlock : aclause -> bool -> aclausesblock


                                              
val string_of_constant_type : aconstanttype -> string
