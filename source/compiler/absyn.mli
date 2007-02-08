(*****************************************************************************
 *Absyn:                                                                     *
 * The abstract syntax representation.                                       *
 ****************************************************************************)
type pos = Errormsg.pos
type symbol = Symbol.symbol

(****************************************************************************
 *Kinds:                                                                    *
 * (symbol, arity, index)                                                   *
 ***************************************************************************)
type akindInfo = (symbol * int option * int option ref * pos) 

and akind = 
	LocalKind of akindInfo
  | GlobalKind of akindInfo
  | PervasiveKind of akindInfo

(*****************************************************************************
 *Type Variable Data:                                                        *
 * (firstuse, lastuse, perm, safety, heapvar, offset, firstgoal, lastgoal)   *
 ****************************************************************************)
and atypevar = 
    TypeVar of (atype option ref * atype option ref * bool option ref * 
				  bool option ref * bool option ref * int option ref * 
				  int option ref * int option ref)

(****************************************************************************
 * Type:                                                                    *
 ***************************************************************************)
and atype = 
	SkeletonVarType of (int ref)
  | TypeVarType of (atypevar option ref * bool option ref)
  |	ArrowType of (atype * atype)
  | AppType of (akind * atype list)
  | TypeSetType of (atype * atype list ref)
  | TypeRefType of (atype)
  | ErrorType

(****************************************************************************
 * Type Skeleton: (type, tyskel table index, adjust)                        *
 ***************************************************************************)

and askeleton = Skeleton of (atype * int option ref * bool ref)

(****************************************************************************
 * Type Abbreviations:                                                      *
 ***************************************************************************)
and atypeabbrev =
	TypeAbbrev of (symbol * symbol list * atype * pos)

(****************************************************************************
 *Constants:                                                                *
 * Symbol                                                                   *
 * Fixity                                                                   *
 * Precedence                                                               *
 * Export Def                                                               *
 * Use Only                                                                 *
 * No Defs                                                                  *
 * Closed                                                                   * 
 * (Type Preserving)                                                        *
 * Skeleton                                                                 *
 * Type Environment Size                                                    *
 * Neededness                                                               *
 * (Type Environment)                                                       *
 * Reducible (true for exportdef and anonymous constants or local constants *
 *            marked as useonly in the imported and accumulated modules)    *
 * Code Info                                                                *
 * Constant Type                                                            *
 * Index                                                                    *
 ***************************************************************************)
and aconstant = 
	Constant of (symbol * afixity * int * bool * bool * bool * bool ref *
				   bool * askeleton * int ref * bool array option ref * 
				   atype list * bool ref * acodeinfo option ref * 
				   aconstanttype * int ref * pos)

and aconstanttype =
	GlobalConstant
  | LocalConstant
  | PervasiveConstant of bool (*  Can this constant be redefined? *)
  | HiddenConstant
  | Anonymous

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
	Builtin
  | Clauses of aclausesblock

(*****************************************************************************
 * Variables (name based):                                                   * 
 *   (symbol, hidden constant, newtysy, type)                                *
 ****************************************************************************)
and atysyinfo = (symbol * aconstant option ref * bool ref * atype option ref)
 
and atysy = 
	ImplicitVar of atysyinfo
  | AnonImplicitVar of atysyinfo
  | BoundVar of atysyinfo

(*****************************************************************************
 * Variables (logic variables):                                              *
 *   (oneuse, perm, safety, heapvar, offset, firstgoal, lastgoal, lastuse)   *
 ****************************************************************************)
and avar = Var of (bool option ref * bool option ref * bool option ref  * 
					 bool option ref * int option ref * int option ref * 
					 int option ref * aterm option ref)

(*****************************************************************************
 * Variable occurrences:                                                     *
 ****************************************************************************)
and freevarinfo = 
	NamedFreeVar of (atysy)
  | FreeVar of (bool option ref * avar)

and boundvarinfo =
	NamedBoundVar of (atysy)
  | DBIndex of int
	
(****************************************************************************
 * Strings:                                                                 *
 ***************************************************************************)
and stringdata = (string * int option ref * bool option ref)

and strinfo =
	StrLiteral of (string)
  | StrData of (stringdata)

(****************************************************************************
 * Abstractions:                                                            *
 ***************************************************************************)
and abstractionInfo =
	NestedAbst of (atysy * aterm)
  | UNestedAbst of (atysy list * int * aterm)

(****************************************************************************
 * Applications:                                                            *
 ***************************************************************************)
and applicationInfo =
	CurriedApp of (aterm * aterm)
  | FirstOrderApp of (aterm * aterm list * int)

(****************************************************************************
 * Terms:                                                                   *
 ***************************************************************************)
and aterm =
	IntTerm         of (int * bool * pos option)
  | RealTerm        of (float * bool * pos option)
  | StringTerm      of (strinfo * bool * pos option)
  | ConstantTerm    of (aconstant * atype list * bool * pos option)
  | FreeVarTerm     of (freevarinfo * bool * pos option)
  | BoundVarTerm    of (boundvarinfo * bool * pos option)
  | AbstractionTerm of (abstractionInfo * bool * pos option)
  | ApplicationTerm of (applicationInfo * bool * pos option)
  | ErrorTerm

(****************************************************************************
 * Goals:                                                                   *
 ***************************************************************************)
and agoal = 
	AtomicGoal of (aconstant * int * int * aterm list * atype list)
  | AndGoal    of (agoal * agoal)
  | ImpGoal    of (adefinitions * avarinits * agoal)
  | AllGoal    of (ahcvarassoc * agoal)
  | SomeGoal   of (avar * agoal)
(*| OrGoal     of (agoal * agoal) *)

and adefinitions = ((aconstant * aclausesblock) list)

and avarinits = (avar list)

and ahcvarassoc = ((avar * aconstant) list)

(****************************************************************************
 * Clauses:                                                                 *
 * (head, args, tyargs, numargs, numtargs, body, offset, varmap, tyvarmap   *
 *  gesplist, cutvar, hasenv, impmods)                                      *
 ***************************************************************************)
(*
and aclause = 
    Clause of (aconstant * aterm list * atype list * int * int * agoal option *
				 atermvarmap * atypevarmap * int option ref * 
				 goalenvassoc ref * avar option ref * bool ref * impmods ref)
*)

and aclause = 
    Fact of (aconstant * aterm list * atype list * int * int * 
			   atermvarmap * atypevarmap * int option ref * impmods)
  | Rule of (aconstant * aterm list * atype list * int * int * atermvarmap *
			   atypevarmap * agoal * goalenvassoc ref * avar option ref *
			   bool ref * impmods)


(* Goal number and environment size association list*)
and goalenvassoc = ((int * int) list)

(* term variable map list *)
and atermvarmap  = ((avar * avar) list)

(* type variable map list *)
and atypevarmap  = ((atypevar * atypevar) list)

(* a clauses block: (clauses, last, offset, nextclause, closed, mapped) *)	  
and aclausesblock = (aclause list ref * aclause ref * int ref * 
					   int option ref * bool ref * bool ref)

(****************************************************************************
 * Modules:                                                                 *
 * (modname, imported, accumulated, kind table, constant table,             *
 *	type abbre table, string list, local kind list, global kind list,       *
 *  local constant list, global constant list, hidden constant list,        *
 *  skeleton list, hskeleton list, clauses blocks list)                     *
 ***************************************************************************)

and amodule = 
	 Module of (string * impmods * accmods * aconstant Table.SymbolTable.t *
				  akind Table.SymbolTable.t * atypeabbrev Table.SymbolTable.t *
				  strinfo list * akind list * akind list * aconstant list *
				  aconstant list * aconstant list * askeleton list *
				  askeleton list * aclauseinfo)
  |  Signature

and impmods = ((string * int * amodsig) list)

and accmods = ((string * amodsig) list)

(* global kinds list and global constants list *)
and amodsig = (akind list * aconstant list)

and aclauseinfo = 
	 ClauseBlocks of aclausesblock list
  |  PreClauseBlocks of adefinitions

(****************************************************************************)
(* Auxiliary functions                                                      *)
(****************************************************************************)
val tysyHiddenConst : atysy -> aconstant
val tysyType : atysy -> atype

val constantCodeInfoFd : aconstant -> acodeinfo option ref
val constantTypeEnvSize : aconstant -> int
