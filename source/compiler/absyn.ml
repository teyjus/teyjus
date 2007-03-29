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
*Type Abbreviations:  (name, arguments, return type, position)
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
* Skeleton
* Type Environment Size
* Neededness
* (Type Environment)
* Reducible (true for exportdef and anonymous constants or local constants
*            marked as useonly in the imported and accumulated modules)
* Code Info
* Constant Type
* Index
* Position
***************************************************************************)
and aconstant = 
  Constant of (symbol * afixity ref * int ref * bool ref * bool ref *
    bool ref * bool ref * bool ref * bool ref * askeleton option ref *
    int ref * bool array option ref * acodeinfo option ref *
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

(**********************************************************************
*Clauses:
* (head, args, tyargs, numargs, numtargs, body, offset, varmap, tyvarmap
* gesplist, cutvar, hasenv, impmods)
**********************************************************************)
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
      akind list * aconstant list * aconstant list * aconstant list ref *
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

(**********************************************************************
*string_of_kind:
**********************************************************************)
let string_of_kind = function
  LocalKind(n,_,_,_) -> "LocalKind(" ^ (Symbol.name n) ^ ")"
| GlobalKind(n,_,_,_) -> "GlobalKind(" ^ (Symbol.name n) ^ ")"
| PervasiveKind(n,_,_,_) -> "PervasiveKind(" ^ (Symbol.name n) ^ ")"

(**********************************************************************
*makeKindType:
**********************************************************************)
let makeKindType kind =
  ApplicationType(kind, [])

(**********************************************************************
*getKindPos:
* Get a kind's position information.
**********************************************************************)
let getKindPos = function
  LocalKind(_,_,_,p) -> p
| GlobalKind(_,_,_,p) -> p
| PervasiveKind(_,_,_,p) -> p

(**********************************************************************
*getKindArity:
* Get a kind's arity.
**********************************************************************)
let getKindArity = function
  LocalKind(_,Some a,_,_) -> a
| GlobalKind(_,Some a,_,_) -> a
| PervasiveKind(_,Some a,_,_) -> a
| k -> (Errormsg.impossible (getKindPos k)  "getKindArity(): invalid kind arity")

let getKindArityOption = function
  LocalKind(_,a,_,_) -> a
| GlobalKind(_,a,_,_) -> a
| PervasiveKind(_,a,_,_) -> a

let getKindName = function
  LocalKind(n,_,_,_) -> (Symbol.name n)
| GlobalKind(n,_,_,_) -> (Symbol.name n)
| PervasiveKind(n,_,_,_) -> (Symbol.name n)

(**********************************************************************
* Skeleton Accessors
**********************************************************************)
let makeSkeleton ty =
  Skeleton(ty, ref None, ref true)

let getSkeletonType = function
  Skeleton(f,_,_) -> f

let getSkeletonIndex = function
  Skeleton(_,i,_) ->
    match !i with
      Some i' -> i'
    | None -> (Errormsg.impossible Errormsg.none
                "getSkeletonSize: Skeleton has no index")

(**********************************************************************
* Constant Accessors and Constructors:
**********************************************************************)
let getConstantCodeInfo = function
  Constant(_,_,_,_,_,_,_,_,_,_,_,_,ci,_,_,_) ->
    ci
  
let getConstantTypeEnvSize = function
  Constant(_,_,_,_,_,_,_,_,_,_,s,_,_,_,_,_) ->
    !s

let getConstantTypeEnvSizeRef = function
  Constant(_,_,_,_,_,_,_,_,_,_,s,_,_,_,_,_) ->
    s

let getConstantNoDefs = function
  Constant(_,_,_,_,_,nd,_,_,_,_,_,_,_,_,_,_) ->
    !nd

let getConstantNoDefsRef = function
  Constant(_,_,_,_,_,nd,_,_,_,_,_,_,_,_,_,_) ->
    nd

let getConstantClosed = function
  Constant(_,_,_,_,_,_,c,_,_,_,_,_,_,_,_,_) ->
    !c

let getConstantClosedRef = function
  Constant(_,_,_,_,_,_,c,_,_,_,_,_,_,_,_,_) ->
    c

let getConstantUseOnly = function
  Constant(_,_,_,_,u,_,_,_,_,_,_,_,_,_,_,_) ->
    !u

let getConstantUseOnlyRef = function
  Constant(_,_,_,_,u,_,_,_,_,_,_,_,_,_,_,_) ->
    u
   
let getConstantPos = function
  Constant(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,p) ->
    p

let getConstantSymbol = function
  Constant(sym,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> sym

let getConstantName = fun c ->
  (Symbol.name (getConstantSymbol c))

let getConstantSkeleton = function
  Constant(_,_,_,_,_,_,_,_,_,s,_,_,_,_,_,_) ->
    !s

let getConstantSkeletonRef = function
  Constant(_,_,_,_,_,_,_,_,_,s,_,_,_,_,_,_) ->
    s

let getConstantType = function
  Constant(_,_,_,_,_,_,_,_,_,_,_,_,_,ctype,_,_) ->
    !ctype


let getConstantTypeRef = function
  Constant(_,_,_,_,_,_,_,_,_,_,_,_,_,ctype,_,_) ->
    ctype

let getConstantRedefinable c =
  let t = getConstantType c in
  match t with
      PervasiveConstant(b) -> b
    | _ -> false

let getConstantFixityRef = function
  Constant(_,fix,_,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
    fix
    
let getConstantFixity = function
  Constant(_,fix,_,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
    !fix

let getConstantPrec = function
  Constant(_,_,prec,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
    !prec

let getConstantPrecRef = function
  Constant(_,_,prec,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
    prec

let makeAnonymousConstant i =
  Constant(Symbol.symbol "", ref NoFixity, ref (-1), ref true, ref false,
    ref false, ref true, ref false, ref false, ref None, ref i, ref None,
    ref None, ref AnonymousConstant, ref 0, Errormsg.none)

let makeHiddenConstant skel =
  Constant(Symbol.symbol "", ref NoFixity, ref (-1), ref true, ref false,
    ref false, ref true, ref false, ref false, ref (Some skel), ref 0, ref None,
    ref None, ref HiddenConstant, ref 0, Errormsg.none)

let makeConstantTerm c pos =
  let esize = getConstantTypeEnvSize c in
  if esize = 0 then
    ConstantTerm(c, [], false, pos)
  else
    Errormsg.impossible (getConstantPos c)
      "makeConstantTerm: constant has non-zero environment size"

(*
(**********************************************************************
*makeConstantType:
* Constructs a type representation of the given constant, assuming
* that the constant doesn't need an associated environment.
**********************************************************************)
let makeConstantType c =
  let env = ref [] in
  let instance ty =
    let ty' = dereferenceType ty in
    match ty' with
      TypeSetType(def, l) ->
        let l' = ref (List.iter instance (!l)) in
        let t = TypeSetType(def, l') in
        (env := t :: (!env);
        t)
    | ApplicationType(k, tl) ->
        ApplicationType(k, (List.iter instance tl))
    | ArrowType(l,r) -> ArrowType(instance l, instance r)
    | SkeletonVarType(_) -> ty'
    | TypeVarType(_) -> ty'
    | ErrorType -> ty'
  in
  
  let envsize = getConstantTypeEnvSize c in
  let skel = getConstantSkeleton c in
  
  if envsize <> 0 then
    Errormsg.impossible (getConstantPos c)
      "makeConstantMolecule: constant has non-zero environment size"
  else

  if Option.isNone skel then
    Errormsg.impossible (getConstantPos c)
      "makeConstantType: constant has no skeleton"
  else
  
  let ty = getSkeletonType (Option.get skel) in
  instance ty
*)

(**********************************************************************
*string_of_fixity:
* Convert an absyn fixity to a string.  Used only in print
**********************************************************************)
let string_of_fixity = function
  Infix -> "Infix"
| Infixl -> "Infixl"
| Infixr -> "Infixr"
| Prefix -> "Prefix"
| Prefixr -> "Prefixr"
| Postfix -> "Postfix"
| Postfixl -> "Postfixl"
| NoFixity -> "No Fixity"

let isFixityPrefix = function
  Prefix -> true
| Prefixr -> true
| _ -> false

let isFixityPostfix = function
  Postfix -> true
| Postfixl -> true
| _ -> false

(**********************************************************************
*printAbsyn:
**********************************************************************)
let printAbsyn = fun m out ->
  (*  Text output functions *)
  let output = function s -> (output_string out s) in
  let output_line = function s -> (output_string out (s ^ "\n")) in
  
  let printPos = fun p ->
    (output "Pos(";
    output (Errormsg.string_of_pos p);
    output ")")
  in
  
  let rec printTypeVar = function
    TypeVar(_) -> (output "TypeVar")
    
  and printTypeVarInfo = function
    BindableTypeVar(t) ->
      (match (!t) with
        Some t -> printType t
      | None -> output "None")
  | FreeTypeVar(tv, b) ->
      (match !tv with
        Some tvar -> printTypeVar tvar
      | None -> output "None")

  (*  Print an absyn type *)
  and printType = function
    TypeVarType(v) ->
      (output "TypeVar(";
      printTypeVarInfo (!v);
      output ")")
  | TypeSetType(def, l) ->
      let rec print' = function
        t::[] -> (printType t)
      | t::ts -> (printType t; output ", "; print' ts)
      | [] -> (output "")
      in
      
      (output "TypeSetType(";
      printType def;
      output "[";
      print' !l;
      output "])")
  | ArrowType(l, r) -> 
      (output "Arrow(";
      printType l;
      output ", ";
      printType r;
      output ")")
  | ApplicationType(f,t) ->
      let rec print' = function
        t::[] -> (printType t)
      | t::ts -> (printType t; output ", "; print' ts)
      | [] -> (output "None")
      in
      
      (output "App(";
      printKind f;
      output ", ";
      print' t;
      output ")")
  | SkeletonVarType(i) ->
      (output "SkeletonVar(";
      output (string_of_int !i);
      output ")")
  | ErrorType ->
      (output "Error")

  (*  Print a constant.  For use with printTable. *)
  and printConstant = fun sym const ->
    let printConstantType = function
      GlobalConstant -> output "Global"
    | LocalConstant -> output "Local"
    | AnonymousConstant -> output "Anonymous"
    | HiddenConstant -> output "Hidden"
    | PervasiveConstant(b) -> 
        if b then
          output "Mutable Pervasive"
        else
          output "Immutable Pervasive"
    in
    
    let rec printSkeleton = function
      None -> output "None"
    | Some(Skeleton(t,i,b)) -> 
        (output "Skeleton(";
        printType t;
        output (", " ^ (Option.string_of_option !i string_of_int));
        output (", " ^ (string_of_bool !b));
        output ")")
    in
    
    match const with
      Constant(sym,fix,prec,exportdef,useonly,nodefs,closed,typepreserv,
        reducible,skel,envsize,_,codeinfo,ctype,index,pos) ->
        (output "Constant(";
        output (Symbol.name sym);
        output ", ";
        output (string_of_fixity !fix);
        output ", ";
        output (string_of_int !prec);
        output ", ";
        output (string_of_bool !exportdef);
        output ", ";
        output (string_of_bool !useonly);
        output ", ";
        output (string_of_bool !nodefs);
        output ", ";
        output (string_of_bool !closed);
        output ", ";
        output (string_of_bool !typepreserv);
        output ", ";
        output (string_of_bool !reducible);
        output ", ";
        printSkeleton !skel;
        output ", ";
        printConstantType !ctype;
        output ", ";
        printPos pos;
        output_line ")")
  
  (*  Print a kind.  For use with printTable. *)
  and printKind = fun k ->
    let print' = fun sym a kmap ->
      match a with
        Some a' ->
          (output (Symbol.name sym);
          output ", ";
          output (string_of_int a'))
      | None ->
          (output (Symbol.name sym);
          output ", ";
          output "None")
    in
    
    match k with
      LocalKind(sym,arity,kmap,p) ->
        (output "LocalKind(";
        print' sym arity kmap;
        output ")")
    | GlobalKind(sym,arity,kmap,p) ->
        (output "GlobalKind(";
        print' sym arity kmap;
        output ")")
    | PervasiveKind(sym,arity,kmap,p) ->
        (output "PervasiveKind(";
        print' sym arity kmap;
        output ")")
  in
  
  (*  Print the contents of a table *)
  let printTable = fun f table ->
    Table.iter f table
  in
  
  let printkind' = fun s k ->
    (output (Symbol.name s);
    output " : ";
    printKind k;
    output_line "")
  in

  match m with
    Module(name, impmods, accummods, ctable, ktable, tabbrevtable, strings,
      gkinds, lkinds, gconsts, lconsts, hconsts, gskels, lskels, clauses) ->
      
      (output "Module(";
      output name;
      output_line ", ";
      
      output_line "ConstantTable:";
      printTable printConstant !ctable;
      output_line "";
      
      output_line "KindTable:";
      printTable printkind' !ktable;
      output_line ")")
      
  | Signature(name, consts, kinds) ->
      (output "Signature(";
      output name;
      output_line ")")
  | ErrorModule ->
      (output "ErrorModule")

(**********************************************************************
*Various type accessors and values.
**********************************************************************)
let errorType = ErrorType

let rec string_of_type = fun ty ->
  let rec print' = function
      t::[] -> (string_of_type t)
    | t::ts -> (string_of_type t) ^ ", " ^ (print' ts)
    | [] -> ""
  in

  match ty with
    ArrowType(t1, t2) -> "ArrowType(" ^ (string_of_type t1) ^ ", " ^ (string_of_type t2) ^ ")"
  | TypeVarType(_) -> "TypeVarType(#ERROR#)"
  | ApplicationType(kind, tlist) -> "ApplicationType(" ^ (string_of_kind kind) ^ ", " ^ (print' tlist) ^ ")"
  | SkeletonVarType(i) -> "SkeletonVarType(" ^ (string_of_int !i) ^ ")"
  | TypeSetType(d, tlist) -> "TypeSetType(" ^ (string_of_type d) ^ ", " ^ (print' (!tlist)) ^ ")"
  | ErrorType -> "ErrorType"

let rec getArrowTypeTarget = function
  ArrowType(l, r) -> (getArrowTypeTarget r)
| t -> t

let rec getArrowTypeArguments = fun ty ->
  let rec get' = function
    ArrowType(l,r) -> l :: (get' r)
  | t -> []
  in
  
  match ty with
    ArrowType(_) -> get' ty
  | t -> Errormsg.impossible Errormsg.none ("getArrowTypeArguments: invalid type: " ^ (string_of_type t))

let getTypeSetSet = function
  TypeSetType(_, set) -> set
| _ -> (Errormsg.impossible Errormsg.none "getTypeSetType: invalid type")

let getTypeSetDefault = function
  TypeSetType(def, _) -> def
| _ -> (Errormsg.impossible Errormsg.none "getTypeSetType: invalid type")

let makeTypeVariable () = TypeVarType(ref (FreeTypeVar(ref None, ref None)))
let makeTypeSetVariable def tl = TypeSetType(def, ref tl)

let rec makeTypeEnvironment i =
  match i with
      0 -> []
    | i' ->
        if i' < 0 then
          Errormsg.impossible Errormsg.none "makeTypeEnvironment: invalid environment size"
        else
          (makeTypeVariable ()) :: (makeTypeEnvironment (i - 1))

let isPermanentTypeVariable = function
  TypeVar(fu, lu, p, s, h, o, fg, lg) -> !p

let getTypeVariableLastGoalRef = function
  TypeVar(fu, lu, p, s, h, o, fg, lg) -> lg
let getTypeVariableLastGoal v = !(getTypeVariableLastGoalRef v)
let setTypeVariableLastGoal v o = (getTypeVariableLastGoalRef v) := o

let getTypeVariableFirstGoalRef = function
  TypeVar(fu, lu, p, s, h, o, fg, lg) -> fg
let getTypeVariableFirstGoal v = !(getTypeVariableFirstGoalRef v)
let setTypeVariableFirstGoal v o = (getTypeVariableFirstGoalRef v) := o

let getTypeVariableOffsetRef = function
  TypeVar(fu, lu, p, s, h, o, fg, lg) -> o
let getTypeVariableOffset v = !(getTypeVariableOffsetRef v)
let setTypeVariableOffset v o =
  (getTypeVariableOffsetRef v) := Some o

let getTypeVariableReference = function
  TypeVarType(info) ->
    (match !info with
      BindableTypeVar(r) -> r
    | _ -> (Errormsg.impossible Errormsg.none "getTypeVariableReference: invalid type variable info"))
| _ -> (Errormsg.impossible Errormsg.none "getTypeVariableReference: invalid type")

let getTypeArguments = function
  ApplicationType(_, args) -> args
| t -> (Errormsg.impossible Errormsg.none ("getTypeArguments: invalid type: " ^ (string_of_type t)))

let getTypeKind = function
  ApplicationType(k,_) -> k
| t -> (Errormsg.impossible Errormsg.none ("getTypeKind: invalid type: " ^ (string_of_type t)))
let isArrowType = function
  ArrowType(_) -> true
| _ -> false

let isTypeSetType = function
  TypeSetType(_) -> true
| _ -> false

let isConstantType = function
  ApplicationType(_, args) -> (List.length args) = 0
| _ -> false

let isVariableType = function
  TypeVarType(r) ->
    (match !r with
      BindableTypeVar(tr) ->
        (match !tr with
          Some(t) -> (Errormsg.impossible Errormsg.none "isVariableType: bound variable")
        | None -> true)
    | _ -> (Errormsg.impossible Errormsg.none "isVariableType: type variable info"))
| _ -> false

let isSkeletonVariableType = function
  SkeletonVarType(_) -> true
| _ -> false

let getSkeletonVariableIndex = function
  SkeletonVarType(i) -> !i
| _ -> (Errormsg.impossible Errormsg.none "getSkeletonVariableIndex: invalid type")

let getSkeletonVariableIndexRef = function
  SkeletonVarType(i) -> i
| _ -> (Errormsg.impossible Errormsg.none "getSkeletonVariableIndex: invalid type")

let rec makeArrowType = fun targ args ->
  match args with
    arg::args -> ArrowType(arg, (makeArrowType targ args))
  | [] -> (targ)

(**********************************************************************
*dereferenceType:
* Dereference a type.
**********************************************************************)
let rec dereferenceType = fun ty ->
  match ty with
    TypeVarType(r) ->
      (match !r with
        BindableTypeVar(tr) ->
          (match !tr with
            Some(t) -> dereferenceType t
          | None -> ty)
      | FreeTypeVar(_) ->
          Errormsg.impossible Errormsg.none "dereferenceType: Invalid type variable")
  | _ -> ty

let maxPrec = 255
let appFixity = Infixl
let appPrec = maxPrec + 2

(**********************************************************************
*Various term accessors and constructors.
**********************************************************************)
let getTermPos = function
  IntTerm(_,_,p) -> p
| StringTerm(_,_,p) -> p
| RealTerm(_,_,p) -> p
| AbstractionTerm(_,_,p) -> p
| ConstantTerm(_,_,_,p) -> p
| FreeVarTerm(_,_,p) -> p
| BoundVarTerm(_,_,p) -> p
| ApplicationTerm(_,_,p) -> p
| ErrorTerm -> Errormsg.none

let getTermAbstractionVar = function
  AbstractionTerm(NestedAbstraction(v,_),_,_) -> v
| _ -> Errormsg.impossible Errormsg.none "getTermAbstractionVar: invalid term"

let getTermAbstractionBody = function
  AbstractionTerm(NestedAbstraction(_,b),_,_) -> b
| _ -> Errormsg.impossible Errormsg.none "getTermAbstractionBody: invalid term"

let getTermConstant = function
  ConstantTerm(c,_,_,_) -> c
| _ -> Errormsg.impossible Errormsg.none "getTermConstant: invalid term"

let getTermTypeEnv = function
  ConstantTerm(_,te,_,_) -> te
| _ -> Errormsg.impossible Errormsg.none "getTermConstant: invalid term"

let errorTerm = ErrorTerm

let makeFreeVarTerm tsym pos =
  FreeVarTerm(NamedFreeVar(tsym), false, pos)

let makeBoundVarTerm tsym pos =
  BoundVarTerm(NamedBoundVar(tsym), false, pos)

let getTypeSymbolType ty =
  let get' t =
    if Option.isSome t then
      Option.get t
    else
      Errormsg.impossible Errormsg.none "getTypeSymbolType: invalid type"
  in
  match ty with
    ImplicitVar(_,_,_,t) -> get' !t
  | AnonymousImplicitVar(_,_,_,t) -> get' !t
  | BoundVar(_,_,_,t) -> get' !t

let getTypeSymbolHiddenConstantRef tsym =
  match tsym with
    ImplicitVar(_,c,_,_) -> c
  | AnonymousImplicitVar(_,c,_,_) -> c
  | BoundVar(_,c,_,_) -> c

let getTypeSymbolHiddenConstant tsym =
  let get' = function
      Some(c) -> c
    | None -> Errormsg.impossible Errormsg.none "getTypeSymbolHiddenConstant: type symbol has no constant"
  in  
  get' !(getTypeSymbolHiddenConstantRef tsym)

let getTypeSymbolRawType = fun s ->
  let get' = function
      Some(t) -> t
    | _ -> Errormsg.impossible Errormsg.none "getTypeSymbolRawType: type symbol has no type"
  in
  
  match s with
    ImplicitVar(_,_,_,t) -> get' !t
  | AnonymousImplicitVar(_,_,_,t) -> get' !t
  | BoundVar(_,_,_,t) -> get' !t

let getTypeSymbolSymbol = function
    ImplicitVar(s,_,_,_) -> s
  | AnonymousImplicitVar(s,_,_,_) -> s
  | BoundVar(s,_,_,_) -> s

let getTypeSymbolName s = (Symbol.name (getTypeSymbolSymbol s))

(**********************************************************************
*avar accessors.
**********************************************************************)
let isPermanentVariable = function
  Var(oneuse, perm, safety, heapvar, offset, firstgoal, lastgoal, lastuse) ->
    !perm

let getVariableDataLastGoalRef = function
  Var(oneuse, perm, safety, heapvar, offset, firstgoal, lastgoal, lastuse) ->
    lastgoal
let getVariableDataLastGoal v = !(getVariableDataLastGoalRef v)
let setVariableDataLastGoal v o = (getVariableDataLastGoalRef v) := o

let getVariableDataOffsetRef = function
  Var(oneuse, perm, safety, heapvar, offset, firstgoal, lastgoal, lastuse) ->
    offset
let setVariableDataOffset v o =
  (getVariableDataOffsetRef v) := Some o

  

(**********************************************************************
*Term Context:
* Keeps track of what state the converter is in when going from
* a term to a string.
**********************************************************************)
type atermcontext =
  LeftTermContext
| RightTermContext
| WholeTermContext

(**********************************************************************
*needsParens:
* Determins whether or not to output parentheses based on the current
* operator precedence and fixity.
**********************************************************************)
let rec needsParens = fun opfix opprec context fix prec ->
  let check1 = fun () -> opprec <= prec in
  let check2 = fun () -> opprec < prec in
  let checkLeft = fun () ->
    match opfix with
      Infix -> check2 ()
    | Infixl -> check2 ()
    | Prefix -> check2 ()
    | Postfix -> check2 ()
    | Postfixl -> check2 ()
    | _ -> check1 ()
  in
  
  let checkRight = fun () ->
    match opfix with
      Infixl -> check1 ()
    | Postfixl -> check1 ()
    | _ -> check2 ()
  in
  
  match context with
    LeftTermContext ->
      (match fix with
        Infix -> check1 ()
      | Infixr -> check1 ()
      | Postfix -> check1 ()
      
      | Infixl -> checkLeft ()
      | Postfixl -> checkLeft ()
      | _ -> (Errormsg.impossible Errormsg.none "needsParens: invalid fixity"))

  | RightTermContext ->
      (match fix with
        Infix -> check1 ()
      | Infixl -> check1 ()
      | Prefix -> check1 ()
      
      | Infixr -> checkRight ()
      | Prefixr -> checkRight ()
      | _ -> (Errormsg.impossible Errormsg.none "needsParens: invalid fixity"))
  | WholeTermContext -> false

(**********************************************************************
*string_of_term:
* Converts an absyn term to a string representation.
**********************************************************************)
let string_of_term = fun term ->
  let rec string_of_prefixterm = fun op opfix args context fix prec ->
    let opprec = getConstantPrec op in
    let paren = needsParens opfix opprec context fix prec in
    let result = (getConstantName op) ^ " " ^ (string_of_term' (List.hd args) RightTermContext opfix opprec) in

    if paren then
      "(" ^ result ^ ")"
    else
      result
  
  and string_of_infixterm = fun op opfix args context fix prec ->
    let opprec = getConstantPrec op in
    let paren = needsParens opfix opprec context fix prec in
    let result = (string_of_term' (List.hd args) LeftTermContext opfix opprec) ^ 
      " " ^ (getConstantName op) ^ " " ^
      (string_of_term' (List.hd (List.tl args)) RightTermContext opfix opprec) in
    
    if paren then
      "(" ^ result ^ ")"
    else
      result
  
  and string_of_postfixterm = fun op opfix args context fix prec ->
    let opprec = getConstantPrec op in
    let paren = needsParens opfix opprec context fix prec in
    let result = (string_of_term' (List.hd args) LeftTermContext opfix opprec) ^ " " ^ (getConstantName op) in
    
    if paren then
      "(" ^ result ^ ")"
    else
      result
  
  and string_of_app = fun term context fix prec ->
    let rec string_of_args args =
      match args with
          [] -> ""
        | a::[] -> (string_of_term' a WholeTermContext appFixity appPrec)
        | a::aa -> (string_of_term' a WholeTermContext appFixity appPrec) ^ " " ^ (string_of_args aa)
    in
    
    match term with
      ApplicationTerm(FirstOrderApplication(f, args, numargs),_,_) ->
        let string_of_head h =
          (match h with
            ConstantTerm(c,_,_,_) ->
              (match (getConstantFixity c) with
                  Prefix -> 
                    if numargs = 1 then
                      (string_of_prefixterm c Prefix args context fix prec,
                      [])
                    else
                      (string_of_prefixterm c Prefix args LeftTermContext appFixity appPrec,
                      List.tl args)
                | Prefixr ->
                    if numargs = 1 then
                      (string_of_prefixterm c Prefix args context fix prec,
                      [])
                    else
                      (string_of_prefixterm c Prefix args LeftTermContext appFixity appPrec,
                      List.tl args)
                | Infix ->
                    if numargs = 2 then
                      (string_of_infixterm c Infix args context fix prec,
                      [])
                    else
                      (string_of_infixterm c Infix args LeftTermContext appFixity appPrec,
                      List.tl (List.tl args))
                | Infixr ->
                    if numargs = 2 then
                      (string_of_infixterm c Infixr args context fix prec,
                      [])
                    else
                      (string_of_infixterm c Infixr args LeftTermContext appFixity appPrec,
                      List.tl (List.tl args))
                | Infixl ->
                    if numargs = 2 then
                      (string_of_infixterm c Infixl args context fix prec,
                      [])
                    else
                      (string_of_infixterm c Infixl args LeftTermContext appFixity appPrec,
                      List.tl (List.tl args))
                | Postfix ->
                    if numargs = 1 then
                      (string_of_postfixterm c Postfix args context fix prec,
                      [])
                    else
                      (string_of_postfixterm c Postfix args LeftTermContext appFixity appPrec,
                      List.tl args)
                | Postfixl ->
                    if numargs = 1 then
                      (string_of_postfixterm c Postfixl args context fix prec,
                      [])
                    else
                      (string_of_postfixterm c Postfixl args LeftTermContext appFixity appPrec,
                      List.tl args)
                | NoFixity ->
                    (string_of_term' term LeftTermContext appFixity appPrec,
                    []))
            | _ ->
              (string_of_term' term LeftTermContext appFixity appPrec,
              []))
          in
        
        let paren = needsParens appFixity appPrec context fix prec in        
        let (head, args') = (string_of_head f) in
        let argstring = (string_of_args args') in
        
        if paren && (List.length args' > 0) then
          "(" ^ head ^ " " ^ argstring ^ ")"
        else if (List.length args' > 0) then
          (head ^ " " ^ argstring)
        else
          head
    | ApplicationTerm(CurriedApplication(l, r),_,_) ->
        let paren = needsParens appFixity appPrec context fix prec in
        let result = (string_of_term' l LeftTermContext appFixity appPrec) ^ " " ^ (string_of_term' r RightTermContext appFixity appPrec) in
        if paren then
          "(" ^ result ^ ")"
        else
          result
    | _ -> Errormsg.impossible (getTermPos term) "string_of_app: term not an application"

  and string_of_abstraction = fun term context fix prec ->
    "error"
  
  and string_of_term' = fun term context fix prec ->
    match term with
      IntTerm(i,_,_) -> (string_of_int i)
    | RealTerm(r,_,_) -> (string_of_float r)
    | StringTerm(StringLiteral(s),_,_) -> "\"" ^ s ^ "\""
    | StringTerm(StringData(s,_,_),_,_) -> "\"" ^ s ^ "\""
    | ConstantTerm(c,_,_,_) -> (getConstantName c)
    | FreeVarTerm(NamedFreeVar(s),_,_) -> Symbol.name (getTypeSymbolSymbol s)
    | BoundVarTerm(NamedBoundVar(s),_,_) -> Symbol.name (getTypeSymbolSymbol s)
    | ApplicationTerm(_) -> string_of_app term context fix prec
    | AbstractionTerm(_) -> string_of_abstraction term context fix prec
    | ErrorTerm -> "error"
    | _ -> Errormsg.impossible (getTermPos term) "string_of_term': unimplemented for this term"
  in
  
  (string_of_term' term WholeTermContext NoFixity 0)


let getModuleConstantTable = function
  Module(_,_,_,ctable,_,_,_,_,_,_,_,_,_,_,_) -> !ctable
| Signature(_) -> Errormsg.impossible Errormsg.none "getModuleConstantTable: not a module"
| ErrorModule -> Errormsg.impossible Errormsg.none "getModuleConstantTable: invalid module"

let getModuleHiddenConstantsRef = function
  Module(_,_,_,_,_,_,_,_,_,_,_,hcs,_,_,_) -> hcs
| Signature(_) -> Errormsg.impossible Errormsg.none "getModuleHiddenConstantsRef: not a module"
| ErrorModule -> Errormsg.impossible Errormsg.none "getModuleHiddenConstantsRef: invalid module"

let getModuleKindTable = function
  Module(_,_,_,_,ktable,_,_,_,_,_,_,_,_,_,_) -> !ktable
| Signature(_) -> Errormsg.impossible Errormsg.none "getModuleKindTable: not a module"
| ErrorModule -> Errormsg.impossible Errormsg.none "getModuleKindTable: invalid module"

let getModuleTypeAbbrevTable = function
  Module(_,_,_,_,_,atable,_,_,_,_,_,_,_,_,_) -> atable
| Signature(_) -> Errormsg.impossible Errormsg.none "getModuleTypeAbbrevTable: not a module"
| ErrorModule -> Errormsg.impossible Errormsg.none "getModuleTypeAbbrevTable: invalid module"

let getModuleClauses = function
  Module(_,_,_,_,_,_,_,_,_,_,_,_,_,_,c) -> !c
| Signature(_) -> Errormsg.impossible Errormsg.none "getModuleClauses: not a module"
| ErrorModule -> Errormsg.impossible Errormsg.none "getModuleClauses: invalid module"


let getModuleClausesRef = function
  Module(_,_,_,_,_,_,_,_,_,_,_,_,_,_,c) -> c
| Signature(_) -> Errormsg.impossible Errormsg.none "getModuleClausesRef: argument is a signature"
| ErrorModule -> Errormsg.impossible Errormsg.none "getModuleClausesRef: argument is invalid"