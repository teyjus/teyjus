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
type akindinfo = (symbol * int option * int option ref * pos) 

and akind = 
    LocalKind of akindinfo
  | GlobalKind of akindinfo
  | PervasiveKind of akindinfo

(*****************************************************************************
*Type Variable Data:
* (firstuse, lastuse, perm, safety, heapvar, offset, firstgoal, lastgoal)
*****************************************************************************)
and atypevar = 
  TypeVar of (atype option ref * atype option ref * bool option ref * 
    bool option ref * bool option ref * int option ref * 
    int option ref * int option ref)

(*****************************************************************************
*Type:
*****************************************************************************)
and atype = 
    SkeletonVarType of (int ref)
  | TypeVarType of (atypevar option ref * bool option ref)
  |	ArrowType of (atype * atype)
  | AppType of (akind * atype list)
  | TypeSetType of (atype * atype list ref)
  | TypeRefType of (atype)
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
    bool ref * bool ref * bool ref * bool ref * askeleton * int ref *
    bool array option ref * atype list * acodeinfo option ref *
    aconstanttype * int ref * pos)

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
    Builtin
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
  Var of (bool option ref * bool option ref * bool option ref *
    bool option ref * int option ref * int option ref * int option ref *
    aterm option ref)

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
    IntTerm of (int * bool * pos option)
  | RealTerm of (float * bool * pos option)
  | StringTerm of (astringinfo * bool * pos option)
  | ConstantTerm of (aconstant * atype list * bool * pos option)
  | FreeVarTerm of (afreevarinfo * bool * pos option)
  | BoundVarTerm of (aboundvarinfo * bool * pos option)
  | AbstractionTerm of (aabstractioninfo * bool * pos option)
  | ApplicationTerm of (aapplicationinfo * bool * pos option)
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

and adefinitions = ((aconstant * aclausesblock) list)
and avarinits = (avar list)
and ahcvarassoc = ((avar * aconstant) list)

(****************************************************************************
 *Clauses:
 * (head, args, tyargs, numargs, numtargs, body, offset, varmap, tyvarmap   *
 *gesplist, cutvar, hasenv, impmods)                                      *
 ***************************************************************************)

and aclause = 
    Fact of (aconstant * aterm list * atype list * int * int * 
			   atermvarmap * atypevarmap * int option ref * aimportedmodule list)
  | Rule of (aconstant * aterm list * atype list * int * int * atermvarmap *
			   atypevarmap * agoal * agoalenvassoc ref * avar option ref *
			   bool ref * aimportedmodule list)


(* Goal number and environment size association list*)
and agoalenvassoc = ((int * int) list)

(* term variable map list *)
and atermvarmap  = ((avar * avar) list)

(* type variable map list *)
and atypevarmap  = ((atypevar * atypevar) list)

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
      aconstant Table.SymbolTable.t * akind Table.SymbolTable.t *
      atypeabbrev Table.SymbolTable.t * astringinfo list * akind list *
      akind list * aconstant list * aconstant list * aconstant list *
      askeleton list * askeleton list * aclauseinfo)
  | Signature of (string * akind list * aconstant list)

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

let getKindName = function
  LocalKind(n,_,_,_) -> (Symbol.name n)
| GlobalKind(n,_,_,_) -> (Symbol.name n)
| PervasiveKind(n,_,_,_) -> (Symbol.name n)

(**********************************************************************
* Skeleton Accessors
**********************************************************************)
let getSkeletonType = function
  Skeleton(f,_,_) -> f

let getSkeletonSize = function
  Skeleton(_,i,_) ->
    match !i with
      Some i' -> i'
    | None -> (Errormsg.impossible Errormsg.none
                "Absyn.getSkeletonSize: Skeleton has no size.")

(**********************************************************************
* Constant Accessors
**********************************************************************)
let getConstantPos = function
  Constant(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,p) ->
    p

let getConstantSymbol = function
  Constant(sym,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> sym

let getConstantName = fun c ->
  (Symbol.name (getConstantSymbol c))

let getConstantSkeleton = function
  Constant(_,_,_,_,_,_,_,_,skel,_,_,_,_,_,_,_,_) -> skel

let getConstantType = function
  Constant(_,_,_,_,_,_,_,_,_,_,_,_,_,_,ctype,_,_) ->
    ctype

let getConstantFixity = function
  Constant(_,fix,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
    !fix

let getConstantPrec = function
  Constant(_,_,prec,_,_,_,_,_,_,_,_,_,_,_,_,_,_) ->
    !prec

(**********************************************************************
*string_of_fixity:
* Convert an absyn fixity to a string.  Used only in printAbsyn.
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
  
  let rec printTypeVar = function TypeVar(firstuse,_,_,_,_,_,_,_) -> ()

  (*  Print an absyn type *)
  and printType = function
    TypeVarType(v, b) ->
      (match !v with
        Some v' ->
          (output "TypeVar(";
          printTypeVar v';
          output ", ";
          output (Option.string_of_option !b string_of_bool);
          output ")")
      | None ->
          (output "TypeVar(";
          output "None";
          output ", ";
          output (Option.string_of_option !b string_of_bool);
          output ")"))
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
  | AppType(f,t) ->
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
  | TypeRefType(t) ->
      (output "Ref(";
      printType t;
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
      Skeleton(t,i,b) -> 
        (output "Skeleton(";
        printType t;
        output (", " ^ (Option.string_of_option !i string_of_int));
        output (", " ^ (string_of_bool !b));
        output ")")
    in
    
    match const with
      Constant(sym,fix,prec,exportdef,useonly,nodefs,closed,typepreserv,
        reducible,skel,envsize,_,env,codeinfo,ctype,index,pos) ->
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
        printSkeleton skel;
        output ", ";
        printConstantType ctype;
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
      printTable printConstant ctable;
      output_line "";
      
      output_line "KindTable:";
      printTable printkind' ktable;
      output_line ")")
      
  | Signature(name, consts, kinds) ->
      (output "Signature(";
      output name;
      output_line ")")

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
  | AppType(kind, tlist) -> "AppType(" ^ (string_of_kind kind) ^ ", " ^ (print' tlist) ^ ")"
  | SkeletonVarType(i) -> "SkeletonVarType(" ^ (string_of_int !i) ^ ")"
  | TypeSetType(d, tlist) -> "TypeSetType(" ^ (string_of_type d) ^ ", " ^ (print' (!tlist)) ^ ")"
  | TypeRefType(t) -> (string_of_type t)
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
  | t -> Errormsg.impossible Errormsg.none ("Absyn.getArrowTypeArguments: invalid type: " ^ (string_of_type t))

let getTypeSetSet = function
  TypeSetType(_, set) -> set
| _ -> (Errormsg.impossible Errormsg.none "Absyn.getTypeSetType: invalid type")

let getTypeSetDefault = function
  TypeSetType(def, _) -> def
| _ -> (Errormsg.impossible Errormsg.none "Absyn.getTypeSetType: invalid type")

let makeTypeVariable () = TypeVarType(ref None, ref None)
let getTypeVariableReference = function
  TypeVarType(t, _) -> t
| _ -> (Errormsg.impossible Errormsg.none "Absyn.getTypeVariableReference: invalid type")

let getTypeArguments = function
  AppType(_, args) -> args
| t -> (Errormsg.impossible Errormsg.none ("Absyn.getTypeArguments: invalid type: " ^ (string_of_type t)))

let isArrowType = function
  ArrowType(_) -> true
| _ -> false

let isTypeSetType = function
  TypeSetType(_) -> true
| _ -> false

let isConstantType = function
  AppType(_, args) -> (List.length args) = 0
| _ -> false

let isVariableType = function
  TypeVarType(r,_) ->
    (match !r with
      Some(t) -> (Errormsg.impossible Errormsg.none "Absyn.isVariableType: bound variable.")
    | None -> true)
| _ -> false

let isSkeletonVariableType = function
  SkeletonVarType(_) -> true
| _ -> false

let getSkeletonVariableIndex = function
  SkeletonVarType(i) -> i
| _ -> (Errormsg.impossible Errormsg.none "Absyn.getSkeletonVariableIndex: invalid type.")

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
    TypeRefType(t) -> dereferenceType t
  | TypeVarType(r, _) ->
      (match !r with
        Some(t) -> dereferenceType t
      | None -> ty)
  | _ -> ty

let maxPrec = 255
let appFixity = Infixl
let appPrec = maxPrec + 2

(**********************************************************************
*Various term accessors and constructors.
**********************************************************************)
let getTermPos = function
  IntTerm(_,p) -> p
| StringTerm(_,p) -> p
| RealTerm(_,p) -> p
| AbstractionTerm(_,_,p) -> p
| ConstantTerm(_,_,p) -> p
| FreeVarTerm(_,p) -> p
| BoundVarTerm(_,p) -> p
| ApplyTerm(_,_,p) -> p
| ErrorTerm -> Errormsg.none
| _ -> Errormsg.impossible Errormsg.none "Absyn.getTermPos: invalid term"

let getTermAbstractionVar = function
  AbstractionTerm(avar,_,_) -> avar
| _ -> Errormsg.impossible Errormsg.none "Absyn.getTermAbstractionVar: invalid term"

let errorTerm = ErrorTerm
let errorFixedTerm = FixedErrorTerm

let makeFreeVarTerm = fun tsym pos ->
  FreeVarTerm(tsym, pos)

let makeBoundVarTerm = fun tsym pos ->
  BoundVarTerm(tsym, pos)

let getTypeSymbolType = function
    ImplicitTypeSymbol(_,_,_,t) -> t
  | AnonymousTypeSymbol(_,_,_,t) -> t
  | BoundTypeSymbol(_,_,_,t) -> t

let getTypeSymbolRawType = fun s ->
  let get' = function
      RawType(t) -> t
    | _ -> Errormsg.impossible Errormsg.none "Absyn.getTypeSymbolRawType: type symbol has no raw type"
  in
  
  match s with
    ImplicitTypeSymbol(_,_,_,t) -> get' t
  | AnonymousTypeSymbol(_,_,_,t) -> get' t
  | BoundTypeSymbol(_,_,_,t) -> get' t

let getTypeSymbolSymbol = function
    ImplicitTypeSymbol(_,_,s,_) -> s
  | AnonymousTypeSymbol(_,_,s,_) -> s
  | BoundTypeSymbol(_,_,s,_) -> s

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
      | _ -> (Errormsg.impossible Errormsg.none "Absyn.needsParens: invalid fixity."))

  | RightTermContext ->
      (match fix with
        Infix -> check1 ()
      | Infixl -> check1 ()
      | Prefix -> check1 ()
      
      | Infixr -> checkRight ()
      | Prefixr -> checkRight ()
      | _ -> (Errormsg.impossible Errormsg.none "Absyn.needsParens: invalid fixity."))
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
  
  and string_of_infix = fun op opfix args context fix prec ->
    let opprec = getConstantPrec op in
    let paren = needsParens opfix opprec context fix prec in
    let result = (string_of_term' (List.hd args) LeftTermContext opfix opprec) ^ 
      " " ^ (getConstantName op) ^ " " ^
      (string_of_term' (List.hd (List.tl args)) RightTermContext opfix opprec) in
    
    if paren then
      "(" ^ result ^ ")"
    else
      result
  
  and string_of_postfix = fun op opfix args context fix prec ->
    let opprec = getConstantPrec op in
    let paren = needsParens opfix opprec context fix prec in
    let result = (string_of_term' (List.hd args) LeftTermContext opfix opprec) ^ " " ^ (getConstantName op) in
    
    if paren then
      "(" ^ result ^ ")"
    else
      result
  
  and string_of_app = fun term context fix prec ->
    match term with
      ApplyTerm(l,r,_) ->
        let paren = needsParens appFixity appPrec context fix prec in
        let result = (string_of_term' l LeftTermContext appFixity appPrec) ^ " " ^ (string_of_term' r RightTermContext appFixity appPrec) in
        if paren then
          "(" ^ result ^ ")"
        else
          result
    | _ -> Errormsg.impossible (getTermPos term) "Absyn.string_of_app: term not an application."

  and string_of_abstraction = fun term context fix prec ->
    "error"
  
  and string_of_term' = fun term context fix prec ->
    match term with
      IntTerm(i,_) -> (string_of_int i)
    | RealTerm(r,_) -> (string_of_float r)
    | StringTerm(s,_) -> "\"" ^ s ^ "\""
    | ConstantTerm(c,_,_) -> (getConstantName c)
    | FreeVarTerm(s,_) -> Symbol.name (getTypeSymbolSymbol s)
    | BoundVarTerm(s,_) -> Symbol.name (getTypeSymbolSymbol s)
    | ApplyTerm(_) -> string_of_app term context fix prec
    | AbstractionTerm(_) -> string_of_abstraction term context fix prec
    | ErrorTerm -> "error"
    | _ -> Errormsg.impossible (getTermPos term) "string_of_term': unimplemented for this term."
  in
  
  (string_of_term' term WholeTermContext NoFixity 0)


let getModuleConstantTable = function
  Module(_,ctable,_,_,_,_,_,_,_,_) -> ctable
| Signature(_) -> Errormsg.impossible Errormsg.none "Absyn.getModuleConstantTable: not a module."

let getModuleKindTable = function
  Module(_,_,ktable,_,_,_,_,_,_,_) -> ktable
| Signature(_) -> Errormsg.impossible Errormsg.none "Absyn.getModuleKindTable: not a module."

let getModuleTypeAbbrevTable = function
  Module(_,_,_,atable,_,_,_,_,_,_) -> atable
| Signature(_) -> Errormsg.impossible Errormsg.none "Absyn.getModuleTypeAbbrevTable: not a module."

let getModuleClauses = function
  Module(_,_,_,_,_,_,_,_,_,c) -> c
| Signature(_) -> Errormsg.impossible Errormsg.none "Absyn.getModuleClauses: not a module."
