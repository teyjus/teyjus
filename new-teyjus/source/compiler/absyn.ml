(**********************************************************************
*Absyn:
* The abstract syntax representation.
**********************************************************************)
type pos = Errormsg.pos
type symbol = Symbol.symbol

(********************************************************************
*atypevardata
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
*Constants:
* Code info might not be necessary.
*
* Constants are represented by the following data:
*   Name
*   Fixity
*   Precedence
*   Export Def?
*   Use Only?
*   No Defs?
*   Closed?
*   Type Preserving?
*   Type Skeleton
*   Type Environment
*   Code Info
*   Constant Type
********************************************************************)

and acodeinfo =
    BuiltinIndex of int
  | Clauses of aclause list
  
and aconstant =
    Constant of (symbol * afixity * int * bool * bool * bool * bool * bool * askeleton list * atype list * acodeinfo * aconstanttype * pos)

and aconstanttype = 
    GlobalConstant
  | LocalConstant
  | PervasiveConstant of bool (*  Can this constant be redefined? *)
  | NewConstant
  | HiddenConstant
  | AnonymousConstant

(*  Type Symbols  *)
and atypesymboltype =
    RawType of atype
  | SkeletonType of (askeleton list * atype list * int)

and atypesymbol =
    ImplicitTypeSymbol of (bool * aconstant * symbol * atypesymboltype)
  | AnonymousTypeSymbol of (bool * aconstant * symbol * atypesymboltype)
  | BoundTypeSymbol of (bool * aconstant * symbol * atypesymboltype)

(********************************************************************
*Variables:
* Representation of explicitly or implicitly quantified variables in 
* clauses; this is used during code generation for clauses.
********************************************************************)
and avar = Var of (bool * bool * bool * bool * int * askeleton list * int * int * int * aterm)

(********************************************************************
*Terms:
********************************************************************)
and aterm =
    IntTerm of (int * pos)
  | StringTerm of (string * pos)
  | RealTerm of (float * pos)
  | AbstractionTerm of (atypesymbol list * aterm * pos)
  | SansAbstractionTerm of (atypesymbol list * aterm)
  
  | ConstTerm of (aconstant * atype list * pos)
  | SansConstTerm of (aconstant * atype list)
  
  | FreeVarTerm of (atypesymbol * pos)
  | SansFreeVarTerm of (atypesymbol)
  
  | BoundVarTerm of (atypesymbol * pos)
  | SansBoundVarTerm of (atypesymbol)
  
  | ApplyTerm of (aterm * aterm * int * pos)
  | SansApplyTerm of (aterm * aterm * int)
  
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

and adefinition = (aconstant * aclause list)

(********************************************************************
*
********************************************************************)
and avarinit = (avar * atype list)

(**********************************************************************
*Goals:
**********************************************************************)
and agoal =
    AtomicGoal of (aconstant * int * int * aterm list * atype list)
  | ImplicationGoal of (adefinition list * avarinit list * agoal)
  | AndGoal of (agoal * agoal)
  | AllGoal of (ahcvarpair list * agoal)
  | SomeGoal of (avar * atype list * agoal)

(**********************************************************************
*
**********************************************************************)
and atermvarmap =  TermVarMap of (avar * avar) list

(**********************************************************************
*Clauses:
**********************************************************************)
and aclause =
    Clause of (aconstant * aterm list * atype list * int * int *
      agoal * int * agoal list * atermvarmap * atermvarmap * bool)

(**********************************************************************
*String:
**********************************************************************)
and astring = (string * int * bool)

(**********************************************************************
*Module:
**********************************************************************)
and amodule =
    Module of (string * aconstant Table.SymbolTable.t *
      akind Table.SymbolTable.t * atypeabbrev Table.SymbolTable.t * 
      astring list * aconstant list * 
      aconstant list * aconstant list * akind list * akind list *
      askeleton list * askeleton list * aclause list)
|   Signature

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
*getConstantPos:
* Get a constant's position information.
**********************************************************************)
let getConstantPos = function
  Constant(_,_,_,_,_,_,_,_,_,_,_,_,p) ->
    p

(**********************************************************************
*getConstantSymbol:
* Get a constant's symbol.
**********************************************************************)
let getConstantSymbol = function
  Constant(sym, _ , _ , _ , _ , _ , _ , _ , _ , _ , _ , _ , _) -> sym

let getConstantName = fun c ->
  (Symbol.name (getConstantSymbol c))

(**********************************************************************
*getConstantType:
* Get a constant's type (local, global, closed, etc.)
**********************************************************************)
let getConstantType = function
  Constant(_,_,_,_,_,_,_,_,_,_,_,ctype,_) ->
    ctype

(**********************************************************************
*getConstantFixity:
* Get a constant's fixity.
**********************************************************************)
let getConstantFixity = function
  Constant(_,fix,_,_,_,_,_,_,_,_,_,_,_) ->
    fix

(**********************************************************************
*getConstantPrec:
* Get a constant's precedence.
**********************************************************************)
let getConstantPrec = function
  Constant(_,_,prec,_,_,_,_,_,_,_,_,_,_) ->
    prec

(*  Print a fixity.  Used only in printConstant.  *)
let string_of_fixity = function
  Infix -> "Infix"
| Infixl -> "Infixl"
| Infixr -> "Infixr"
| Prefix -> "Prefix"
| Prefixr -> "Prefixr"
| Postfix -> "Postfix"
| Postfixl -> "Postfixl"
| NoFixity -> "No Fixity"

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
  
  (*  Print an absyn type *)
  let rec printType = function
    TypeVarType(v, b) ->
      (match !v with
        Some v ->
          (output "TypeVar(";
          output "#ERROR#";
          output ", ";
          output (string_of_bool b);
          output ")")
      | None ->
          (output "TypeVar(";
          output "None";
          output ", ";
          output (string_of_bool b);
          output ")"))
  | ArrowType(l, r) -> 
      (output "Arrow(";
      printType l;
      output ", ";
      printType r;
      output ")")
  | AppType(f,t) ->
      let rec print' = function
      | t::[] -> (printType t)
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
      output (string_of_int i);
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
      GlobalConstant -> "Global"
    | LocalConstant -> "Local"
    | PervasiveConstant(b) -> 
        if b then
          "Mutable Pervasive"
        else
          "Immutable Pervasive"
    in
    
    let rec printSkeleton = function
      Skeleton(t,i,b)::[] -> 
        (output "Skeleton(";
        printType t;
        output (", " ^ (string_of_int i));
        output (", " ^ (string_of_bool b));
        output ")")
    | Skeleton(t, i, b)::skels ->
        (output "Skeleton(";
        printType t;
        output (", " ^ (string_of_int i));
        output (", " ^ (string_of_bool b));
        output "), ";
        printSkeleton skels)
    | [] -> (output "None")
    in
    match const with
      Constant(sym,fix,prec,exportdef,useonly,nodefs,closed,typepreserv,skel,env,codeinfo,ctype, pos) ->
        (output "Constant(";
        output (Symbol.name sym);
        output ", ";
        output (string_of_fixity fix);
        output ", ";
        output (string_of_int prec);
        output ", ";
        output (string_of_bool exportdef);
        output ", ";
        output (string_of_bool useonly);
        output ", ";
        output (string_of_bool nodefs);
        output ", ";
        output (string_of_bool closed);
        output ", ";
        output (string_of_bool typepreserv);
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
    Module(name, ctable, ktable, tabbrevtable, strings,
      gconsts, lconsts, cconsts,
      gkinds, lkinds,
      skels,hskels,clause) ->
      
      (output "Module(";
      output name;
      output_line ", ";
      
      output_line "ConstantTable:";
      printTable printConstant ctable;
      output_line "";
      
      output "KindTable:";
      printTable printkind' ktable;
      output_line ")")
      
  | Signature ->
      (output "Signature(";
      output_line ")")

(**********************************************************************
*Various type accessors and values.
**********************************************************************)
let errorType = ErrorType

let getTypeTarget = function
  ArrowType(l, r) -> r
| t -> t

let getTypeVariableReference = function
  TypeVarType(t, b) -> t
| _ -> (Errormsg.impossible Errormsg.none "Absyn.getTypeVariableReference: invalid type")

let getTypeArguments = function
  AppType(k, args) -> args
| _ -> (Errormsg.impossible Errormsg.none "Absyn.getTypeArguments: invalid type")

let isArrowType = function
  ArrowType(_) -> true
| _ -> false

let isVariableType = function
  TypeVarType(_) -> true
| _ -> false

let rec makeArrowType = function
  arg::args ->
    (match args with
      [] -> (Errormsg.impossible Errormsg.none "Absyn.makeArrowType: invalid number of types.")
    | [arg'] -> ArrowType(arg, arg')
    | arg'::args' -> ArrowType(arg, makeArrowType(args)))
| [] -> (Errormsg.impossible Errormsg.none "Absyn.makeArrowType: invalid number of types.")

(**********************************************************************
*dereferenceType:
* Dereference a type.
**********************************************************************)
let rec dereferenceType = function
  TypeRefType(t) -> dereferenceType t
| t -> t

let maxPrec = 255
let appFixity = Infixl
let appPrec = maxPrec + 2

(**********************************************************************
*Various term accessors.
**********************************************************************)
let getTermPos = function
  IntTerm(_,p) -> p
| StringTerm(_,p) -> p
| RealTerm(_,p) -> p
| AbstractionTerm(_,_,p) -> p
| ConstTerm(_,_,p) -> p
| FreeVarTerm(_,p) -> p
| BoundVarTerm(_,p) -> p
| ApplyTerm(_,_,_,p) -> p
| _ -> (Errormsg.impossible Errormsg.none "Absyn.getTermPos: invalid term.")

let errorTerm = ErrorTerm

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
*string_of_term:
* Converts an absyn term to a string representation.
**********************************************************************)
let string_of_term = fun term ->
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
        | _ -> (Errormsg.impossible Errormsg.none "Absyn.string_of_term: invalid fixity."))

    | RightTermContext ->
        (match fix with
          Infix -> check1 ()
        | Infixl -> check1 ()
        | Prefix -> check1 ()
        
        | Infixr -> checkRight ()
        | Prefixr -> checkRight ()
        | _ -> (Errormsg.impossible Errormsg.none "Absyn.string_of_term: invalid fixity."))
    | WholeTermContext -> (Errormsg.impossible Errormsg.none "Absyn.string_of_term: invalid term context.")

  
  and string_of_prefixterm = fun op opfix args context fix prec ->
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
    let paren = needsParens appFixity appPrec context fix prec in
    "error"
  
  and string_of_abstraction = fun term context fix prec ->
    "error"
  
  and string_of_term' = fun term context fix prec ->
    "error"
  in
  
  (string_of_term' term WholeTermContext NoFixity 0)

