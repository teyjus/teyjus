(**********************************************************************
*Absyn:
*	The abstract syntax representation.
**********************************************************************)
type pos = Errormsg.pos
type symbol = Symbol.symbol
	
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
		LocalKind of (symbol * int option * akindmap * pos)
	|	GlobalKind of (symbol * int option * akindmap * pos)
	|	PervasiveKind of (symbol * int option * akindmap * pos)

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
	|	TypeVarType of (atypevar option * bool)
	| AppType of (akind * atype list)
	|	SkeletonVarType of (int)
	|	TypeRefType of (atype)
	|	ErrorType

and afixity =
		Infix
	|	Infixl
	|	Infixr
	|	Prefix
	|	Prefixr
	|	Postfix
	|	Postfixl
	|	NoFixity

(********************************************************************
*Constants:
*	Code info might not be necessary.
*
*	Constants are represented by the following data:
*		Name
*		Fixity
*		Precedence
*		Export Def?
*		Use Only?
*		No Defs?
*		Closed?
*		Type Preserving?
*		Type Skeleton
*		Type Environment
*		Code Info
*		Constant Type
********************************************************************)

and acodeinfo =
		BuiltinIndex of int
	|	Clauses of aclause list
	
and aconstant =
		Constant of (symbol * afixity * int * bool * bool * bool * bool * bool * askeleton list * atype list * acodeinfo * aconstanttype * pos)

and aconstanttype = 
		GlobalConstant
	|	LocalConstant
	|	PervasiveConstant
	|	NewConstant
	| HiddenConstant
	|	AnonymousConstant

(*	Type Symbols	*)
and atypesymboltype =
		RawType of atype
	|	SkeletonType of (askeleton list * atype list * int)

and atypesymbol =
		ImplicitTypeSymbol of (bool * aconstant * symbol * atypesymboltype)
	|	AnonymousTypeSymbol of (bool * aconstant * symbol * atypesymboltype)
	|	BoundTypeSymbol of (bool * aconstant * symbol * atypesymboltype)

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

(**********************************************************************
*Goals:
**********************************************************************)
and agoal =
		AtomicGoal of (aconstant * int * int * aterm list * atype list)
	|	ImplicationGoal of (adefinition list * avarinit list * agoal)
	|	AndGoal of (agoal * agoal)
	|	AllGoal of (ahcvarpair list * agoal)
	|	SomeGoal of (avar * atype list * agoal)

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
|		Signature

let getKindPos = function
	LocalKind(_,_,_,p) -> p
|	GlobalKind(_,_,_,p) -> p
|	PervasiveKind(_,_,_,p) -> p

(**********************************************************************
*getKindArity:
**********************************************************************)
let getKindArity = function
	LocalKind(_,Some a,_,_) -> a
|	GlobalKind(_,Some a,_,_) -> a
|	PervasiveKind(_,Some a,_,_) -> a
|	k -> (Errormsg.impossible (getKindPos k)  "getKindArity(): invalid kind arity")

(*	Print a fixity.  Used only in printConstant.	*)
let string_of_fixity = function
	Infix -> "Infix"
|	Infixl -> "Infixl"
|	Infixr -> "Infixr"
|	Prefix -> "Prefix"
|	Prefixr -> "Prefixr"
|	Postfix -> "Postfix"
|	Postfixl -> "Postfixl"
|	NoFixity -> "No Fixity"

(**********************************************************************
*printAbsyn:
**********************************************************************)
let printAbsyn = fun m out ->
	(*	Text output functions	*)
	let output = function s -> (output_string out s) in
	let output_line = function s -> (output_string out (s ^ "\n")) in
	
	let printPos = fun p ->
		(output "Pos(";
		output (Errormsg.string_of_pos p);
		output ")")
	in
	
	(*	Print an absyn type	*)
	let rec printType = function
		TypeVarType(Some v, b) ->
			(output "TypeVar(";
			output "#ERROR#";
			output ", ";
			output (string_of_bool b);
			output ")")
	|	TypeVarType(None, b) ->
			(output "TypeVar(";
			output "None";
			output ", ";
			output (string_of_bool b);
			output ")")
	|	ArrowType(l, r) -> 
			(output "Arrow(";
			printType l;
			output ", ";
			printType r;
			output ")")
	|	AppType(f,t) ->
			let rec print' = function
				t::ts -> (printType t; output ", "; print' ts)
			|	t::[] -> (printType t)
			| [] -> (output "None")
			in
			
			(output "App(";
			printKind f;
			output ", ";
			print' t;
			output ")")
	|	SkeletonVarType(i) ->
			(output "SkeletonVar(";
			output (string_of_int i);
			output ")")
	|	TypeRefType(t) ->
			(output "Ref(";
			printType t;
			output ")")
	|	ErrorType ->
			(output "Error")

	(*	Print a constant.  For use with printTable.	*)
	and printConstant = fun sym const ->
		let printConstantType = function
			GlobalConstant -> "Global"
		|	LocalConstant -> "Local"
		in
		
		let rec printSkeleton = function
			Skeleton(t, i, b)::skels ->
				(output "Skeleton(";
				printType t;
				output (", " ^ (string_of_int i));
				output (", " ^ (string_of_bool b));
				output "), ";
				printSkeleton skels)
		|	Skeleton(t,i,b)::[] -> 
				(output "Skeleton(";
				printType t;
				output (", " ^ (string_of_int i));
				output (", " ^ (string_of_bool b));
				output ")")
		|	[] -> (output "None")
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
	
	(*	Print a kind.  For use with printTable.	*)
	and printKind = fun k ->
		let print' = fun sym a kmap ->
			match a with
				Some a' ->
					(output (Symbol.name sym);
					output ", ";
					output (string_of_int a'))
			|	None ->
					(output (Symbol.name sym);
					output ", ";
					output "None")
		in
		
		match k with
			LocalKind(sym,arity,kmap,p) ->
				(output "LocalKind(";
				print' sym arity kmap;
				output ")")
		|	GlobalKind(sym,arity,kmap,p) ->
				(output "GlobalKind(";
				print' sym arity kmap;
				output ")")
		|	PervasiveKind(sym,arity,kmap,p) ->
				(output "PervasiveKind(";
				print' sym arity kmap;
				output ")")
	in
	
	(*	Print the contents of a table	*)
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
			
	|	Signature ->
			(output "Signature(";
			output_line ")")
