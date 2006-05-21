(**********************************************************************
*Preabsyn Module:
*	The prebstract syntax for Teyjus.  This is more or less a direct
*	translation from the original C source.  I am sure there are numerous
*	places where it could be made better.
**********************************************************************)
type symbol = Symbol.symbol
type pos = Errormsg.pos

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
type psymbol = Symbol of (symbol * pidkind * pos)

(*	Type Symbols	*)
and ptypesymbol = TypeSymbol of (symbol * ptype option * pidkind * pos)


(*	Types	*)
and ptype =
		Atom of (symbol * pidkind * pos)
	|	App of (ptype * ptype * pos)
	|	Arrow of (ptype * ptype * pos)
	|	TypeAbbrevCall of (symbol * ptype list * pos)
	|	ErrorType

and ptypeabbrev = TypeAbbrev of (symbol * symbol list * ptype * pos)

(*	Terms	*)
and pterm =
		SeqTerm	of (pterm list * pos)
	|	ListTerm of (pterm list * pos)
	|	ConsTerm of (pterm list * pterm * pos)
	|	IdTerm of (symbol * ptype option * pidkind * pos)
	|	RealTerm of (float * pos)
	|	IntTerm of (int * pos)
	|	StringTerm of (string * pos)
	|	OpTerm of (poperation * pos)

(*	Constants	*)
and pconstant = Constant of (psymbol list * ptype option * pos)

(*	Kinds	*)
and pkind = Kind of (psymbol list * int option * pos)

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

(********************************************************************
*Module:
*	This type stores information about a preabsyn module.
*	Its fields are:
*		Name: string
*		Global Constants: pconstant list
*		Local Constants: pconstant list
*		Closed Constants: pconstant list
*
*		Accumulated Modules:
*		Accumulated Signatures:
********************************************************************)
type pmodule =
		Module of (string * pconstant list * pconstant list * pconstant list * pterm list)
	|	Signature of (string * pconstant list)
				
(********************************************************************
*printPreAbsyn:
*	Prints all information about a preabsyn module.
********************************************************************)
let printPreAbsyn = fun m ->
	let newLine = function () -> (print_endline "") in
	let rec
		printConstant = function
				Constant(symlist, Some t, pos) ->
					(List.map printSymbol symlist;
					printType t;
					printPos pos)
			|	Constant(symlist, None, pos) ->
					(List.map printSymbol symlist;
					printPos pos)

		and printSymbol = function
			 Symbol(sym, k, pos) ->
				(print_string (Symbol.name sym);
				printIdKind k;
				printPos pos)
		
		and printPos = function
				p -> (print_string ("Pos(" ^ (string_of_int p) ^ ")"))
					
		and printFixityKind = function
				Infix(pos) ->
					(print_string "Infix(";
					printPos pos;
					print_string ")")
			|	Infixl(pos) ->
					(print_string "Infixl(";
					printPos pos;
					print_string ")")
			|	Infixr(pos) ->
					(print_string "Infixr(";
					printPos pos;
					print_string ")")
			|	Prefix(pos) ->
						(print_string "Prefix(";
						printPos pos;
						print_string ")")
			|	Prefixr(pos) ->
					(print_string "Prefixr(";
					printPos pos;
					print_string ")")
			|	Postfix(pos) ->
					(print_string "Postfix(";
					printPos pos;
					print_string ")")
			|	Postfixl(pos) ->
					(print_string "Postfixl(";
					printPos pos;
					print_string ")")
	
		and printIdKind = function
				CVID -> print_string "CVID"
			|	ConstID -> print_string "ConstID"
			|	AVID -> print_string "AVID"
			|	VarID -> print_string "VarID"
		
		and printKind = function
				Kind(symlist, Some i, pos) ->
					(print_string "Kind(";
					List.map printSymbol symlist;
					print_string ("Arity: " ^ (string_of_int i));
					print_string ")")
			|	Kind(symlist, None, pos) ->
					(print_string "Kind(";
					List.map printSymbol symlist;
					print_string ")")
		and printType = function
				Atom(sym, k, pos) ->
					(print_string (Symbol.name sym);
					printIdKind k;
					printPos pos)
			|	App(t1, t2, pos) ->
					(print_string "App(";
					printType t1;
					print_string ", ";
					printType t2;
					print_string ")";
					printPos pos)
			|	Arrow(t1, t2, pos) ->
					(printType t1;
					print_string "->";
					printType t2;
					printPos pos)

		and printTypeSymbol = function
				TypeSymbol(tsym, Some t, idk, pos) ->
					(print_string "TypeSymbol(";
					print_string (Symbol.name tsym);
					printType t;
					printIdKind idk;
					printPos pos)
			|	TypeSymbol(tsym, None, idk, pos) ->
					(print_string "TypeSymbol(";
					print_string (Symbol.name tsym);
					printIdKind idk;
					printPos pos)

		(*	Print a Preabstract Syntax Term	*)
		and printTerm = function
				SeqTerm(tlist, pos) ->
					(print_string "SeqTerm(";
					List.map printTerm tlist;
					printPos pos;
					print_string ")";
					newLine())
			|	ListTerm(tlist, pos) ->
					(print_string "ListTerm(";
					List.map printTerm tlist;
					printPos pos;
					print_string ")";
					newLine())
			|	ConsTerm(tlist, t, pos) ->
					(print_string "ConsTerm(";
					List.map printTerm tlist;
					printTerm t;
					printPos pos;
					print_string ")";
					newLine())
			|	IdTerm(sym, None, k, pos) ->
					(print_string (Symbol.name sym);
					printIdKind k;
					printPos pos;
					newLine())
			|	IdTerm(sym, Some t, k, pos) ->
					(print_string (Symbol.name sym);
					printType t;
					printIdKind k;
					printPos pos;
					newLine())
			|	RealTerm(r, pos) ->
					((print_string (string_of_float r));
					printPos pos;
					newLine())
			|	IntTerm(i, pos) ->
					((print_string (string_of_int i));
					printPos pos;
					newLine())
			|	StringTerm(s, pos) ->
					(print_string s;
					printPos pos;
					newLine())
			|	OpTerm(o, pos) ->
					(printOperation o;
					printPos pos;
					newLine())
		
		and printOperation = function
				COMMA -> print_string ","
			|	PLUS -> print_string "+"
			|	MINUS -> print_string "-"
			|	TIMES -> print_string "*"
			|	UMINUS -> print_string "-"
			|	GE -> print_string ">="
			|	GT -> print_string ">"
			|	LE -> print_string "<="
			|	LT -> print_string "<"

		and printPreAbsyn' = function
				Module(name, gconstants, lconstants, cconstants, tlist) ->
					(print_endline ("Module: " ^ name);
					List.map printConstant gconstants;
					List.map printConstant lconstants;
					List.map printConstant cconstants;
					List.map printTerm tlist;
					())
			|	Signature(name, gconstants) ->
					(print_endline ("Signature: " ^ name);
					List.map printConstant gconstants;
					())
	in
		printPreAbsyn' m

let fixityGetPos = function
		Infix(i) -> i
	|	Infixl(i) -> i
	|	Infixr(i) -> i
	|	Prefix(i) -> i
	|	Prefixr(i) -> i
	|	Postfix(i) -> i
	|	Postfixl(i) -> i
