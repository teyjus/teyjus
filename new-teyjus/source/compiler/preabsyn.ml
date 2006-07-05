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
	|	ErrorType

and ptypeabbrev = TypeAbbrev of (psymbol * psymbol list * ptype * pos)

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
	|	ErrorTerm
	
and pclause = Clause of (pterm list)

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
*	See interface for details.
********************************************************************)
type pmodule =
		Module of (string * pconstant list * pconstant list * 
			pconstant list * pfixity list * pkind list * pkind list * ptypeabbrev list *
			pclause list * psymbol list * psymbol list * psymbol list)
	|	Signature of (string * pconstant list * pkind list *
			ptypeabbrev list * pfixity list * psymbol list)
				
(********************************************************************
*printPreAbsyn:
*	Prints all information about a preabsyn module.
********************************************************************)
let printPreAbsyn = fun m out ->
	(*	Text output functions	*)
	let output = function s -> (output_string out s) in
	let output_line = function s -> (output_string out (s ^ "\n")) in
	
	let rec printWithCommas : ('a -> unit) -> 'a list -> unit = 
		fun f ls ->
			match ls with
				(l::[]) -> (f l)
			|	(l::ls) ->
					((f l);
					output ", ";
					printWithCommas f ls)
			|	([]) -> ()

	(*	Print constant to output channel	*)
	and printConstant = function
			Constant(symlist, Some t, pos) ->
				(output "Constant(";
				printWithCommas printSymbol symlist;
				output ", ";
				printType t;
				output ", ";
				printPos pos;
				output_line ")";)
		|	Constant(symlist, None, pos) ->
				(List.map printSymbol symlist;
				printPos pos)

	and printSymbol = function
			Symbol(sym, k, pos) ->
				(output "Symbol(";
				output (Symbol.name sym);
				output ", ";
				printIdKind k;
				output ", ";
				printPos pos;
				output ")")

		and printPos = function
			p ->
				(output "Pos(";
				output (Errormsg.string_of_pos p);
				output ")")
		
		and printTypeAbbrev = function
			TypeAbbrev(name, arglist, ty, pos) ->
				(output "TypeAbbrev(";
				printSymbol name;
				printWithCommas printSymbol arglist;
				output ", ";
				printType ty;
				output ", ";
				printPos pos;
				output_line ")";)

		and printFixity = function
				Fixity(names, k, prec, pos) ->
					(output "Fixity(";
					printWithCommas printSymbol names;
					output ", ";
					printFixityKind k;
					output ", ";
					output (string_of_int prec);
					output ", ";
					printPos pos;
					output_line ")")
					
		and printFixityKind = function
				Infix(pos) ->
					(output "Infix(";
					printPos pos;
					output ")")
			|	Infixl(pos) ->
					(output "Infixl(";
					printPos pos;
					output ")")
			|	Infixr(pos) ->
					(output "Infixr(";
					printPos pos;
					output ")")
			|	Prefix(pos) ->
						(output "Prefix(";
						printPos pos;
						output ")")
			|	Prefixr(pos) ->
					(output "Prefixr(";
					printPos pos;
					output ")")
			|	Postfix(pos) ->
					(output "Postfix(";
					printPos pos;
					output ")")
			|	Postfixl(pos) ->
					(output "Postfixl(";
					printPos pos;
					output ")")
	
		and printIdKind = function
				CVID -> output "CVID"
			|	ConstID -> output "ConstID"
			|	AVID -> output "AVID"
			|	VarID -> output "VarID"
		
		and printKind = function
				Kind(symlist, Some i, pos) ->
					(output "Kind(";
					printWithCommas printSymbol symlist;
					output ",";
					output (string_of_int i);
					output_line ")")
			|	Kind(symlist, None, pos) ->
					(output "Kind(";
					printWithCommas printSymbol symlist;
					output_line ")")

		and printType = function
				Atom(sym, k, pos) ->
					(output "Atom(";
					output (Symbol.name sym);
					output ", ";
					printIdKind k;
					output ", ";
					printPos pos;
					output ")")
			|	App(t1, t2, pos) ->
					(output "App(";
					printType t1;
					output ", ";
					printType t2;
					output ")";
					printPos pos)
			|	Arrow(t1, t2, pos) ->
					(output "Arrow(";
					printType t1;
					output ", ";
					printType t2;
					output ", ";
					printPos pos;
					output ")")
			|	ErrorType ->
					(output "Error")

		and printTypeSymbol = function
				TypeSymbol(tsym, Some t, idk, pos) ->
					(output "TypeSymbol(";
					output (Symbol.name tsym);
					printType t;
					printIdKind idk;
					printPos pos)
			|	TypeSymbol(tsym, None, idk, pos) ->
					(output "TypeSymbol(";
					output (Symbol.name tsym);
					printIdKind idk;
					printPos pos)

		(*	Print a Preabstract Syntax Term	*)
		and printTerm = function
				SeqTerm(tlist, pos) ->
					(output "SeqTerm(";
					List.iter printTerm tlist;
					printPos pos;
					output_line ")")
			|	ListTerm(tlist, pos) ->
					(output "ListTerm(";
					List.map printTerm tlist;
					printPos pos;
					output_line ")")
			|	ConsTerm(tlist, t, pos) ->
					(output "ConsTerm(";
					List.map printTerm tlist;
					printTerm t;
					printPos pos;
					output_line ")")
			|	IdTerm(sym, None, k, pos) ->
					(output "IdTerm(";
					output (Symbol.name sym);
					output ", ";
					printIdKind k;
					output ", ";
					printPos pos;
					output_line ")")
			|	IdTerm(sym, Some t, k, pos) ->
					(output "IdTerm(";
					output (Symbol.name sym);
					printType t;
					output ", ";
					printIdKind k;
					output ", ";
					printPos pos;
					output_line ")")
			|	RealTerm(r, pos) ->
					(output "RealTerm(";
					output (string_of_float r);
					output ", ";
					printPos pos;
					output_line ")")
			|	IntTerm(i, pos) ->
					(output "RealTerm(";
					output (string_of_int i);
					printPos pos;
					output_line ")")
			|	StringTerm(s, pos) ->
					(output "StringTerm(";
					output s;
					printPos pos;
					output_line ")")
			|	OpTerm(o, pos) ->
					(output "OpTerm(";
					printOperation o;
					printPos pos;
					output_line ")")
			|	ErrorTerm ->
					(output "Error")
		
		and printClause = function
				Clause(ts) ->
					(output "Clause(";
					List.map printTerm ts;
					output_line ")")

		and printOperation = function
				COMMA -> output ","
			|	PLUS -> output "+"
			|	MINUS -> output "-"
			|	TIMES -> output "*"
			|	UMINUS -> output "-"
			|	GE -> output ">="
			|	GT -> output ">"
			|	LE -> output "<="
			|	LT -> output "<"

		and printPreAbsyn' = function
				Module(name, gconstants, lconstants, cconstants, fixities, gkinds, lkinds, tabbrevs, clauses, accummod, accumsig, usesig) ->
					(output_line ("Module:" ^ name);
					output_line "Constants:";
					List.map printConstant gconstants;
					List.map printConstant lconstants;
					List.map printConstant cconstants;
					output_line "";
					output_line "Kinds:";
					List.map printKind gkinds;
					output_line "";
					List.map printKind lkinds;
					output_line "Type Abbrevs:";
					List.map printTypeAbbrev tabbrevs;
					output_line "Clauses:";
					List.map printClause clauses;
					output_line "Fixities:";
					List.map printFixity fixities;
					())
			|	Signature(name, gconstants, gkinds, tabbrevs, fixities, accumsig) ->
					(output_line ("Signature: " ^ name);
					output_line "Constants:";
					List.map printConstant gconstants;
					output_line "Kinds:";
					List.map printKind gkinds;
					output_line "Type Abbrevs:";
					List.map printTypeAbbrev tabbrevs;
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
