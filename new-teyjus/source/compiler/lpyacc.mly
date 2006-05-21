%{
(****************************************************************************
*lpyacc.mly:
*	This file contains the Teyjus ocamlyacc specification.  The semantic
*	actions are more or less correct...
****************************************************************************)
open Errormsg
open Lexing

let maxPrecedence = 255

(**********************************************************************
*getPos:
*	Gets the character position of the given token.
**********************************************************************)
let getPos = fun i ->
	let p = Parsing.rhs_start_pos i in
		p.pos_cnum

(* Accessors for IDs	*)
let getIDName = fun (name, kind) ->
	name
let getIDKind = fun (name, kind) ->
	kind


(**********************************************************************
*	These data structures are the result of parsing a signature or
*	module.  If a signature is parsed, the appropriate structures are
*	placed in a Preabsyn.Signature structure and returned.  If a module
*	is parsed, the data is placed in a Preabsyn.Module.
**********************************************************************)
let moduleName = ref ""

let importList = ref []

let accumulatedModList = ref []
let accumulatedSigList = ref []
let useSigList = ref []
let useOnlyList = ref []

let globalConstants = ref []
let closedConstants = ref []
let localConstants = ref []

let globalKinds = ref []

let globalTypeAbbrevs = ref []

let fixityList = ref []

let preAbsynClauses = ref []

%}


%token	MODULE END IMPORT ACCUMULATE ACCUMSIG USESIG LOCAL
%token	LOCALKIND CLOSED SIG KIND TYPE TYPEABBREV EXPORTDEF
%token	USEONLY INFIXL INFIX INFIXR PREFIX PREFIXR
%token	POSTFIX POSTFIXL LAMBDA FORALL FORSOME COLONDASH
%token	IMPLIES INFIXLAMBDA TYARROW CUT PI SIGMA COMMA
%token	SEMICOLON AMPAND RDIVIDE NILLIST LISTCONS EQUAL
%token	PLUS MINUS TIMES LESS LEQ GTR GEQ UMINUS PERIOD
%token	LPAREN RPAREN LBRACK RBRACK COLON VBAR
%token	SIGSTART MODSTART TERMSTART
	
%token	<(string * Preabsyn.pidkind)>	ID SYID VID UPCID
%token	<string> STRLIT
%token	<int> INTLIT
%token	<float> REALLIT
	
%type	<unit> sigormod module signature modheader sigheader
%type	<unit> modend modclause sigend modpreamble modbody
%type	<unit> modsigndecl signdecls signdecl
%type	<Preabsyn.psymbol list> idlist cvidlist
%type	<int> kind
%type	<Preabsyn.ptype> type ctype prtype
%type	<Preabsyn.pfixity> fixity
%type	<Preabsyn.pterm list> term
%type	<Preabsyn.pterm> abstterm atomterm constvar
%type	<Preabsyn.pterm> sigmaid piid nilid consid equalid
%type <(Symbol.symbol * Symbol.symbol list)> typeabbrev

%type	<(string * Preabsyn.pidkind)> tok
%type	<(Symbol.symbol * Preabsyn.ptype option * Preabsyn.pidkind)> typedid sanstypedid
%type	<(Preabsyn.ptypesymbol list)> typedidlist

%nonassoc  INFIXLAMBDA LAMBDA FORALL FORSOME
%nonassoc  COLONDASH IMPLIES CUT PI SIGMA COMMA SEMICOLON AMPAND RDIVIDE
           NILLIST LISTCONS EQUAL PLUS MINUS TIMES LESS LEQ GTR GEQ UMINUS
           LPAREN LBRACK ID SYID VID UPCID STRLIT INTLIT REALLIT

%start sigormod

%%
sigormod
	:	SIGSTART signature		{}
	|	MODSTART module				{}
	|	TERMSTART modclause		{}
	;

module
	:	modheader modpreamble modbody modend		{}
	|	error modpreamble modbody modend				{}
	;

signature
	:	sigheader sigpreamble signdecls sigend	{}
	|	error signdecls sigend									{}
	;

tok
	:	ID		{$1}
	|	UPCID	{$1}
	|	SYID	{$1}
	|	VID		{$1}
	;

modheader
	:   MODULE tok PERIOD	{if ((getIDName $2) <> !moduleName) then
														Errormsg.error (getPos 1) ("Expected module name '" ^ !moduleName ^ "'.")
													else
														()}
	;


sigheader
	:	SIG tok PERIOD	{if ((getIDName $2) <> !moduleName) then
											Errormsg.error (getPos 1) ("Expected signature name '" ^ !moduleName ^ "'.")
										else
											()}
	;

modend
	:				{}
	|	END		{}
	;


sigend
	:				{}
	|	END		{}
	;

modpreamble
	:			{}
	|	modpreamble IMPORT  cvidlist  PERIOD
                {importList := $3 :: !importList}
	|	modpreamble ACCUMULATE cvidlist PERIOD
                {accumulatedModList := $3 :: !accumulatedModList}
	|	modpreamble ACCUMSIG cvidlist PERIOD
                {accumulatedSigList := $3 :: !accumulatedSigList}
	|	modpreamble USESIG cvidlist PERIOD
								{useSigList := $3 :: !useSigList}
	;

sigpreamble
	:							{}
	|	sigpreamble ACCUMSIG cvidlist PERIOD
                {accumulatedSigList := $3 :: !accumulatedSigList}
	;

cvidlist
	:	tok										{Preabsyn.Symbol(Symbol.symbol(getIDName $1), (getIDKind $1), getPos 1) :: []}
	|	cvidlist COMMA ID			{Preabsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
	|	cvidlist COMMA UPCID	{Preabsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
	|	cvidlist COMMA SYID		{Preabsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
	;

idlist
	:	ID								{Preabsyn.Symbol(Symbol.symbol(getIDName $1), (getIDKind $1), getPos 1) :: []}
	|	SYID							{Preabsyn.Symbol(Symbol.symbol(getIDName $1), (getIDKind $1), getPos 1) :: []}
	|	idlist COMMA ID		{Preabsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
	|	idlist COMMA SYID	{Preabsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
	;


modbody
	: {}
	|	modbody modsigndecl {}
	|	modbody modclause {}
	;

signdecls
	: {}
	|	signdecls signdecl {}
	;

signdecl
	:	KIND idlist kind PERIOD				{globalKinds := Preabsyn.Kind($2, Some ($3 - 1), getPos 1) :: !globalKinds}
	|	TYPE idlist type PERIOD				{globalConstants := Preabsyn.Constant($2, Some $3, getPos 1) :: !globalConstants}
	|	TYPEABBREV typeabbrev type PERIOD	{let (name, argnames) = $2 in
																			globalTypeAbbrevs := Preabsyn.TypeAbbrev(name, argnames, $3, getPos 1) :: !globalTypeAbbrevs}
	|	fixity idlist INTLIT PERIOD		{if ($3 < 0 || $3 > maxPrecedence) then
																		Errormsg.error (getPos 1) ("Precedence must be between 0 and " ^ (string_of_int $3) ^ ".")
																	else
																		fixityList :=  Preabsyn.Fixity($2, $1, $3, Preabsyn.fixityGetPos $1) :: !FixityList}
	|	EXPORTDEF idlist PERIOD				{exportList := Preabsyn.Constant($2, None, getPos 1) :: !exportList}
	|	EXPORTDEF idlist type PERIOD	{exportList := Preabsyn.Constant($2, Some $3, getPos 1) :: !exportList}
	|	USEONLY idlist PERIOD					{useOnlyList := Preabsyn.Constant($2, None, getPos 1) :: !useOnlyList}
	|	USEONLY idlist type PERIOD		{useOnlyList := Preabsyn.Constant($2, Some $3, getPos 1) :: !useOnlyList}
;

modsigndecl
	:	signdecl											{}
	|	LOCAL idlist PERIOD						{localConstants := Preabsyn.Constant($2, None, getPos 1) :: !localConstants}
	|	LOCAL idlist type PERIOD			{localConstants := Preabsyn.Constant($2, Some $3, getPos 1) :: !localConstants}
	|	LOCALKIND idlist PERIOD				{localKinds := Preabsyn.Kind($2, None, getPos 1) :: !localKinds}
	|	LOCALKIND idlist kind PERIOD	{localKinds := Preabsyn.Kind($2, Some ($3 - 1), getPos 1) :: !localKinds}
	|	CLOSED idlist PERIOD					{closedConstants := Preabsyn.Constant($2, ty = None, getPos 1) :: !closedConstants}
	|	CLOSED idlist type PERIOD			{closedConstants := Preabsyn.Constant($2, ty = Some $3, getPos 1) :: !closedConstants}
	;

kind
	:	TYPE							{1}
	|	kind TYARROW TYPE	{$1 + 1}
	;

typeabbrev
	:	tok LPAREN varlist RPAREN			{((getIDName $1), $3)}
	;

varlist
	:	tok COMMA varlist							{$1 :: $3}
	|	tok														{$1 :: []}
	;

type
	:	ctype TYARROW type	{Preabsyn.Arrow($1, $3)}
	|	ctype								{$1}
	;

ctype
	:	prtype				{$1}
	|	ctype prtype	{Preabsyn.App($1, $2)}
	;

prtype
	:	tok													{Preabsyn.Atom((getIDName $1), (getIDKind $1), getPos 1)}
	|	tok LPAREN typelist RPAREN	{Preabsyn.TypeAbbrevCall((GetIDName $1), $3, getPos 1)}
	|	LPAREN type RPAREN					{$2}
	;

typelist
	:	type									{$1 :: []}
	|	typelist COMMA type		{$3 :: $1}
	;

fixity
	:	INFIX			{Preabsyn.Infix(getPos 1)}
	|	INFIXL		{Preabsyn.Infixl(getPos 1)}
	|	INFIXR		{Preabsyn.Infixr(getPos 1)}
	|	PREFIX		{Preabsyn.Prefix(getPos 1)}
	|	PREFIXR		{Preabsyn.Prefixr(getPos 1)}
	|	POSTFIX		{Preabsyn.Postfix(getPos 1)}
	|	POSTFIXL	{Preabsyn.Posfixl(getPos 1)}
	;

modclause
	:	term PERIOD		{(PreabsynClauses := $1 :: (!PreabsynClauses))}
	|	error PERIOD	{}
	;

term
	:	abstterm			{$1 :: []}
	|	term abstterm	{$2 :: $1}
	;

abstterm
	:	typedid INFIXLAMBDA term	{Preabsyn.LambdaTerm(Preabsyn.TypeSymbol($1.sym, $1.ty, $1.kind, $1.pos)::[], $3, getPos 1)}
	|	LAMBDA typedidlist term		{Preabsyn.LambdaTerm($2, $3, getPos 1)}
	|	FORALL typedidlist term		{Preabsyn.AllTerm($2, $3, getPos 1)}
	|	FORSOME typedidlist term	{Preabsyn.SomeTerm($2, $3, getPos 1)}
	|	atomterm									{$1}
	;

atomterm
	:	constvar											{$1}
	|	LPAREN term RPAREN						{Preabsyn.SeqTerm($2, getPos 2)}
	|	LPAREN error RPAREN						{Preabsyn.ErrorTerm}
	|	LPAREN error									{Preabsyn.ErrorTerm}
	|	LBRACK error RBRACK						{Preabsyn.ErrorTerm}
	|	LBRACK error									{Preabsyn.ErrorTerm}
	|	LBRACK RBRACK									{Preabsyn.IdTerm((Symbol.symbol "[]"), None, Preabsyn.ConstID, getPos 1)}
	|	LBRACK term RBRACK						{Preabsyn.ListTerm($2, getPos 1)}
	|	LBRACK term VBAR term RBRACK	{Preabsyn.ConsTerm($2, Preabsyn.SeqTerm($4, getPos 1), getPos 1)}
	;

constvar      
	:	typedid		{Preabsyn.IdTerm($1.sym, $1.ty, $1.kind, $1.pos)}
	|	VID				{Preabsyn.IdTerm((getIDName $1), None, (getIDKind $1), getPos 1)}
	|	VID COLON type	{Preabsyn.IdTerm((getIDName $1), (Some $3), (getIDKind $1), getPos 1)}

	|	CUT				{Preabsyn.IdTerm(Symbol.symbol("!"), None, Preabsyn.ConstID, getPos 1)}
	|	piid			{$1}
	|	sigmaid		{$1}
	|	nilid			{$1}
	|	consid		{$1}
	|	equalid		{$1}
	|	SEMICOLON	{Preabsyn.IdTerm(Symbol.symbol(";"), None, Preabsyn.ConstID, getPos 1)}
	|	AMPAND		{Preabsyn.IdTerm(Symbol.symbol("&"), None, Preabsyn.ConstID, getPos 1)}
	|	RDIVIDE		{Preabsyn.IdTerm(Symbol.symbol("/"), None, Preabsyn.ConstID, getPos 1)}

	|	COMMA			{Preabsyn.OpTerm(Preabsyn.COMMA, getPos 1)}
	|	PLUS			{Preabsyn.OpTerm(Preabsyn.PLUS, getPos 1)}
	|	MINUS			{Preabsyn.OpTerm(Preabsyn.MINUS, getPos 1)}
	|	TIMES			{Preabsyn.OpTerm(Preabsyn.TIMES, getPos 1)}
	|	LESS			{Preabsyn.OpTerm(Preabsyn.LT, getPos 1)}
	|	LEQ				{Preabsyn.OpTerm(Preabsyn.LE, getPos 1)}
	|	GTR				{Preabsyn.OpTerm(Preabsyn.GT, getPos 1)}
	|	GEQ				{Preabsyn.OpTerm(Preabsyn.GE,  getPos 1)}
	|	UMINUS		{Preabsyn.OpTerm(Preabsyn.UMINUS, getPos 1)}
	|	REALLIT		{Preabsyn.RealTerm($1, getPos 1)}
	|	INTLIT		{Preabsyn.IntTerm($1, getPos 1)}
	|	STRLIT		{Preabsyn.StringTerm($1, getPos 1)}
	|	COLONDASH	{Preabsyn.IdTerm(Symbol.symbol(":-"), None, Preabsyn.ConstID, getPos 1)}
	|	IMPLIES		{Preabsyn.IdTerm(Symbol.symbol("=>"), None, Preabsyn.ConstID, getPos 1)}
	;

piid
	:	PI									{Preabsyn.IdTerm(Symbol.symbol("pi"), None, Preabsyn.ConstID, getPos 1)}
	| PI COLON type				{Preabsyn.IdTerm(Symbol.symbol("pi"), Some $3, Preabsyn.ConstID, getPos 1)}
	| LPAREN piid RPAREN	{$2}
	;

sigmaid
	:	SIGMA									{Preabsyn.IdTerm(Symbol.symbol("sigma"), None, Preabsyn.ConstID, getPos 1)}
	|	SIGMA COLON type			{Preabsyn.IdTerm(Symbol.symbol("sigma"), Some $3, Preabsyn.ConstID, getPos 1)}
	| LPAREN sigmaid RPAREN	{$2}
	;


nilid
	:	NILLIST							{Preabsyn.IdTerm(Symbol.symbol("[]"), None, Preabsyn.ConstID, getPos 1)}
	|	NILLIST COLON type	{Preabsyn.IdTerm(Symbol.symbol("[]"), Some $3, Preabsyn.ConstID, getPos 1)}
	|	LPAREN nilid RPAREN	{$2}
	;

consid
	:	LISTCONS							{Preabsyn.IdTerm(Symbol.symbol("::"), None, Preabsyn.ConstID, getPos 1)}
	|	LISTCONS COLON type		{Preabsyn.IdTerm(Symbol.symbol("::"), Some $3, Preabsyn.ConstID, getPos 1)}
	|	LPAREN consid RPAREN	{$2}
	;

equalid
	:	EQUAL									{Preabsyn.IdTerm(Symbol.symbol("="), None, Preabsyn.ConstID, getPos 1)}
	|	EQUAL COLON type			{Preabsyn.IdTerm(Symbol.symbol("="), Some $3, Preabsyn.ConstID, getPos 1)}
	|	LPAREN equalid RPAREN	{$2}
	;

typedid
	:	sanstypedid						{$1}
	|	LPAREN typedid RPAREN	{$2}
	;

sanstypedid
	:	ID								{Preabsyn.IdTerm((getIDName $1), None, (GetIDKind $1), getPos 1)}
	|	UPCID							{Preabsyn.IdTerm((getIDName $1), None, (GetIDKind $1), getPos 1)}
	|	SYID							{Preabsyn.IdTerm((getIDName $1), None, (GetIDKind $1), getPos 1)}
	|	ID COLON type			{Preabsyn.IdTerm((getIDName $1), ty = Some $3, (GetIDKind $1), getPos 1)}
	|	UPCID COLON type	{Preabsyn.IdTerm((getIDName $1), ty = Some $3, (GetIDKind $1), getPos 1)}
	|	SYID COLON type		{Preabsyn.IdTerm((getIDName $1), ty = Some $3, (GetIDKind $1), getPos 1)}
               ;

typedidlist
	:	typedid										{Preabsyn.TypeSymbol($1.sym, $1.ty, $1.kind, $1.pos) :: []}
	|	typedid COMMA typedidlist	{Preabsyn.TypeSymbol($1.sym, $1.ty, $1.kind, $1.pos) :: $3}
	|	LPAREN typedid COMMA typedidlist RPAREN
															{Preabsyn.TypeSymbol($2.sym, $2.ty, $2.kind, $2.pos) :: $4}

%%
