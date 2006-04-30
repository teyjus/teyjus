%{
(****************************************************************************
*lp-yacc.mly:
*	This file contains the Teyjus ocamlyacc specification.  The semantic
*	actions are not yet correct (I haven't yet updated all of them to use
*	tuples and whatnot).
****************************************************************************)
module S = Symbol

let maxPrecedence = 255

(**********************************************************************
*getPos:
*	Gets the character position of the given token.
**********************************************************************)
let getPos = fun i ->
	let p = Parsing.rhs_start_pos i in
		p.pos_cnum


let getIDName = fun (name, kind) ->
	name
let getIDKind = fun (name, kind) ->
	kind


(**********************************************************************
*	These data structures are the result of parsing a signature or
*	module.  If a signature is parsed, the appropriate structures are
*	placed in a PreAbsyn.Signature structure and returned.  If a module
*	is parsed, the data is placed in a PreAbsyn.Module.
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
	
%token	<(string * PreAbsyn.pidkind)>	ID SYID VID UPCID
%token	<string> STRLIT
%token	<int> INTLIT
%token	<float> REALLIT
	
%type	<unit> sigormod module signature modheader sigheader
%type	<unit> modend modclause sigend modpreamble modbody
%type	<unit> modsigndecl signdecls signdecl
%type	<PreAbsyn.psymbol list> idlist cvidlist
%type	<int> kind
%type	<PreAbsyn.ptype> type ctype prtype
%type	<PreAbsyn.pfixity> fixity
%type	<PreAbsyn.pterm list> term
%type	<PreAbsyn.pterm> abstterm atomterm constvar
%type	<PreAbsyn.pterm> sigmaid piid nilid consid equalid

%type	<(string * PreAbsyn.pidkind)> tok
%type	<(S.symbol * PreAbsyn.ptype option * PreAbsyn.pidkind)> typedid sanstypedid
%type	<PreAbsyn.ptypesymbol list> typedidlist

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
														ErrorMsg.error (getPos 1) ("Expected module name '" ^ !moduleName ^ "'.")
													else
														()}
	;


sigheader
	:	SIG tok PERIOD	{if ((getIDName $2) <> !moduleName) then
											ErrorMsg.error (getPos 1) ("Expected signature name '" ^ !moduleName ^ "'.")
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
	:	tok										{PreAbsyn.Symbol(Symbol.symbol(getIDName $1), (getIDKind $1), getPos 1) :: []}
	|	cvidlist COMMA ID			{PreAbsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
	|	cvidlist COMMA UPCID	{PreAbsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
	|	cvidlist COMMA SYID		{PreAbsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
	;

idlist
	:	ID								{PreAbsyn.Symbol(Symbol.symbol(getIDName $1), (getIDKind $1), getPos 1) :: []}
	|	SYID							{PreAbsyn.Symbol(Symbol.symbol(getIDName $1), (getIDKind $1), getPos 1) :: []}
	|	idlist COMMA ID		{PreAbsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
	|	idlist COMMA SYID	{PreAbsyn.Symbol(Symbol.symbol(getIDName $3), (getIDKind $3), getPos 1) :: $1}
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
	:	KIND idlist kind PERIOD				{globalKinds := PreAbsyn.Kind($2, Some ($3 - 1), getPos 1) :: !globalKinds}
	|	TYPE idlist type PERIOD				{globalConstants := PreAbsyn.Constant($2, Some $3, getPos 1) :: !globalConstants}
	|	TYPEABBREV typeabbrev type PERIOD	{globalTypeAbbrevs := PreAbsyn.TypeAbbrev($2, $3, getPos 1) :: !globalTypeAbbrevs}
	|	fixity idlist INTLIT PERIOD		{if ($3 < 0 || $3 > maxPrecedence) then
																		ErrorMsg.error (getPos 1) ("Precedence must be between 0 and " ^ (string_of_int $3) ^ ".")
																	else
																		fixityList :=  PreAbsyn.Fixity($2, $1, $3, PreAbsyn.fixityGetPos $1) :: !FixityList}
	|	EXPORTDEF idlist PERIOD				{exportList := PreAbsyn.Constant($2, None, getPos 1) :: !exportList}
	|	EXPORTDEF idlist type PERIOD	{exportList := PreAbsyn.Constant($2, Some $3, getPos 1) :: !exportList}
	|	USEONLY idlist PERIOD					{useOnlyList := PreAbsyn.Constant($2, None, getPos 1) :: !useOnlyList}
	|	USEONLY idlist type PERIOD		{useOnlyList := PreAbsyn.Constant($2, Some $3, getPos 1) :: !useOnlyList}
;

modsigndecl
	:	signdecl											{}
	|	LOCAL idlist PERIOD						{localConstants := PreAbsyn.Constant($2, None, getPos 1) :: !localConstants}
	|	LOCAL idlist type PERIOD			{localConstants := PreAbsyn.Constant($2, Some $3, getPos 1) :: !localConstants}
	|	LOCALKIND idlist PERIOD				{localKinds := PreAbsyn.Kind($2, None, getPos 1) :: !localKinds}
	|	LOCALKIND idlist kind PERIOD	{localKinds := PreAbsyn.Kind($2, Some ($3 - 1), getPos 1) :: !localKinds}
	|	CLOSED idlist PERIOD					{closedConstants := PreAbsyn.Constant($2, ty = None, getPos 1) :: !closedConstants}
	|	CLOSED idlist type PERIOD			{closedConstants := PreAbsyn.Constant($2, ty = Some $3, getPos 1) :: !closedConstants}
	;

kind
	:	TYPE							{1}
	|	kind TYARROW TYPE	{$1 + 1}
	;

typeabbrev
	:	tok LPAREN varlist RPAREN			{}
	;

varlist
	:	VID COMMA varlist							{$1 :: $3}
	|	VID														{$1 :: []}
	;

type
	:	ctype TYARROW type	{PreAbsyn.Arrow($1, $3)}
	|	ctype								{$1}
	;

ctype
	:	prtype				{$1}
	|	ctype prtype	{PreAbsyn.App($1, $2)}
	;

prtype
	:	tok									{PreAbsyn.Atom($1.name, $1.kind, getPos 1)}
	|	LPAREN type RPAREN	{$2}
	;

fixity
	:	INFIX			{PreAbsyn.Infix(getPos 1)}
	|	INFIXL		{PreAbsyn.Infixl(getPos 1)}
	|	INFIXR		{PreAbsyn.Infixr(getPos 1)}
	|	PREFIX		{PreAbsyn.Prefix(getPos 1)}
	|	PREFIXR		{PreAbsyn.Prefixr(getPos 1)}
	|	POSTFIX		{PreAbsyn.Postfix(getPos 1)}
	|	POSTFIXL	{PreAbsyn.Posfixl(getPos 1)}
	;

modclause
	:	term PERIOD		{addPreClause $1}
	|	error PERIOD	{}
	;

term
	:	abstterm			{$1 :: []}
	|	term abstterm	{$2 :: $1}
	;

abstterm
	:	typedid INFIXLAMBDA term	{PreAbsyn.LambdaTerm(PreAbsyn.TypeSymbol($1.sym, $1.ty, $1.kind, $1.pos)::[], $3, getPos 1)}
	|	LAMBDA typedidlist term		{PreAbsyn.LambdaTerm($2, $3, getPos 1)}
	|	FORALL typedidlist term		{PreAbsyn.AllTerm($2, $3, getPos 1)}
	|	FORSOME typedidlist term	{PreAbsyn.SomeTerm($2, $3, getPos 1)}
	|	atomterm									{$1}
	;

atomterm
	:	constvar											{$1}
	|	LPAREN term RPAREN						{PreAbsyn.SeqTerm($2, getPos 2)}
	|	LPAREN error RPAREN						{PreAbsyn.ErrorTerm}
	|	LPAREN error									{PreAbsyn.ErrorTerm}
	|	LBRACK error RBRACK						{PreAbsyn.ErrorTerm}
	|	LBRACK error									{PreAbsyn.ErrorTerm}
	|	LBRACK RBRACK									{PreAbsyn.IdTerm((S.symbol "[]"), None, PreAbsyn.ConstID, getPos 1)}
	|	LBRACK term RBRACK						{PreAbsyn.listterm($2, getPos 1)}
	|	LBRACK term VBAR term RBRACK	{PreAbsyn.ConsTerm($2, PreAbsyn.SeqTerm($4, getPos 1), getPos 1)}
	;

constvar      
	:	typedid		{PreAbsyn.IdTerm($1.sym, $1.ty, $1.kind, $1.pos)}
	|	VID				{PreAbsyn.IdTerm($1.name, None, $1.kind, getPos 1)}
	|	VID COLON type	{PreAbsyn.IdTerm(S.symbol($1), (Some $3), $1.kind, getPos 1)}

	|	CUT				{PreAbsyn.IdTerm(S.symbol("!"), None, PreAbsyn.ConstID, getPos 1)}
	|	piid			{$1}
	|	sigmaid		{$1}
	|	nilid			{$1}
	|	consid		{$1}
	|	equalid		{$1}
	|	SEMICOLON	{PreAbsyn.IdTerm(S.symbol(";"), None, PreAbsyn.ConstID, getPos 1)}
	|	AMPAND		{PreAbsyn.IdTerm(S.symbol("&"), None, PreAbsyn.ConstID, getPos 1)}
	|	RDIVIDE		{PreAbsyn.IdTerm(S.symbol("/"), None, PreAbsyn.ConstID, getPos 1)}

	|	COMMA			{PreAbsyn.OpTerm(PreAbsyn.COMMA, getPos 1)}
	|	PLUS			{PreAbsyn.OpTerm(PreAbsyn.PLUS, getPos 1)}
	|	MINUS			{PreAbsyn.OpTerm(PreAbsyn.MINUS, getPos 1)}
	|	TIMES			{PreAbsyn.OpTerm(PreAbsyn.TIMES, getPos 1)}
	|	LESS			{PreAbsyn.OpTerm(PreAbsyn.LT, getPos 1)}
	|	LEQ				{PreAbsyn.OpTerm(PreAbsyn.LE, getPos 1)}
	|	GTR				{PreAbsyn.OpTerm(PreAbsyn.GT, getPos 1)}
	|	GEQ				{PreAbsyn.OpTerm(PreAbsyn.GE,  getPos 1)}
	|	UMINUS		{PreAbsyn.OpTerm(PreAbsyn.UMINUS, getPos 1)}
	|	REALLIT		{PreAbsyn.RealTerm($1, getPos 1)}
	|	INTLIT		{PreAbsyn.IntTerm($1, getPos 1)}
	|	STRLIT		{PreAbsyn.StringTerm($1, getPos 1)}
	|	COLONDASH	{PreAbsyn.IdTerm(S.symbol(":-"), None, PreAbsyn.ConstID, getPos 1)}
	|	IMPLIES		{PreAbsyn.IdTerm(S.symbol("=>"), None, PreAbsyn.ConstID, getPos 1)}
	;

piid
	:	PI									{PreAbsyn.IdTerm(S.symbol("pi"), None, PreAbsyn.ConstID, getPos 1)}
	| PI COLON type				{PreAbsyn.IdTerm(S.symbol("pi"), Some $3, PreAbsyn.ConstID, getPos 1)}
	| LPAREN piid RPAREN	{$2}
	;

sigmaid
	:	SIGMA									{PreAbsyn.IdTerm(S.symbol("sigma"), None, PreAbsyn.ConstID, getPos 1)}
	|	SIGMA COLON type			{PreAbsyn.IdTerm(S.symbol("sigma"), Some $3, PreAbsyn.ConstID, getPos 1)}
	| LPAREN sigmaid RPAREN	{$2}
	;


nilid
	:	NILLIST							{PreAbsyn.IdTerm(S.symbol("[]"), None, PreAbsyn.ConstID, getPos 1)}
	|	NILLIST COLON type	{PreAbsyn.IdTerm(S.symbol("[]"), Some $3, PreAbsyn.ConstID, getPos 1)}
	|	LPAREN nilid RPAREN	{$2}
	;

consid
	:	LISTCONS							{PreAbsyn.IdTerm(S.symbol("::"), None, PreAbsyn.ConstID, getPos 1)}
	|	LISTCONS COLON type		{PreAbsyn.IdTerm(S.symbol("::"), Some $3, PreAbsyn.ConstID, getPos 1)}
	|	LPAREN consid RPAREN	{$2}
	;

equalid
	:	EQUAL									{PreAbsyn.IdTerm(S.symbol("="), None, PreAbsyn.ConstID, getPos 1)}
	|	EQUAL COLON type			{PreAbsyn.IdTerm(S.symbol("="), Some $3, PreAbsyn.ConstID, getPos 1)}
	|	LPAREN equalid RPAREN	{$2}
	;

typedid
	:	sanstypedid						{$1}
	|	LPAREN typedid RPAREN	{$2}
	;

sanstypedid
	:	ID								{PreAbsyn.IdTerm($1.name, None, $1.kind, getPos 1)}
	|	UPCID							{PreAbsyn.IdTerm($1.name, None, $1.kind, getPos 1)}
	|	SYID							{PreAbsyn.IdTerm($1.name, None, $1.kind, getPos 1)}
	|	ID COLON type			{PreAbsyn.IdTerm($1.name, ty = Some $3, $1.kind, getPos 1)}
	|	UPCID COLON type	{PreAbsyn.IdTerm($1.name, ty = Some $3, $1.kind, getPos 1)}
	|	SYID COLON type		{PreAbsyn.IdTerm($1.name, ty = Some $3, $1.kind, getPos 1)}
               ;

typedidlist
	:	typedid										{PreAbsyn.TypeSymbol($1.sym, $1.ty, $1.kind, $1.pos) :: []}
	|	typedid COMMA typedidlist	{PreAbsyn.TypeSymbol($1.sym, $1.ty, $1.kind, $1.pos) :: $3}
	|	LPAREN typedid COMMA typedidlist RPAREN
															{PreAbsyn.TypeSymbol($2.sym, $2.ty, $2.kind, $2.pos) :: $4}

%%
