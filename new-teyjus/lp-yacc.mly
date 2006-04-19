%{
(****************************************************************************
*lp-yacc.mly:
*	This file contains the Teyjus ocamlyacc specification.  The semantic
*	actions are not yet correct (I haven't yet updated all of them to use
*	tuples and whatnot).
****************************************************************************)

(**********************************************************************
*getPos:
*	Gets the character position of the given token.
**********************************************************************)
let getPos = fun i ->
	let p: Lexing.position = Parsing.rhs_start_pos i in
		i.pos_cnum

%}


%token	MODULE END IMPORT ACCUMULATE ACCUMSIG USESIG LOCAL
%token	LOCALKIND CLOSED SIG KIND TYPE EXPORTDEF
%token	USEONLY INFIXL INFIX INFIXR PREFIX PREFIXR
%token	POSTFIX POSTFIXL LAMBDA FORALL FORSOME COLONDASH
%token	IMPLIES INFIXLAMBDA TYARROW CUT PI SIGMA COMMA
%token	SEMICOLON AMPAND RDIVIDE NILLIST LISTCONS EQUAL
%token	PLUS MINUS TIMES LESS LEQ GTR GEQ UMINUS PERIOD
%token	LPAREN RPAREN LBRACK RBRACK COLON VBAR
%token	SIGSTART MODSTART TERMSTART
	
%token	<{name: string, kind: P.idkind}>	ID SYID VID UPCID
%token	<string> STRLIT
%token	<int> INTLIT
%token	<float> REALLIT
	
%type	<pos> sigormod module signature modheader sigheader
%type	<pos> modend modclause sigend modpreamble modbody
%type	<pos> modsigndecl signdecls signdecl
%type	<P.psymbol list> idlist cvidlist
%type	<int> kind
%type	<P.ptype> type ctype prtype
%type	<P.pfixity> fixity
%type	<P.pterm list> term
%type	<P.pterm> abstterm atomterm constvar
%type	<P.pterm> sigmaid piid nilid consid equalid

%type	<{name: string, kind: P.idkind}> tok
%type	<{sym: S.symbol, ty: P.ptype ref, kind: P.idkind}> typedid sanstypedid
%type	<P.ptypesymbol list> typedidlist

%nonassoc  INFIXLAMBDA LAMBDA FORALL FORSOME
%nonassoc  COLONDASH IMPLIES CUT PI SIGMA COMMA SEMICOLON AMPAND RDIVIDE
           NILLIST LISTCONS EQUAL PLUS MINUS TIMES LESS LEQ GTR GEQ UMINUS
           LPAREN LBRACK ID SYID VID UPCID STRLIT INTLIT REALLIT

%start sigormod

%%
sigormod
	:	SIGSTART  signature			{}
	|	MODSTART  module				{}
	|	TERMSTART modclause			{P_LastToken := TRUE}
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
	:   MODULE tok PERIOD	{if (S.name($2.name) <> !moduleName) then
														EM.error("Expected module name '" ^ !moduleName ^ "'.")
													else
														()}
	;


sigheader
	:	SIG tok PERIOD	{if (S.name($2.name) <> !moduleName) then
											EM.error("Expected signature name '" ^ !moduleName ^ "'.")
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
	:	tok										{P.Symbol($1.name, $1.kind, getPos 1) :: nil}
	|	cvidlist COMMA ID			{P.Symbol($3.name, $3.kind, getPos 1) :: $1}
	|	cvidlist COMMA UPCID	{P.Symbol($3.name, $3.kind, getPos 1) :: $1}
	|	cvidlist COMMA SYID		{P.Symbol($3.name, $3.kind, getPos 1) :: $1}
	;

idlist
	:	ID								{P.Symbol($1.name, $1.kind, getPos 1) :: nil}
	|	SYID							{P.Symbol($1.name, $1.kind, getPos 1) :: nil}
	|	idlist COMMA ID		{P.Symbol($3.name, $3.kind, getPos 1) :: $1}
	|	idlist COMMA SYID	{P.Symbol($3.name, $3.kind, getPos 1) :: $1}
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
	:	KIND idlist kind PERIOD				{globalKindList := P.Kind($2, SOME ($3 - 1), getPos 1) :: !globalKindList}
	|	TYPE idlist type PERIOD				{globalConstants := P.Constant($2, SOME $3, getPos 1) :: !globalConstants}
	|	fixity idlist INTLIT PERIOD		{if ($3 < 0 orelse $3 > maxPrecedence) then
																		EM.error("Precedence must be between 0 and " ^ (intToString $3) ^ ".")
																	else
																		fixityList :=  P.Fixity($2, $1, $3, $1.pos) :: !FixityList}
	|	EXPORTDEF idlist PERIOD				{exportList := P.Constant($2, NONE, getPos 1) :: !exportList}
	|	EXPORTDEF idlist type PERIOD	{exportList := P.Constant($2, SOME $3, getPos 1) :: !exportList}
	|	USEONLY idlist PERIOD					{useOnlyList := P.Constant($2, NONE, getPos 1) :: !useOnlyList}
	|	USEONLY idlist type PERIOD		{useOnlyList := P.Constant($2, SOME $3, getPos 1) :: !useOnlyList}
;

modsigndecl
	:	signdecl											{}
	|	LOCAL idlist PERIOD						{localConstants := P.Constant($2, NONE, getPos 1) :: !localConstants}
	|	LOCAL idlist type PERIOD			{localConstants := P.Constant($2, SOME $3, getPos 1) :: !localConstants}
	|	LOCALKIND idlist PERIOD				{localKinds := P.Kind($2, NONE, getPos 1) :: !localKinds}
	|	LOCALKIND idlist kind PERIOD	{localKinds := P.Kind($2, SOME ($3 - 1), getPos 1) :: !localKinds}
	|	CLOSED idlist PERIOD					{closedConstants := P.Constant($2, ty = NONE, getPos 1) :: !closedConstants}
	|	CLOSED idlist type PERIOD			{closedConstants := P.Constant($2, ty = SOME $3, getPos 1) :: !closedConstants}
	;

kind
	:	TYPE							{1}
	|	kind TYARROW TYPE	{$1 + 1}
	;

type
	:	ctype TYARROW type	{P.Arrow($1, $3)}
	|	ctype								{$1}
	;

ctype
	:	prtype				{$1}
	|	ctype prtype	{P.App($1, $2)}
	;

prtype
	:	tok									{P.Atom($1.name, $1.kind, getPos 1)}
	|	LPAREN type RPAREN	{$2}
	;

fixity
	:	INFIX			{P.Infix(getPos 1)}
	|	INFIXL		{P.Infixl(getPos 1)}
	|	INFIXR		{P.Infixr(getPos 1)}
	|	PREFIX		{P.Prefix(getPos 1)}
	|	PREFIXR		{P.Prefixr(getPos 1)}
	|	POSTFIX		{P.Postfix(getPos 1)}
	|	POSTFIXL	{P.Posfixl(getPos 1)}
	;


modclause
	:	term PERIOD		{addPreClause $1}
	|	error PERIOD	{}
	;


term
	:	abstterm			{$1 :: nil}
	|	term abstterm	{$2 :: $1}
	;

abstterm
	:	typedid INFIXLAMBDA term	{P.LambdaTerm(P.TypeSymbol($1.sym, $1.ty, $1.kind, $1.pos)::nil, $3, getPos 1)}
	|	LAMBDA typedidlist term		{P.LambdaTerm($2, $3, getPos 1)}
	|	FORALL typedidlist term		{P.AllTerm($2, $3, getPos 1)}
	|	FORSOME typedidlist term	{P.SomeTerm($2, $3, getPos 1)}
	|	atomterm									{$1}
	;

atomterm
	:	constvar											{$1}
	|	LPAREN term RPAREN						{P.SeqTerm($2, getPos 2)}
	|	LPAREN error RPAREN						{P.ErrorTerm}
	|	LPAREN error									{P.ErrorTerm}
	|	LBRACK error RBRACK						{P.ErrorTerm}
	|	LBRACK error									{P.ErrorTerm}
	|	LBRACK RBRACK									{P.IdTerm((S.symbol "nil"), NONE, P.ConstID, getPos 1)}
	|	LBRACK term RBRACK						{P.listterm($2, getPos 1)}
	|	LBRACK term VBAR term RBRACK	{P.ConsTerm($2, P.SeqTerm($4, getPos 1), getPos 1)}
	;

constvar      
	:	typedid		{P.IdTerm($1.sym, $1.ty, $1.kind, $1.pos)}
	|	VID				{P.IdTerm($1.name, NONE, $1.kind, getPos 1)}
	|	VID COLON type	{P.IdTerm(S.symbol($1), (SOME $3), $1.kind, getPos 1)}

	|	CUT				{P.IdTerm(S.symbol("!"), NONE, P.ConstID, getPos 1)}
	|	piid			{$1}
	|	sigmaid		{$1}
	|	nilid			{$1}
	|	consid		{$1}
	|	equalid		{$1}
	|	SEMICOLON	{P.IdTerm(S.symbol(";"), NONE, P.ConstID, getPos 1)}
	|	AMPAND		{P.IdTerm(S.symbol("&"), NONE, P.ConstID, getPos 1)}
	|	RDIVIDE		{P.IdTerm(S.symbol("/"), NONE, P.ConstID, getPos 1)}

	|	COMMA			{P.OpTerm(P.COMMA, getPos 1)}
	|	PLUS			{P.OpTerm(P.PLUS, getPos 1)}
	|	MINUS			{P.OpTerm(P.MINUS, getPos 1)}
	|	TIMES			{P.OpTerm(P.TIMES, getPos 1)}
	|	LESS			{P.OpTerm(P.LT, getPos 1)}
	|	LEQ				{P.OpTerm(P.LE, getPos 1)}
	|	GTR				{P.OpTerm(P.GT, getPos 1)}
	|	GEQ				{P.OpTerm(P.GE,  getPos 1)}
	|	UMINUS		{P.OpTerm(P.UMINUS, getPos 1)}
	|	REALLIT		{P.RealTerm($1, getPos 1)}
	|	INTLIT		{P.IntTerm($1, getPos 1)}
	|	STRLIT		{P.StringTerm($1, getPos 1)}
	|	COLONDASH	{P.IdTerm(S.symbol(":-"), NONE, P.ConstID, getPos 1)}
	|	IMPLIES		{P.IdTerm(S.symbol("=>"), NONE, P.ConstID, getPos 1)}
	;

piid
	:	PI									{P.IdTerm(S.symbol("pi"), NONE, P.ConstID, getPos 1)}
	| PI COLON type				{P.IdTerm(S.symbol("pi"), SOME $3, P.ConstID, getPos 1)}
	| LPAREN piid RPAREN	{$2}
	;

sigmaid
	:	SIGMA									{P.IdTerm(S.symbol("sigma"), NONE, P.ConstID, getPos 1)}
	|	SIGMA COLON type			{P.IdTerm(S.symbol("sigma"), SOME $3, P.ConstID, getPos 1)}
	| LPAREN sigmaid RPAREN	{$2}
	;


nilid
	:	NILLIST							{P.IdTerm(S.symbol("nil"), NONE, P.ConstID, getPos 1)}
	|	NILLIST COLON type	{P.IdTerm(S.symbol("nil"), SOME $3, P.ConstID, getPos 1)}
	|	LPAREN nilid RPAREN	{$2}
	;

consid
	:	LISTCONS							{P.IdTerm(S.symbol("::"), NONE, P.ConstID, getPos 1)}
	|	LISTCONS COLON type		{P.IdTerm(S.symbol("::"), SOME $3, P.ConstID, getPos 1)}
	|	LPAREN consid RPAREN	{$2}
	;

equalid
	:	EQUAL									{P.IdTerm(S.symbol("="), NONE, P.ConstID, getPos 1)}
	|	EQUAL COLON type			{P.IdTerm(S.symbol("="), SOME $3, P.ConstID, getPos 1)}
	|	LPAREN equalid RPAREN	{$2}
	;

typedid
	:	sanstypedid						{$1}
	|	LPAREN typedid RPAREN	{$2}
	;

sanstypedid
	:	ID								{P.IdTerm($1.name, NONE, $1.kind, getPos 1)}
	|	UPCID							{P.IdTerm($1.name, NONE, $1.kind, getPos 1)}
	|	SYID							{P.IdTerm($1.name, NONE, $1.kind, getPos 1)}
	|	ID COLON type			{P.IdTerm($1.name, ty = SOME $3, $1.kind, getPos 1)}
	|	UPCID COLON type	{P.IdTerm($1.name, ty = SOME $3, $1.kind, getPos 1)}
	|	SYID COLON type		{P.IdTerm($1.name, ty = SOME $3, $1.kind, getPos 1)}
               ;

typedidlist
	:	typedid										{P.TypeSymbol($1.sym, $1.ty, $1.kind, $1.pos) :: nil}
	|	typedid COMMA typedidlist	{P.TypeSymbol($1.sym, $1.ty, $1.kind, $1.pos) :: $3}
	|	LPAREN typedid COMMA typedidlist RPAREN
															{P.TypeSymbol($2.sym, $2.ty, $2.kind, $2.pos) :: $4}

%%
