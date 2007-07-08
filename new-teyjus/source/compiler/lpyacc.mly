%{
(****************************************************************************
*lpyacc.mly:
* This file contains the Teyjus ocamlyacc specification.  The semantic
* actions are more or less correct...
****************************************************************************)
open Lexing
open Preabsyn

let basename () =
  Filename.chop_extension (symbol_start_pos ()).pos_fname

type pos = Errormsg.pos

let maxPrecedence = 255

(**********************************************************************
* These data structures are the result of parsing a signature or
* module.  If a signature is parsed, the appropriate structures are
* placed in a Preabsyn.Signature structure and returned.  If a module
* is parsed, the data is placed in a Preabsyn.Module.
**********************************************************************)
let importedModList = ref []

let accumulatedModList = ref []
let accumulatedSigList = ref []
let useSigList = ref []
let useOnlyList = ref []
let exportList = ref []
let clauseList = ref []

let globalConstants = ref []
let closedConstants = ref []
let localConstants = ref []

let globalKinds = ref []
let localKinds = ref []

let globalTypeAbbrevs = ref []

let fixityList = ref []

let reverseResults () =
  accumulatedModList := List.rev !accumulatedModList;
  importedModList := List.rev !importedModList;
  accumulatedSigList := List.rev !accumulatedSigList;
  useSigList := List.rev !useSigList;
  useOnlyList := List.rev !useOnlyList;
  exportList := List.rev !exportList;
  clauseList := List.rev !clauseList;

  globalConstants := List.rev !globalConstants;
  closedConstants := List.rev !closedConstants;
  localConstants := List.rev !localConstants;

  globalKinds := List.rev !globalKinds;
  localKinds := List.rev !localKinds;

  globalTypeAbbrevs := List.rev !globalTypeAbbrevs;

  fixityList := List.rev !fixityList

let clearResults () =
  importedModList := [];
  accumulatedModList := [];
  accumulatedSigList := [];
  useSigList := [];
  useOnlyList := [];
  exportList := [];
  clauseList := [];

  globalConstants := [];
  closedConstants := [];
  localConstants := [];

  globalKinds := [];
  localKinds := [];

  globalTypeAbbrevs := [];

  fixityList := []

(**********************************************************************
*getPos:
* Gets the character position of the given token.
**********************************************************************)
let getPos i =
  Parsing.rhs_start_pos i

(* Accessors for IDs  *)
let getIDName (name, _) = name
let getIDKind (_, kind) = kind

(* Accessors for Typed IDs  *)
let getTypedIDSym (s,_,_,_) = s
let getTypedIDType (_,t,_,_) = t
let getTypedIDKind (_,_,k,_) = k
let getTypedIDPos (_,_,_,p) = p


let makeSymbol pos t =
  let name = getIDName t in
  let k = getIDKind t in
    Symbol(Symbol.symbol name, k, pos)

let makeModule () =
  reverseResults () ;
  let m = Module(basename (), !globalConstants, !localConstants,
                 !closedConstants, !useOnlyList, !fixityList,
                 !globalKinds, !localKinds, !globalTypeAbbrevs,
                 !clauseList, !accumulatedModList, !accumulatedSigList,
                 !useSigList, !importedModList) in
    clearResults () ;
    m

let makeSignature () =
  reverseResults ();
  let s = Signature(basename (), !globalConstants, !globalKinds,
                    !globalTypeAbbrevs, !fixityList, !accumulatedSigList) in
    clearResults ();
    s

%}


%token MODULE END IMPORT ACCUMULATE ACCUMSIG USESIG LOCAL
%token LOCALKIND CLOSED SIG KIND TYPE TYPEABBREV EXPORTDEF
%token USEONLY INFIXL INFIX INFIXR PREFIX PREFIXR
%token POSTFIX POSTFIXL LAMBDA FORALL FORSOME COLONDASH
%token IMPLIES INFIXLAMBDA TYARROW CUT PI SIGMA COMMA
%token SEMICOLON AMPAND RDIVIDE NILLIST LISTCONS EQUAL
%token PLUS MINUS TIMES LESS LEQ GTR GEQ UMINUS PERIOD
%token LPAREN RPAREN LBRACK RBRACK COLON VBAR
%token SIGSTART MODSTART TERMSTART

%token <(string * Preabsyn.pidkind)> ID SYID VID UPCID
%token <string> STRLIT
%token <int> INTLIT
%token <float> REALLIT

%type <Preabsyn.pmodule> parseModule parseSignature
%type <Preabsyn.pterm> parseModClause
%type <unit> modheader sigheader
%type <unit> modend sigend modpreamble modbody
%type <unit> modsigndecl signdecls signdecl
%type <Preabsyn.psymbol list> idlist cvidlist
%type <int> kind
%type <Preabsyn.ptype> type ctype prtype
%type <Preabsyn.pfixitykind> fixity
%type <Preabsyn.pterm list> term
%type <Preabsyn.pterm> abstterm atomterm constvar
%type <Preabsyn.pterm> sigmaid piid nilid consid equalid

%type <(string * Preabsyn.pidkind)> tok
%type <(Symbol.symbol * Preabsyn.ptype option * Preabsyn.pidkind * pos)>
      typedid sanstypedid

%nonassoc  INFIXLAMBDA LAMBDA FORALL FORSOME
%nonassoc  COLONDASH IMPLIES CUT PI SIGMA COMMA SEMICOLON AMPAND RDIVIDE
           NILLIST LISTCONS EQUAL PLUS MINUS TIMES LESS LEQ GTR GEQ UMINUS
           LPAREN LBRACK ID SYID VID UPCID STRLIT INTLIT REALLIT

%start parseModule parseSignature parseModClause

%%
parseModule:
  | modheader modpreamble modbody modend        { makeModule () }
  | error modpreamble modbody modend            { makeModule () }

parseSignature:
  | sigheader sigpreamble signdecls sigend      { makeSignature () }
  | error signdecls sigend                      { makeSignature () }

tok:
  | ID       { $1 }
  | UPCID    { $1 }
  | SYID     { $1 }
  | VID      { $1 }

modheader:
  | MODULE tok PERIOD
      { if getIDName $2 <> basename () then
          Errormsg.error (getPos 1)
            ("Expected module name '" ^ basename () ^ "'.") }

sigheader:
  | SIG tok PERIOD
      { if getIDName $2 <> basename () then
          Errormsg.error (getPos 1)
            ("Expected signature name '" ^ basename () ^ "'.") }

modend:
  |       {  }
  | END   {  }


sigend:
  |       {  }
  | END   {  }

modpreamble:
  |   {  }

  | modpreamble IMPORT  cvidlist  PERIOD
      { importedModList := $3 @ !importedModList }

  | modpreamble ACCUMULATE cvidlist PERIOD
      { accumulatedModList := $3 @ !accumulatedModList }

  | modpreamble ACCUMSIG cvidlist PERIOD
      { accumulatedSigList := $3 @ !accumulatedSigList }

  | modpreamble USESIG cvidlist PERIOD
      { useSigList := $3 @ !useSigList }

sigpreamble:
  |   {  }

  | sigpreamble ACCUMSIG cvidlist PERIOD
      { accumulatedSigList := $3 @ !accumulatedSigList }

cvidlist:
  | tok                       { (makeSymbol (getPos 1) $1) :: [] }
  | cvidlist COMMA ID         { (makeSymbol (getPos 1) $3) :: $1 }
  | cvidlist COMMA UPCID      { (makeSymbol (getPos 1) $3) :: $1 }
  | cvidlist COMMA SYID       { (makeSymbol (getPos 1) $3) :: $1 }


idlist:
  | ID                        { (makeSymbol (getPos 1) $1) :: [] }
  | SYID                      { (makeSymbol (getPos 1) $1) :: [] }
  | idlist COMMA ID           { (makeSymbol (getPos 1) $3) :: $1 }
  | idlist COMMA SYID         { (makeSymbol (getPos 1) $3) :: $1 }

modbody:
  |                           {  }
  | modbody modsigndecl       {  }
  | modbody parseModClause    {  }

signdecls:
  |                           {  }
  | signdecls signdecl        {  }

signdecl:
  | KIND idlist kind PERIOD
      { globalKinds := Kind($2, Some ($3 - 1), getPos 1) :: !globalKinds }

  | TYPE idlist type PERIOD
      { globalConstants := Constant($2, Some $3, getPos 1) :: !globalConstants }

  | TYPEABBREV LPAREN tok arglist RPAREN type PERIOD
      { globalTypeAbbrevs :=
          TypeAbbrev(makeSymbol (getPos 1) $3,
                     (List.map (makeSymbol (getPos 1)) $4), $6, getPos 1) ::
            !globalTypeAbbrevs }

  | TYPEABBREV tok type PERIOD
      { globalTypeAbbrevs :=
          TypeAbbrev(makeSymbol (getPos 1) $2, [], $3, getPos 1) ::
            !globalTypeAbbrevs }

  | fixity idlist INTLIT PERIOD
      { if $3 < 0 || $3 > maxPrecedence then
          Errormsg.error (getPos 1)
            ("Precedence must be between 0 and " ^ (string_of_int $3) ^ ".")
        else
          fixityList := Fixity($2, $1, $3, getFixityPos $1) :: !fixityList }

  | EXPORTDEF idlist PERIOD
      { exportList := Constant($2, None, getPos 1) :: !exportList }

  | EXPORTDEF idlist type PERIOD
      { exportList := Constant($2, Some $3, getPos 1) :: !exportList }

  | USEONLY idlist PERIOD
      { useOnlyList := Constant($2, None, getPos 1) :: !useOnlyList }

  | USEONLY idlist type PERIOD
      { useOnlyList := Constant($2, Some $3, getPos 1) :: !useOnlyList }

modsigndecl:
  | signdecl {  }

  | LOCAL idlist PERIOD
      { localConstants := Constant($2, None, getPos 1) :: !localConstants }

  | LOCAL idlist type PERIOD
      { localConstants := Constant($2, Some $3, getPos 1) :: !localConstants }

  | LOCALKIND idlist PERIOD
      { localKinds := Kind($2, None, getPos 1) :: !localKinds }

  | LOCALKIND idlist kind PERIOD
      { localKinds := Kind($2, Some ($3 - 1), getPos 1) :: !localKinds }

  | CLOSED idlist PERIOD
      { closedConstants := Constant($2, None, getPos 1) :: !closedConstants }

  | CLOSED idlist type PERIOD
      { closedConstants := Constant($2, Some $3, getPos 1) :: !closedConstants }

kind:
  | TYPE                  { 1 }
  | kind TYARROW TYPE     { $1 + 1 }

type:
  | ctype TYARROW type    { Arrow($1, $3, getPos 1) }
  | ctype                 { $1 }

ctype:
  | prtype                { $1 }
  | ctype prtype          { App($1, $2, getPos 1) }

prtype:
  | tok                   { Atom(Symbol.symbol (getIDName $1),
                                 getIDKind $1, getPos 1) }
  | LPAREN type RPAREN    { $2 }

fixity:
  | INFIX                 { Infix(getPos 1) }
  | INFIXL                { Infixl(getPos 1) }
  | INFIXR                { Infixr(getPos 1) }
  | PREFIX                { Prefix(getPos 1) }
  | PREFIXR               { Prefixr(getPos 1) }
  | POSTFIX               { Postfix(getPos 1) }
  | POSTFIXL              { Postfixl(getPos 1) }

parseModClause:
  | term PERIOD
      { let pt = SeqTerm(List.rev $1, (getPos 1)) in
          clauseList := Clause(pt) :: !clauseList ;
          pt }

  | error PERIOD
      { Errormsg.error Errormsg.none "parsing parseModClause" ;
        ErrorTerm }

term:
  | abstterm        { $1 :: [] }
  | term abstterm   { $2 :: $1 }

abstterm:
  | typedid INFIXLAMBDA term
      { LambdaTerm(
          [TypeSymbol(getTypedIDSym $1, getTypedIDType $1,
                      getTypedIDKind $1, getTypedIDPos $1)],
          List.rev $3, getPos 1) }

  | atomterm
      { $1 }

arglist:
  | tok           { $1 :: [] }
  | arglist tok   { $2 :: $1 }

atomterm:
  | constvar
      { $1 }

  | LPAREN term RPAREN
      { SeqTerm(List.rev $2, getPos 2) }

  | LPAREN error RPAREN
      { Errormsg.error Errormsg.none "parsing atomterm"; ErrorTerm }

  | LPAREN error
      { Errormsg.error Errormsg.none "parsing atomterm"; ErrorTerm }

  | LBRACK error RBRACK
      { Errormsg.error Errormsg.none "parsing atomterm"; ErrorTerm }

  | LBRACK error
      { Errormsg.error Errormsg.none "parsing atomterm"; ErrorTerm }

  | LBRACK RBRACK
      { IdTerm(Symbol.symbol "nil", None, ConstID, getPos 1) }

  | LBRACK term RBRACK
      { ListTerm($2, getPos 1) }

  | LBRACK term VBAR term RBRACK
      { ConsTerm($2, SeqTerm(List.rev $4, getPos 1), getPos 1) }

constvar:
  | typedid
      { IdTerm(getTypedIDSym $1, getTypedIDType $1,
               getTypedIDKind $1, getTypedIDPos $1) }

  | VID
      { IdTerm(Symbol.symbol (getIDName $1), None, getIDKind $1, getPos 1) }

  | VID COLON type
      { IdTerm(Symbol.symbol (getIDName $1), Some $3, getIDKind $1, getPos 1) }

  | piid        { $1 }
  | sigmaid     { $1 }
  | nilid       { $1 }

  | consid      { $1 }
  | equalid     { $1 }

  | CUT         { IdTerm(Symbol.symbol "!", None, ConstID, getPos 1) }
  | SEMICOLON   { IdTerm(Symbol.symbol ";", None, ConstID, getPos 1) }
  | AMPAND      { IdTerm(Symbol.symbol "&", None, ConstID, getPos 1) }
  | RDIVIDE     { IdTerm(Symbol.symbol "/", None, ConstID, getPos 1) }
  | COMMA       { IdTerm(Symbol.symbol ",", None, ConstID, getPos 1) }
  | PLUS        { IdTerm(Symbol.symbol "+", None, ConstID, getPos 1) }
  | MINUS       { IdTerm(Symbol.symbol "-", None, ConstID, getPos 1) }
  | TIMES       { IdTerm(Symbol.symbol "*", None, ConstID, getPos 1) }
  | LESS        { IdTerm(Symbol.symbol "<", None, ConstID, getPos 1) }
  | LEQ         { IdTerm(Symbol.symbol "<=", None, ConstID, getPos 1) }
  | GTR         { IdTerm(Symbol.symbol ">", None, ConstID, getPos 1) }
  | GEQ         { IdTerm(Symbol.symbol ">=", None, ConstID,  getPos 1) }
  | UMINUS      { IdTerm(Symbol.symbol "-", None, ConstID, getPos 1) }
  | REALLIT     { RealTerm($1, getPos 1) }
  | INTLIT      { IntTerm($1, getPos 1) }
  | STRLIT      { StringTerm($1, getPos 1) }
  | COLONDASH   { IdTerm(Symbol.symbol ":-", None, ConstID, getPos 1) }
  | IMPLIES     { IdTerm(Symbol.symbol "=>", None, ConstID, getPos 1) }

piid:
  | PI
      { IdTerm(Symbol.symbol "pi", None, ConstID, getPos 1) }

  | PI COLON type
      { IdTerm(Symbol.symbol "pi", Some $3, ConstID, getPos 1) }

  | LPAREN piid RPAREN
      { $2 }

sigmaid:
  | SIGMA
      { IdTerm(Symbol.symbol "sigma", None, ConstID, getPos 1) }

  | SIGMA COLON type
      { IdTerm(Symbol.symbol "sigma", Some $3, ConstID, getPos 1) }

  | LPAREN sigmaid RPAREN
      { $2 }


nilid:
  | NILLIST
      { IdTerm(Symbol.symbol "nil", None, ConstID, getPos 1) }

  | NILLIST COLON type
      { IdTerm(Symbol.symbol "nil", Some $3, ConstID, getPos 1) }

  | LPAREN nilid RPAREN
      { $2 }

consid:
  | LISTCONS
      { IdTerm(Symbol.symbol "::", None, ConstID, getPos 1) }

  | LISTCONS COLON type
      { IdTerm(Symbol.symbol "::", Some $3, ConstID, getPos 1) }

  | LPAREN consid RPAREN
      { $2 }

equalid:
  | EQUAL
      { IdTerm(Symbol.symbol "=", None, ConstID, getPos 1) }

  | EQUAL COLON type
      { IdTerm(Symbol.symbol "=", Some $3, ConstID, getPos 1) }

  | LPAREN equalid RPAREN
      { $2 }

typedid:
  | sanstypedid              { $1 }
  | LPAREN typedid RPAREN    { $2 }

sanstypedid:
  | ID                { (Symbol.symbol (getIDName $1), None,
                         getIDKind $1, getPos 1) }
  | UPCID             { (Symbol.symbol (getIDName $1), None,
                         getIDKind $1, getPos 1) }
  | SYID              { (Symbol.symbol (getIDName $1), None,
                         getIDKind $1, getPos 1) }
  | ID COLON type     { (Symbol.symbol (getIDName $1), Some $3,
                         getIDKind $1, getPos 1) }
  | UPCID COLON type  { (Symbol.symbol (getIDName $1), Some $3,
                         getIDKind $1, getPos 1) }
  | SYID COLON type   { (Symbol.symbol (getIDName $1), Some $3,
                         getIDKind $1, getPos 1) }

