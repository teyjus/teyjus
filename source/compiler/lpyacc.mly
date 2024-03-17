%{
(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Nathan Guermond, Steven Holte, 
*  Gopalan Nadathur, Xiaochu Qi, Zach Snow
****************************************************************************)
(****************************************************************************
* This file is part of Teyjus.
*
* Teyjus is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Teyjus is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************)

(****************************************************************************
*lpyacc.mly:
* This file contains the Teyjus ocamlyacc specification.  The semantic
* actions are more or less correct...
****************************************************************************)

(****************************************************************************
* Shift/Reduce Conflicts
*   This grammar has 6 shift/reduce conflicts, which can be examined by
*   running ocamlyacc with the -v flag. These conflicts comes from two
*   sources: error handling and typing. The conflicts due to error handling
*   are benign. The conflicts from typing stem from trying to parse the
*   string "A : B C". This string can be interpreted as a term A with type
*   B C or as a term A with type B where A is applied to term C. In order to
*   agree with the previous Teyjus grammar, we choose the first
*   interpretation. In terms of shift/reduce conflicts, this coincides with
*   the grammar's choice to shift rather than reduce after reading "A : B".
****************************************************************************)
open Lexing
open Preabsyn

let basename () =
  Filename.chop_extension (Filename.basename (symbol_start_pos ()).pos_fname)

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
  importedModList := List.rev !importedModList;
  accumulatedModList := List.rev !accumulatedModList;
  accumulatedSigList := List.rev !accumulatedSigList;
  useSigList := List.rev !useSigList;
  clauseList := List.rev !clauseList;

  globalConstants := !globalConstants;
  closedConstants := !closedConstants;
  localConstants := !localConstants;
  useOnlyList := !useOnlyList;
  exportList := !exportList;
  
  globalKinds := !globalKinds;
  localKinds := !localKinds;

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

let genericError str =
  Errormsg.error (getPos 1) ("Unexpected input while parsing " ^ str)

let makeSymbol (name, kind) =
  Symbol(Symbol.symbol name, kind, getPos 1)

let makeTuple ?ty (name, kind) =
  (Symbol.symbol name, ty, kind, getPos 1)
      
let makeConst ?ty sym =
  IdTerm(Symbol.symbol sym, ty, ConstID, getPos 1)

let makeAbsSymbol (sym, typ_opt, _, pos) = 
  AbstractedSymbol(sym, typ_opt, pos) 
      
let makeModule () =
  reverseResults () ;
  let m = Module(basename (), !globalConstants, !localConstants,
                 !closedConstants, !useOnlyList, !exportList, !fixityList,
                 !globalKinds, !localKinds, !globalTypeAbbrevs,
                 !clauseList, !accumulatedModList, !accumulatedSigList,
                 !useSigList, !importedModList) in
    clearResults () ;
    m

let makeSignature () =
  reverseResults ();
  let s = Signature(basename (), !globalConstants, !useOnlyList, !exportList,
                    !globalKinds, !globalTypeAbbrevs, !fixityList,
                    !accumulatedSigList, !useSigList) in
    clearResults ();
    s

(********************************************************************
*errorEof:
* Prints an error occuring before the end of the file, 
* along with the line and character position.
********************************************************************)
let errorEof pos msg =
    Errormsg.error {pos with pos_lnum = pos.pos_lnum - 1} msg

%}


%token MODULE END IMPORT ACCUMULATE ACCUMSIG USESIG LOCAL
%token LOCALKIND CLOSED SIG KIND TYPE TYPEABBREV EXPORTDEF
%token USEONLY INFIXL INFIX INFIXR PREFIX PREFIXR
%token POSTFIX POSTFIXL COLONDASH
%token IMPLIES INFIXLAMBDA TYARROW CUT PI SIGMA COMMA
%token SEMICOLON AMPAND RDIVIDE NILLIST LISTCONS EQUAL
%token PLUS MINUS TIMES LESS LEQ GTR GEQ UMINUS PERIOD
%token LPAREN RPAREN LBRACK RBRACK COLON VBAR
%token EOF

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
%type <Preabsyn.pterm> abstterm atomterm constvar typedconst

%type <(string * Preabsyn.pidkind)> tok
%type <(Symbol.symbol * Preabsyn.ptype option * Preabsyn.pidkind * pos)>
      typedid

/* Lower precedence */

%nonassoc  INFIXLAMBDA LAMBDA FORALL FORSOME
%nonassoc  COLONDASH IMPLIES CUT PI SIGMA COMMA SEMICOLON AMPAND RDIVIDE
           NILLIST LISTCONS EQUAL PLUS MINUS TIMES LESS LEQ GTR GEQ UMINUS
           LPAREN LBRACK ID SYID VID UPCID STRLIT INTLIT REALLIT

/* Higher precedence */

%start parseModule parseSignature parseModClause

%%
parseModule:
  | modheader modpreamble modbody modend        { makeModule () }
  | error                                       { genericError "module" ;
                                                  makeModule () }
parseSignature:
  | sigheader sigpreamble signdecls sigend      { makeSignature () }
  | error                                       { genericError "signature" ;
                                                  makeSignature () }

tok:
  | ID                        { $1 }
  | UPCID                     { $1 }
  | SYID                      { $1 }
  | VID                       { $1 }

modheader:
  | MODULE tok PERIOD
      { if getIDName $2 <> basename () then
          Errormsg.error (getPos 2)
            ("Expected module name '" ^ basename () ^
            "', found module name '" ^ (getIDName $2) ^ "'.") }

sigheader:
  | SIG tok PERIOD
      { if getIDName $2 <> basename () then
          Errormsg.error (getPos 2)
            ("Expected signature name '" ^ basename () ^ "'.") }

modend:
  |                           {  }
  | END                       {  }
  | EOF                       {  }

sigend:
  |                           {  }
  | END                       {  }
  | EOF                       {  }

modpreamble:
  |                           {  }

  | modpreamble IMPORT cvidlist PERIOD
      { importedModList := $3 @ !importedModList }

  | modpreamble ACCUMULATE cvidlist PERIOD
      { accumulatedModList := $3 @ !accumulatedModList }

  | modpreamble ACCUMSIG cvidlist PERIOD
      { accumulatedSigList := $3 @ !accumulatedSigList }

  | modpreamble USESIG cvidlist PERIOD
      { useSigList := $3 @ !useSigList }

sigpreamble:
  |                           {  }

  | sigpreamble USESIG cvidlist PERIOD
      { useSigList := $3 @ !useSigList }

  | sigpreamble ACCUMSIG cvidlist PERIOD
      { accumulatedSigList := $3 @ !accumulatedSigList }

cvidlist:
  | tok                       { (makeSymbol $1) :: [] }
  | cvidlist COMMA ID         { (makeSymbol $3) :: $1 }
  | cvidlist COMMA UPCID      { (makeSymbol $3) :: $1 }
  | cvidlist COMMA SYID       { (makeSymbol $3) :: $1 }


idlist:
  | ID                        { (makeSymbol $1) :: [] }
  | SYID                      { (makeSymbol $1) :: [] }
  | idlist COMMA ID           { (makeSymbol $3) :: $1 }
  | idlist COMMA SYID         { (makeSymbol $3) :: $1 }

modbody:
  |                           {  }
  | modbody modsigndecl       {  }
  | modbody parseModClause    {  }

signdecls:
  |                           {  }
  | signdecls signdecl        {  }

signdecl:
  | signdeclaux EOF           { Errormsg.error (getPos 1)
                                "Type declaration never terminated" }  
  | error EOF                 { errorEof (getPos 1)
                                 "Incorrect type declaration never terminated" }
  | error PERIOD              { genericError "type" }
  | signdeclaux PERIOD        {  }

signdeclaux:
  | KIND idlist kind
      { globalKinds := Kind($2, Some $3, getPos 1) :: !globalKinds }

  | TYPE idlist type
      { globalConstants := Constant($2, Some $3, getPos 1) :: !globalConstants }

  | TYPEABBREV LPAREN tok arglist RPAREN type
      { globalTypeAbbrevs :=
          TypeAbbrev(makeSymbol $3, (List.map makeSymbol $4), $6, getPos 1) ::
            !globalTypeAbbrevs }

 | TYPEABBREV LPAREN tok RPAREN type
      { globalTypeAbbrevs :=
          TypeAbbrev(makeSymbol $3, [], $5, getPos 1) :: !globalTypeAbbrevs }

  | TYPEABBREV tok type
      { globalTypeAbbrevs :=
          TypeAbbrev(makeSymbol $2, [], $3, getPos 1) :: !globalTypeAbbrevs }

  | fixity idlist INTLIT
      { if $3 < 0 || $3 > maxPrecedence then
          Errormsg.error (getPos 1)
            ("Precedence must be between 0 and " ^ (string_of_int $3) ^ ".")
        else
          fixityList := Fixity($2, $1, $3, getFixityPos $1) :: !fixityList }

  | EXPORTDEF idlist
      { exportList := Constant($2, None, getPos 1) :: !exportList }

  | EXPORTDEF idlist type
      { exportList := Constant($2, Some $3, getPos 1) :: !exportList }

  | USEONLY idlist
      { useOnlyList := Constant($2, None, getPos 1) :: !useOnlyList }

  | USEONLY idlist type
      { useOnlyList := Constant($2, Some $3, getPos 1) :: !useOnlyList }

modsigndecl:
  | modsigndeclaux EOF          {Errormsg.error (getPos 1)
                                   "Type declaration never terminated" }
  | modsigndeclaux PERIOD       {}
  | signdeclaux PERIOD {  }

modsigndeclaux:
  | LOCAL idlist 
      { localConstants := Constant($2, None, getPos 1) :: !localConstants }

  | LOCAL idlist type 
      { localConstants := Constant($2, Some $3, getPos 1) :: !localConstants }

  | LOCALKIND idlist 
      { localKinds := Kind($2, None, getPos 1) :: !localKinds }

  | LOCALKIND idlist kind 
      { localKinds := Kind($2, Some $3, getPos 1) :: !localKinds }

  | CLOSED idlist 
      { closedConstants := Constant($2, None, getPos 1) :: !closedConstants }

  | CLOSED idlist type 
      { closedConstants := Constant($2, Some $3, getPos 1) :: !closedConstants }

kind:
  | kind_arrow                { $1 }
  | paren_type                { 0 }

kind_arrow:
  | LPAREN kind_arrow RPAREN      { $2 }
  | paren_type TYARROW kind   { $3 + 1 }

paren_type:
  | TYPE                      {()}
  | LPAREN paren_type RPAREN  {()}

type:
  | ctype TYARROW type       { Arrow($1, $3, getPos 1) }
  | ctype                    { $1 }

ctype:
  | prtype                   { $1 }
  | ctype prtype             { App($1, $2, getPos 1) }

prtype:
  | tok                      { Atom(Symbol.symbol (getIDName $1),
                                    getIDKind $1, getPos 1) }
  | LPAREN type RPAREN       { $2 }

fixity:
  | INFIX                    { Infix(getPos 1) }
  | INFIXL                   { Infixl(getPos 1) }
  | INFIXR                   { Infixr(getPos 1) }
  | PREFIX                   { Prefix(getPos 1) }
  | PREFIXR                  { Prefixr(getPos 1) }
  | POSTFIX                  { Postfix(getPos 1) }
  | POSTFIXL                 { Postfixl(getPos 1) }

parseModClause:
  | term PERIOD              { let pt = SeqTerm(List.rev $1, (getPos 1)) in
                                 clauseList := Clause(pt) :: !clauseList ;
                                 pt }
  | error PERIOD             { genericError "expression" ; ErrorTerm }
      
  | term EOF                 { Errormsg.error (getPos 1)
                                 "Clause never terminated" ;
                               ErrorTerm }

  | error EOF                { errorEof (getPos 1)
                                 "Incorrect clause never terminated" ;
                               ErrorTerm }

term:
  | abstterm                 { $1 :: [] }
  | term abstterm            { $2 :: $1 }

abstterm:
  | atomterm                 { $1 }
      
  | typedid INFIXLAMBDA term
      { LambdaTerm(makeAbsSymbol $1, List.rev $3, getPos 1) }

arglist:
  | tok                      { $1 :: [] }
  | arglist tok              { $2 :: $1 }

atomterm:
  | constvar                 { $1 }
  | LPAREN term RPAREN       { SeqTerm(List.rev $2, getPos 2) }

  | LBRACK RBRACK            { makeConst "nil" }
  | LBRACK term RBRACK       { ListTerm(List.rev $2, getPos 1) }
      
  | LBRACK term VBAR term RBRACK
      { ConsTerm(List.rev $2, SeqTerm(List.rev $4, getPos 4), getPos 1) }

  | LPAREN error             { Errormsg.error (getPos 1)
                                 "Unmatched parenthesis starting here" ;
                               ErrorTerm }
  | LBRACK error             { Errormsg.error (getPos 1)
                                 "Unmatched bracket starting here" ;
                               ErrorTerm }

constvar:
  | typedid                  { IdTerm $1 }
  | typedconst               { $1 }

  | VID                      { IdTerm (makeTuple $1) }
  | VID COLON type           { IdTerm (makeTuple ~ty:$3 $1) }
      
  | CUT                      { makeConst "!" }
  | SEMICOLON                { makeConst ";" }
  | AMPAND                   { makeConst "&" }
  | RDIVIDE                  { makeConst "/" }
  | COMMA                    { makeConst "," }
  | PLUS                     { makeConst "+" }
  | MINUS                    { makeConst "-" }
  | TIMES                    { makeConst "*" }
  | LESS                     { makeConst "<" }
  | LEQ                      { makeConst "<=" }
  | GTR                      { makeConst ">" }
  | GEQ                      { makeConst ">=" }
  | UMINUS                   { makeConst "~" }
  | COLONDASH                { makeConst ":-" }
  | IMPLIES                  { makeConst "=>" }

  | REALLIT                  { RealTerm($1, getPos 1) }
  | INTLIT                   { IntTerm($1, getPos 1) }
  | STRLIT                   { StringTerm($1, getPos 1) }

typedconst:
  | PI                       { makeConst "pi" }
  | SIGMA                    { makeConst "sigma" }
  | NILLIST                  { makeConst "nil" }
  | LISTCONS                 { makeConst "::" }
  | EQUAL                    { makeConst "=" }
      
  | PI COLON type            { makeConst ~ty:$3 "pi" }
  | SIGMA COLON type         { makeConst ~ty:$3 "sigma" }
  | NILLIST COLON type       { makeConst ~ty:$3 "nil" }
  | LISTCONS COLON type      { makeConst ~ty:$3 "::" }
  | EQUAL COLON type         { makeConst ~ty:$3 "=" }

typedid:
  | ID                       { makeTuple $1 }
  | UPCID                    { makeTuple $1 }
  | SYID                     { makeTuple $1 }
      
  | ID COLON type            { makeTuple ~ty:$3 $1 }
  | UPCID COLON type         { makeTuple ~ty:$3 $1 }
  | SYID COLON type          { makeTuple ~ty:$3 $1 }
