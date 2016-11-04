%{
(**********************************************************************
*Copyright 2008, 2009 Zach Snow
**********************************************************************)
(**********************************************************************
* This file is part of Parinati.
*
* Parinati is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Parinati is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Parinati.  If not, see <http://www.gnu.org/licenses/>.
**********************************************************************)
(**********************************************************************
* Lfparser
**********************************************************************
* This module implements a rudimentary parser for LF.  It assumes
* that the module is fully explicit, and ignores many Twelf commands
* not relevant to Parinati.  It also isn't quite correct (universal
* and lambda abstraction have the wrong precedences).
**********************************************************************)
open Lexing

(**********************************************************************
*getPos:
* Gets the character position of the given token.
**********************************************************************)
let getPos i =
  Parsing.rhs_start_pos i

let declarations = ref Symboltable.empty

let addDeclaration id d = declarations := Symboltable.insert (!declarations) (Symb.symbol id) d

let reset () =
  (declarations := Symboltable.empty)

%}

%token TYPEC TERMC
%token TYPE KIND ARROW REVERSE_ARROW COLON DOT APP
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token <string> VARID 
%token <string> CONSTID
%token END

%type <Lfsig.signature> parse
%type <Lfabsyn.query> parseQuery

%start parse parseQuery

%%
parseQuery
  : querybndrs DOT VARID COLON type_tm DOT    {Lfabsyn.Query($1,Lfabsyn.LogicVar($3, $5),$5)}
  | VARID COLON type_tm DOT                   {Lfabsyn.Query([],Lfabsyn.LogicVar($1, $3),$3)}
  ;

querybndrs
  : LANGLE VARID COLON type_tm RANGLE querybndrs  {((Lfabsyn.LogicVar($2, $4), $4) :: $6)}
  |                                       {[]}
  ;

parse
  : program     {$1}
  | program END {$1}
  ;

program
  : declaration_list  {let result = Lfsig.Signature(ref [""], !declarations) in (reset (); result)}
  |                   {(reset (); Lfsig.Signature(ref [""], Symboltable.empty))}
  ;

declaration_list
  : declaration                        {()}
  | declaration declaration_list       {()}
  ;

typing
  : VARID COLON type_tm                   {(Lfabsyn.Var($1, $3), $3)}
  | CONSTID COLON type_tm                 {(Lfabsyn.Const($1), $3)}
  ;

kinding
  : id_term COLON kind_tm    {($1, $3)}

kind_tm
  : LBRACE VARID COLON type_tm RBRACE kind_tm    {Lfabsyn.PiKind(Lfabsyn.Var($2, $4), $4, $6)}
  | TYPE                                         {Lfabsyn.Type}
  ;

declaration
  : TYPEC kinding DOT obj_list  {let objs = $4 in
                                 let (id,k) = $2 in
                                    addDeclaration (Lfabsyn.get_id_name id) 
                                                   (Lfabsyn.TypeFam(id,k,Lfabsyn.NoFixity, Lfabsyn.None, 0, ref objs, 0))}
  | TYPEC kinding DOT           {let (id,k) = $2 in 
                                    addDeclaration (Lfabsyn.get_id_name id) 
                                                   (Lfabsyn.TypeFam(id,k,Lfabsyn.NoFixity, Lfabsyn.None, 0, ref [], 0))}
  ;

obj_list
  : obj                                {[ref $1]}
  | obj obj_list                       {(ref $1 :: $2)}
  ;

obj
  : TERMC typing DOT {let (id,t) = $2 in
                        Lfabsyn.Object(id, t, Lfabsyn.NoFixity, Lfabsyn.None, 0)}
  ;

ground_term
  : id_term                     {Lfabsyn.IdTerm($1)}
  | LPAREN term RPAREN          {$2}
  ;

application_term
  : id_term tm_list             {Lfabsyn.AppTerm($1, $2)}
  | ground_term                 {$1}              
  ;

prefix_term
  : LBRACK typing RBRACK term                 {let (id,t) = $2 in 
                                               Lfabsyn.AbsTerm(id,t,$4)}
  | application_term                          {$1}
  ;

term
  : prefix_term                        {$1}
  ;

id_term
  : CONSTID                            {Lfabsyn.Const($1)}
  | LANGLE VARID COLON type_tm RANGLE  {Lfabsyn.LogicVar($2, $4)}
  | LPAREN VARID COLON type_tm RPAREN  {Lfabsyn.Var($2, $4)}
  ;

tm_list
  : ground_term tm_list                    {($1 :: $2)}
  | ground_term                            {[$1]}
  ;

ground_type
  : id_term                     {Lfabsyn.IdType($1)}
  | LPAREN type_tm RPAREN          {$2}
  ;

application_type
  : id_term tm_list             {Lfabsyn.AppType($1, $2, getPos 1)}
  | ground_type                 {$1}
  ;

arrow_type
  : application_type ARROW arrow_type          {Lfabsyn.ImpType($1, $3)}
  | application_type                           {$1}
  ;

prefix_type
  : LBRACE typing RBRACE prefix_type        {let (id,t) = $2 in 
                                             Lfabsyn.PiType(id, t, $4)}
  | arrow_type                              {$1}
  ;

type_tm
  : prefix_type              {$1}
  ;


%%
