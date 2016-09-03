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
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LABRACK RABRACK
%token <string> VARID 
%token <string> CONSTID
%token END

%type <Lfsig.signature> parse
%type <Lfabsyn.query> parseQuery

%start parse parseQuery

%%
parseQuery
  : querybndrs type_item    {let (id,tm) = $2 in Lfabsyn.Query($1,id,tm)}
  ;

querybndrs
  : LABRACK type_item RABRACK querybndrs  {($2 :: $4)}
  |                                       {[]}
  ;

parse
  : program     {$1}
  | program END {$1}
  ;

program
  : declaration_list  {let result = Lfsig.Signature("", !declarations) in (reset (); result)}
  |                   {(reset (); Lfsig.Signature("", Symboltable.empty)}
  ;

declaration_list
  : declaration                        {()}
  | declaration declaration_list       {()}
  ;

declaration
  : TYPEC type_item obj_list  {let (id,k) = $2 in 
                                    addDeclaration id (Lfabsyn.TypeFam(id,k,Lfabsyn.Prefix, Lfabsyn.None, 0, ref $3, get_pos 1))}
  ;

obj_list
  : obj                                {[ref $1]}
  | obj obj_list                       {(ref $1 :: $2)}
  ;

obj
  : TERMC id_term COLON type_tm DOT           {Lfabsyn.Object($2,$4,Lfabsyn.Prefix,Lfabsyn.None,0,get_pos 1)}
  ;

type_item
  : id_term COLON kind_tm DOT  {(Lfabsyn.IdTerm($1, getPos 1), $3)}
  ;

kind_tm
  : LBRACE VARID COLON type_tm RBRACE kind_tm    {Lfabsyn.PiKind(Lfabsyn.Var($2,getPos 2), $4, $6, getPos 1)}
  | TYPE                                         {Lfabsyn.Type(getPos 1)}
  ;

application_term
  : id_term tm_list             {Lfabsyn.AppTerm($1, $2, getPos 1)}
  | id_term                     {Lfabsyn.IdTerm($1, getPos 1)}
  ;

prefix_term
  : LBRACK id_term COLON type_tm RBRACK term  {Lfabsyn.AbsTerm($2, $4, $6, getPos 1)}
  | application_term
  ;

term
  : prefix_term
  ;

id_term
  : VARID                              {Lfabsyn.Var($1, getPos 1)}
  | CONSTID                            {Lfabsyn.Const($1, getPos 1)}
  ;

tm_list
  : term tm_list                    {($1 :: $2)}
  | term                            {[$1]}
  ;

application_type
  : id_term tm_list             {Lfabsyn.AppType($1, $2, getPos 1)}
  | id_term                     {Lfabsyn.IdType($1, getPos 1)}
  ;

rarrow_type
  : rarrow_type REVERSE_ARROW application_type  {Lfabsyn.ImpType($3, $1, getPos 2)}
  | application_type                            {$1}
  ;
 
arrow_type
  : rarrow_type ARROW arrow_type          {Lfabsyn.ImpType($1, $3, getPos 2)}
  | rarrow_type                           {$1}
  ;

prefix_type
  : LBRACE id_term COLON type_tm RBRACE prefix_type  {Lfabsyn.PiType($2, $4, $6, getPos 1)}
  | arrow_type                              {$1}
  ;

type_tm
  : prefix_type              {$1}
  ;

%%
