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
{
open Parsing
open Lexing
open Lfparser

(**********************************************************************
*setFileName:
* Sets current filename so that position information is correct.
**********************************************************************)
let setFileName lexbuf name =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name }

(**********************************************************************
*newLine:
**********************************************************************)
let newLine lexbuf =
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with
      pos_bol = lexbuf.lex_curr_p.pos_cnum ;
      pos_lnum = 1 + lexbuf.lex_curr_p.pos_lnum }

(**********************************************************************
*currentPos:
* Returns current character position.
**********************************************************************)
let currentPos = function buf ->
  buf.lex_curr_p.pos_cnum

}

let DIGIT = ['0'-'9']
let LCASE = ['a'-'z']
let UCASE = ['A'-'Z']
let SPECIAL = ['-' '<' '>' '\\' '/' ';' '\'' '?' '+' '-' '*' '~' '!' '=' '_' '@' '$' '^' '|']
let CHAR = (LCASE|UCASE|DIGIT|SPECIAL)
let CONST = LCASE CHAR*
let VAR = UCASE CHAR*
let WSPACE = [' ' '\t' '\r']+

let LINECOMMENT = "% " [^'\n']* "\n"

rule initial = parse
  WSPACE        {initial lexbuf}
| '\n'          {newLine lexbuf; initial lexbuf}
| LINECOMMENT   {newLine lexbuf; initial lexbuf}

| "."           {DOT}
| ":"           {COLON}
| "->"          {ARROW}
| "<-"          {REVERSE_ARROW}
| "type"        {TYPE}

| "%type"      {TYPEC}
| "%term"      {TERMC}


| "{"           {LBRACE}
| "}"           {RBRACE}
| "("           {LPAREN}
| ")"           {RPAREN}
| "["           {LBRACK}
| "]"           {RBRACK}
| "<"           {LABRACK}
| ">"           {RABRACK}

| CONST as text {CONSTID text}
| VAR as text   {VARID text}
| _ as c        {print_endline
                  ("Error: invalid character '" ^ (String.make 1 c) ^ "'.");
                initial lexbuf}
| eof           {END}
