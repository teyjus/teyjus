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

type symbol = Symbol.symbol
type pos = Errormsg.pos

type pidkind =
  | CVID        
  | ConstID     
  | AVID        
  | VarID       

type psymbol = Symbol of symbol * pidkind * pos

type ptype =
  | Atom of symbol * pidkind * pos
  | App of ptype * ptype * pos
  | Arrow of ptype * ptype * pos
  | ErrorType

type pabstractedsymbol = AbstractedSymbol of (symbol * ptype option * pos)

type ptypeabbrev = TypeAbbrev of psymbol * psymbol list * ptype * pos

type pterm =
  | SeqTerm of pterm list * pos 
  | ListTerm of pterm list * pos
  | ConsTerm of pterm list * pterm * pos  
  | LambdaTerm of pabstractedsymbol * pterm list * pos
  | IdTerm of (symbol * ptype option * pidkind * pos)
  | RealTerm of float * pos
  | IntTerm of int * pos
  | StringTerm of string * pos
  | ErrorTerm

type pclause = Clause of pterm

type pconstant = Constant of psymbol list * ptype option * pos

type pkind = Kind of psymbol list * int option * pos

type pfixitykind =
  | Infix of pos
  | Infixl of pos
  | Infixr of pos
  | Prefix of pos
  | Prefixr of pos
  | Postfix of pos
  | Postfixl of pos

type pfixity = Fixity of psymbol list * pfixitykind * int * pos

type pmodule =
  | Module of string * pconstant list * pconstant list * 
      pconstant list * pconstant list * pconstant list * pfixity list *
      pkind list * pkind list * ptypeabbrev list * pclause list * 
      psymbol list * psymbol list * psymbol list * psymbol list
  | Signature of string * pconstant list * pconstant list *
      pconstant list * pkind list *
      ptypeabbrev list * pfixity list * psymbol list * psymbol list


let string_of_pos pos = Errormsg.string_of_pos pos

let map_with_commas f list = String.concat ", " (List.map f list)
  
let rec string_of_termlist list =
  map_with_commas string_of_term list

and string_of_abstractedsymbol = function
  | AbstractedSymbol(tsym, Some t, pos) ->
      "AbstractedSymbol(" ^ (Symbol.name tsym) ^ ", " ^
        (string_of_type t) ^ ", " ^ (string_of_pos pos) ^ ")"
  | AbstractedSymbol(tsym, None, pos) ->
      "AbstractedSymbol(" ^ (Symbol.name tsym) ^ ", " ^ 
      (string_of_pos pos) ^ ")"

and string_of_term = function
  | SeqTerm(tlist, pos) ->
      "SeqTerm([" ^ (string_of_termlist tlist) ^ "], " ^
        (string_of_pos pos) ^ ")"
  | ListTerm(tlist, pos) ->
      "ListTerm([" ^ (string_of_termlist tlist) ^ "], " ^
        (string_of_pos pos) ^ ")"
  | ConsTerm(tlist, t, pos) ->
      "ConsTerm([" ^ (string_of_termlist tlist) ^ "], " ^
        (string_of_term t) ^ ", " ^ (string_of_pos pos) ^ ")"
  | IdTerm(sym, None, k, pos) ->
      "IdTerm(" ^ (Symbol.name sym) ^ ", " ^ (string_of_idkind k) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | IdTerm(sym, Some t, k, pos) ->
      "IdTerm(" ^ (Symbol.name sym) ^ ", " ^ (string_of_type t) ^ ", " ^
        (string_of_idkind k) ^ ", " ^ (string_of_pos pos) ^ ")"
  | RealTerm(r, pos) ->
      "RealTerm(" ^ (string_of_float r) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | IntTerm(i, pos) ->
      "IntTerm(" ^ (string_of_int i) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | StringTerm(s, pos) ->
      "StringTerm(" ^ s ^ ", " ^ (string_of_pos pos) ^ ")"
  | LambdaTerm(lt, t, pos) ->
      "LambdaTerm([" ^ (string_of_abstractedsymbol lt) ^ "], " ^
        (string_of_termlist t) ^ ", " ^ (string_of_pos pos) ^ ")"
  | ErrorTerm ->
      "Error"
        
and string_of_idkind = function
  | CVID -> "CVID"
  | ConstID -> "ConstID"
  | AVID -> "AVID"
  | VarID -> "VarID"

and string_of_type = function
  | Atom(sym, k, pos) ->
      "Atom(" ^ (Symbol.name sym) ^ ", " ^ (string_of_idkind k) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | App(t1, t2, pos) ->
      "App(" ^ (string_of_type t1) ^ ", " ^ (string_of_type t2) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | Arrow(t1, t2, pos) ->
      "Arrow(" ^ (string_of_type t1) ^ ", " ^ (string_of_type t2) ^ ", " ^
         (string_of_pos pos) ^ ")"
  | ErrorType ->
      "Error"
        
let string_of_symbol = function
  | Symbol(sym, k, pos) ->
      "Symbol(" ^ (Symbol.name sym) ^ ", " ^ (string_of_idkind k) ^ ", " ^
        (string_of_pos pos) ^ ")"

let string_of_symbollist list =
  map_with_commas string_of_symbol list

let string_of_constant = function
  | Constant(symlist, Some t, pos) ->
      "Constant(" ^ (string_of_symbollist symlist) ^
        ", " ^ (string_of_type t) ^ ", " ^ (string_of_pos pos) ^ ")"
  | Constant(symlist, None, pos) ->
      "Constant(" ^ (string_of_symbollist symlist) ^
        ", " ^ (string_of_pos pos) ^ ")"
        
let string_of_typeabbrev = function
  | TypeAbbrev(name, arglist, ty, pos) ->
      "TypeAbbrev(" ^ (string_of_symbol name) ^
        (string_of_symbollist arglist) ^ ", " ^
        (string_of_type ty) ^ ", " ^ (string_of_pos pos) ^ ")"

let string_of_fixitykind = function
  | Infix(pos) -> "Infix(" ^ (string_of_pos pos) ^ ") "
  | Infixl(pos) -> "Infixl(" ^ (string_of_pos pos) ^ ") "
  | Infixr(pos) -> "Infixr(" ^ (string_of_pos pos) ^ ") "
  | Prefix(pos) -> "Prefix(" ^ (string_of_pos pos) ^ ") "
  | Prefixr(pos) -> "Prefixr(" ^ (string_of_pos pos) ^ ") "
  | Postfix(pos) -> "Postfix(" ^ (string_of_pos pos) ^ ") "
  | Postfixl(pos) -> "Postfixl(" ^ (string_of_pos pos) ^ ") "

let string_of_fixity = function
  | Fixity(names, k, prec, pos) ->
      "Fixity(" ^ (string_of_symbollist names) ^
        ", " ^ (string_of_fixitykind k) ^ ", " ^ (string_of_int prec) ^ ", " ^
        (string_of_pos pos) ^ ")"
          
let string_of_kind = function
  | Kind(symlist, Some i, pos) ->
      "Kind(" ^ (string_of_symbollist symlist) ^
        ", " ^ (string_of_int i) ^ ", " ^ (string_of_pos pos) ^ ")"
  | Kind(symlist, None, pos) ->
      "Kind(" ^ (string_of_symbollist symlist) ^
        ", " ^ (string_of_pos pos) ^ ")"

let string_of_clause = function
  | Clause(ts) -> "Clause(" ^ (string_of_term ts) ^ ")"

(********************************************************************
 * printPreAbsyn:
 *  Prints all information about a preabsyn module.
 ********************************************************************)
let printPreAbsyn m out =
  (* Text output functions *)
  let output_line s = output_string out (s ^ "\n") in
  let output_list f list = List.iter (fun t -> output_line (f t)) list in
    match m with
      | Module(name, gconstants, lconstants, cconstants, uconstants,
               econstants, fixities, gkinds, lkinds, tabbrevs, clauses, 
               accummod, accumsig, usesig, impmods) ->
          output_line ("Module: " ^ name) ;
          output_line "Constants:" ;
          output_list string_of_constant gconstants ;
          output_list string_of_constant lconstants ;
          output_list string_of_constant cconstants ;
          output_line "" ;
          output_line "Kinds:" ;
          output_list string_of_kind gkinds ;
          output_line "" ;
          output_list string_of_kind lkinds ;
          output_line "Type Abbrevs:" ;
          output_list string_of_typeabbrev tabbrevs ;
          output_line "Clauses:" ;
          output_list string_of_clause clauses ;
          output_line "Fixities:" ;
          output_list string_of_fixity fixities
            
      | Signature(name, gconstants, _, _, gkinds, tabbrevs, 
                  fixities, accumsig, usig) ->
          output_line ("Signature: " ^ name) ;
          output_line "Constants:" ;
          output_list string_of_constant gconstants ;
          output_line "Kinds:" ;
          output_list string_of_kind gkinds ;
          output_line "Type Abbrevs:" ;
          output_list string_of_typeabbrev tabbrevs

let getFixityPos = function
  | Infix(pos) -> pos
  | Infixl(pos) -> pos
  | Infixr(pos) -> pos
  | Prefix(pos) -> pos
  | Prefixr(pos) -> pos
  | Postfix(pos) -> pos
  | Postfixl(pos) -> pos

let getTermPos = function
  | SeqTerm(_, pos) -> pos
  | ListTerm(_, pos) -> pos
  | ConsTerm(_, _, pos) -> pos
  | IdTerm(_, _, _, pos) -> pos
  | RealTerm(_, pos) -> pos
  | IntTerm(_, pos) -> pos
  | StringTerm(_, pos) -> pos
  | LambdaTerm(_,_,pos) -> pos
  | ErrorTerm ->
      Errormsg.impossible Errormsg.none "Preabsyn.getTermPos: invalid term"

let getClauseTerm = function
  Clause(t) -> t

let getModuleClauses = function
  | Module(_, _, _, _, _, _, _, _, _, _, clauses, _, _, _, _) -> clauses
  | _ ->
      Errormsg.impossible Errormsg.none
        "Preabsyn.getModuleClauses: invalid module"
