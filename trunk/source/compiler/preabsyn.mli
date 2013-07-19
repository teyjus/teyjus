(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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
(**********************************************************************
*Preabsyn Module:
* The prebstract syntax for Teyjus.
**********************************************************************)
type symbol = Symbol.symbol
type pos = Errormsg.pos

(* Kinds of Identifiers 
 * CVID and VarID are only distinguished in the preabstract syntax.
 * After the function translateID in parse.ml they are both free variables *)
type pidkind =
  | CVID        (* Free variable, starting with an uppercase letter *)
  | ConstID     (* A constant, lowercase *)
  | AVID        (* Anonymous variable i.e. an underscore *)
  | VarID       (* Free variable, starting with an underscore *)

(* Symbols *)
type psymbol = Symbol of symbol * pidkind * pos

(* Types *)
type ptype =
  | Atom of symbol * pidkind * pos
  | App of ptype * ptype * pos
  | Arrow of ptype * ptype * pos
  | ErrorType

(* Type Symbols for abstracted variables 
* The optional type represents the possible type annotation *)
type ptypesymbol = TypeSymbol of (symbol * ptype option * pos)

type ptypeabbrev = TypeAbbrev of psymbol * psymbol list * ptype * pos

(* Terms *)
type pterm =
  | SeqTerm of pterm list * pos
  | ListTerm of pterm list * pos
  | ConsTerm of pterm list * pterm * pos
  | LambdaTerm of ptypesymbol * pterm list * pos
  | IdTerm of (symbol * ptype option * pidkind * pos)
  | RealTerm of float * pos
  | IntTerm of int * pos
  | StringTerm of string * pos
  | ErrorTerm

type pboundterm = BoundTerm of ptypesymbol list * pterm list


type pclause = Clause of pterm

(* Constants *)
type pconstant = Constant of psymbol list * ptype option * pos

(* Kinds *)
type pkind = Kind of psymbol list * int option * pos

(* Fixity *)
type pfixitykind =
  | Infix of pos
  | Infixl of pos
  | Infixr of pos
  | Prefix of pos
  | Prefixr of pos
  | Postfix of pos
  | Postfixl of pos

type pfixity = Fixity of psymbol list * pfixitykind * int * pos

(********************************************************************
 * Module:
 *  This type stores information about a preabsyn module.
 *  Module:
 *   Name: string
 *   Global Constants: pconstant list
 *   Local Constants: pconstant list
 *   Closed Constants: pconstant list
 *   Useonly Constants: pconstant list
 *   Exportdef Constants: pconstant list
 *
 *   Fixities: pfixity list
 * 
 *   Global Kinds: pkind list
 *   Local Kinds: pkind list
 *
 *   Type Abbreviations: ptypeabbrev list
 *
 *   Clauses: pterm list
 *
 *   Accumulated Modules: psymbol list
 *   Accumulated Signatures: psymbol list
 *   Used Signatures: psymbol list
 *   Imported Modules: psymbol list
 *
 *  Signature:
 *   Name: string
 *   Global Constants: pconstant list
 *   Useonly Constants: pconstant list
 *   Exportdef Constants: pconstant list
 *
 *   Global Kinds: pkind list
 *
 *   Type Abbreviations: ptypeabbrevlist
 *
 *   Accumulated Signatures: psymbol list
 ********************************************************************)
type pmodule =
  | Module of string * pconstant list * pconstant list * 
      pconstant list * pconstant list * pconstant list * pfixity list *
      pkind list * pkind list * ptypeabbrev list * pclause list * psymbol list *
      psymbol list * psymbol list * psymbol list
  | Signature of string * pconstant list * pconstant list *
      pconstant list * pkind list *
      ptypeabbrev list * pfixity list * psymbol list * psymbol list

val printPreAbsyn : pmodule -> out_channel -> unit

(* Accessors *)
val getFixityPos : pfixitykind -> pos
val getTermPos : pterm -> pos
val getModuleClauses : pmodule -> pclause list
val getClauseTerm : pclause -> pterm

val string_of_term : pterm -> string
val string_of_type : ptype -> string
