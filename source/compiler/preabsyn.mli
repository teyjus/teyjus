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
(**********************************************************************
*Preabsyn Module:
* The prebstract syntax for Teyjus.
**********************************************************************)
type symbol = Symbol.symbol
type pos = Errormsg.pos

(* Kinds of Identifiers 
 * CVID and VarID are only distinguished in the preabstract syntax.
 * There are some subtle differences:
 *   - VarID cannot be bound
 *   - VarID cannot appear in type abbreviations
 * After the function translateID in parse.ml they merged to the same datatype*)
type pidkind =
  | CVID        (* Free variable (or bound variable), 
                   starting with an uppercase letter *)
  | ConstID     (* A constant (or bound variable), lowercase *)
  | AVID        (* Anonymous variable i.e. an underscore *)
  | VarID       (* Free variable, starting with an underscore *)

(* Symbols 
* A symbol can represent the name of a term, type, module, signature *)
type psymbol = Symbol of symbol * pidkind * pos

(* Types *)
type ptype =
  | Atom of symbol * pidkind * pos
  | App of ptype * ptype * pos
  | Arrow of ptype * ptype * pos
  | ErrorType

(* Symbols for abstracted variables 
* The optional type represents the possible type annotation 
* The pabstractedsymbol of the term 
* x : int \ t 
* is 
* AbstractedSymbol("x", Some(Atom(int, ConstID, _), _)) *)
type pabstractedsymbol = AbstractedSymbol of (symbol * ptype option * pos)

(* Type abbreviations 
 * For instance, 
 * typeabbrev   (bar A)   list A -> list A
 * will be represented as 
 * TypeAbbrev(bar, [Symbol(A, CVID,_)], "list A -> list A",_)
 * where "list A -> list A" is the ptype representation of list A -> list A
 * All the symbols appearing in the ptype should be present in the
 * list of psymbols *)
type ptypeabbrev = TypeAbbrev of psymbol * psymbol list * ptype * pos

(* Terms *)
type pterm =
    (* A sequence of any terms *)
  | SeqTerm of pterm list * pos 
    (* The usual prolog list notation *)
  | ListTerm of pterm list * pos
    (* ConsTerm(x,y,_) represents the prolog list notation  [x|y] *)
  | ConsTerm of pterm list * pterm * pos  
    (* LambdaTerm(x, t,_) represents x\ t 
    * The list here is just a sequence of terms. 
    * It will be translated as a single absyn term *) 
  | LambdaTerm of pabstractedsymbol * pterm list * pos
   (* An IdTerm is any identifier (constant, kind, term, ...) 
    * with an optional type denoting the possible type annotation *)
  | IdTerm of (symbol * ptype option * pidkind * pos)
  | RealTerm of float * pos
  | IntTerm of int * pos
  | StringTerm of string * pos
  | ErrorTerm

(* Every clause (terminated by a period) in the module file 
 * is encapsulated in a SeqTerm and then in a Clause. 
 * The list of all clauses is stocked in the Module datatype *)
type pclause = Clause of pterm

(* Constants 
 * There are different kind of constants. They are already classified
 * during the parsing and stored in the different list of the module datatype
 * (see below for the different kinds) 
 *
 * A declaration like:
 * type a, b, c o 
 * will be represented as
 * Constant(["a";"b";"c"], Some("o"),_)
 * where "X" is the correct translation of X.
 * The optional type is  set to None when this is a closed, exportdef or useonly
 * declaration alone (the type is declared somewhere else), e.g.
 * type p o.
 * exportdef p.
 * Otherwise this is the type of the constant defined by the user or in the 
 * pervasives *)
type pconstant = Constant of psymbol list * ptype option * pos

(* Kinds 
* There are different categories of kinds. They are already classified
 * during the parsing and stored in the different list of the module datatype
 * (see below for the different kinds) 
 *
 * The integer is the arity of the kind. E.g the kind declaration
 * kind foo type -> type
 * will be represented as Kind(_, Some 1, _) 
 * The optional integer is set to None when declaring that a kind is local
 * after its declaration, e.g.
 * kind foo type.
 * localkind foo. *)
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

(* The position used is the one of pfixitykind *)
type pfixity = Fixity of psymbol list * pfixitykind * int * pos

(********************************************************************
 * Module:
 *  This type stores information about a preabsyn module.
 * The pidkind of used, accumulated, imported modules or signatures
 * are not used. 
                                                                   
 *  Module:
 *   Name: string
 *
 *
 * Notice that constants declared in the .mod file without a keyword other than
 * type are stored in the global constants list. This is only later, 
 * at the level of absyn syntax, that the local/exportdef/useonly
 * constants list will be filled with the set of global constants/...
 * appearing in the module but not in the signature.
 *
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
      pkind list * pkind list * ptypeabbrev list * pclause list * 
      psymbol list * psymbol list * psymbol list * psymbol list
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
