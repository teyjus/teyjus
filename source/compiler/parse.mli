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
type pos = Errormsg.pos
type symbol = Symbol.symbol

(**********************************************************************
*translateTermTopLevel:
* Given a preabstract syntax term (assumed to have been parsed at the
* top-level interface), parses the term and returns an abstract syntax
* term and associated information.  The resulting term has no overloaded
* constants in it.  The resulting term has no nested abstractions in it.
* When translateTermTopLevel is called, Errormsg.anyErrors is set to false, the
* translation takes place, and Errormsg.anyErrors is once again reset.
* The result is None if any errors occurred during translation.
*                                                                      
* Arguments:
*   a preabsyn term
*   an absyn module with kind and constant tables filled in
* Returns:
*   the translated absyn term
*   a typemolecule corresponding to this term
*   a list of free variables in the term
*   a list of new free type variables in the type
*
**********************************************************************)
(* NG: No longer in use, since queries are now compiled *)
(* val translateTermTopLevel : Preabsyn.pterm -> Absyn.amodule -> *)
(*   (Absyn.aterm * Types.typemolecule * Absyn.atypesymbol list * Absyn.atype list) option *)
  
(**********************************************************************
*translateClause:
* Given a preabstract syntax term representing a clause (assumed to
* have been parsed while processing a file), parses the term and returns
* an abstract syntax term.  The resulting clause has no overloaded
* constants in it.  When translateClause is called, Errormsg.anyErrors
* is set to false, the translation takes place, and Errormsg.anyErrors
* is once again reset.  The result is None if any errors occurred
* during translation.
*
* Arguments:
*   a preabsyn term
*   an absyn module with kind and constant tables filled in
* Returns:
*   the translated absyn term
*
**********************************************************************)
val translateClause : ?parsingtoplevel:bool -> Preabsyn.pterm -> Absyn.amodule -> Absyn.aterm option

(**********************************************************************
*removeNestedAbstractions:
* Removes any nested abstractions in an absyn term, replacing them
* with "flattened" unnested abstractions.  It is assumed that the absyn
* term has no unnested abstractions in it; if it does they are not merged
* in any way.
*
* Arguments:
*   an absyn term
* Returns:
*   the unnested absyn term
*
**********************************************************************)
val removeNestedAbstractions : Absyn.aterm -> Absyn.aterm


val unitTests : unit -> unit
