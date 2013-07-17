(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Fabien Renaud,
*  Zach Snow
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


(* explicit_term term add_sing top_level :
 * Given an aterm term, returns the term with every lambda prolog connective 
 * explicit i.e. there are no more :- , pi =>  
 * The result depends on:
 * - add_sing indicating if the resulting aterm should be a 
 * single element of a list (false for the top call in parsefront)
 * - top_level indicating if the aterm is at the top level 
 *  (true for the top call in parsefront, false in all the other cases)
 *  This helps to determine if Pervasive.implconstant should be read as
 *  :- or =>  as well as if a given predicate should be embedded in a list *)
val explicit_term : Absyn.aterm -> bool -> bool ->  Absyn.aterm 

(* Add introduced constants and modify defined types in the .sig file 
 * in the following way: * every "o" which is not in a target position is 
 * replaced by "list o" *)                                                       
val add_constants : Absyn.amodule -> Absyn.amodule

(* For the type of the constant const every "o" except the one in the target
 *  position is replaced by "list o".
 * For instance (A -> o) -> o is transformed into (A -> list o) -> o *)
val explicit_const_ty : Absyn.aconstant -> Absyn.aconstant

(* The interpreter for explicit clauses *)
val interpreter_mod : string
val interpreter_sig : string

(* The symbols we use: clause, fact, ... *)
val explicit_constants : Absyn.aconstant list
