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

(*****************************************************************************)
(* Module Processclauses:                                                    *)
(* Transform the term representations of clauses into their clauses          *)
(* representations.                                                          *)
(* 1. Type environments associated with constants occurrences are trimmed    *)
(*    according to the type skeleton optimization.                           *)
(* 2. Lambda-bound variables are transformed into de Bruijn indexes and the  *)
(*    list of binders in an abstraction term is removed.                     *)
(* 3. Other variables (type variables) are transformed into logic (type)     *)
(*    variables with a form suitable for variable annotations; their scope   *)
(*    information are collected and recorded along with clauses and goals.   *)
(* 4. Clauses defining a predicate are collected and associated with the     *)
(*    predicate name being defined.                                          *)
(* 5. String arguments are collected.                                        *)
(*****************************************************************************)
val processClauses: Absyn.amodule -> Absyn.aterm list -> Absyn.aterm list 
	-> (Absyn.aconstant * Absyn.aterm) list -> Absyn.amodule
