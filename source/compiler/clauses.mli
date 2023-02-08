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
*closeddefinition:
* Represents a closed definition. A closed definition is confined to a module, 
* whereas a non-closed definition may be extended in a future module. 
* The constant is the definition's name, the term is its clause.
**********************************************************************)
type closeddefinition = (Absyn.aconstant * Absyn.aterm)	

(**********************************************************************
*translateClauses:
* Given a preabsyn module containing preabsyn clauses, and an absyn module
* with the appropriate kind and constant tables filled in, returns an
* absyn module with the clauses fields updated, as well clauses and
* new clauses, and a list of closed definitions.
*
* Arguments:
*   a preabsyn module
*   an absyn module with local and global constant tables filled in
* Returns:
*   the absyn module, with the hidden constants list filled in
*   the translated clauses
*   the new clauses introduced during parsing
*   the closed definitions
**********************************************************************)
val translateClauses : Preabsyn.pmodule -> Absyn.amodule -> 
  (Absyn.amodule * Absyn.aterm list * Absyn.aterm list * closeddefinition list)
val linearizeClause : Absyn.aterm -> Absyn.aterm

(*********************************************************************
 *makeQueryClause
 * Given a query and the list of free variables X1 ... Xn in the query,
 * construct a clause of the form
 *   p X1 ... Xn :- {query X1 ... Xn}
 *********************************************************************)
val makeQueryClause : Absyn.aterm -> Absyn.aconstant * Absyn.aterm
                                     * (Absyn.atypesymbol list) * (Absyn.atype list)
  
(*********************************************************************
 *translateQuery:
 * Given a query clause and module,
 * normalize the query clause as above
 *********************************************************************) 
val translateQuery : Absyn.aterm -> Absyn.amodule ->
                     Absyn.amodule * (Absyn.aterm list)
                     * (Absyn.aterm list) * (closeddefinition list)
