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
(**********************************************************************
*Explicify:
**********************************************************************)


val print_aclausesblock : Absyn.aclausesblock -> unit

val print_clause : Absyn.aclause -> unit


(**********************************************************************
*explicify:
* Given an absyn module, explicify clauses and connectives.
**********************************************************************)
val explicify : Absyn.amodule -> Absyn.aterm list -> Absyn.aterm list->
  (Absyn.amodule * Absyn.aterm list * Absyn.aterm list) 

