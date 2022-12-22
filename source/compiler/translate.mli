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
*Translate:
**********************************************************************)

(**********************************************************************
*translate:
* Given a preabsyn module and a preabsyn signature, translates the
* kind and constant declarations in the module and signature and
* produces absyn module and signature representations containing the
* kind and constant tables.
**********************************************************************)
val translate : Preabsyn.pmodule -> Preabsyn.pmodule -> 
                (Absyn.amodule * Absyn.amodule)


(* Data structure used in rationalizeType *)             
type typeandbindings =
  TypeAndBindings of (Absyn.atype * Absyn.atype Table.symboltable)

(**********************************************************************
*translateType:
* Given a preabsyn type (used as a type annotation),
* atble containing bindings for type annotation
* and an absyn module containing kind and constant
* tables, produces an absyn representation of the type.
**********************************************************************)
val translateTypeAnnot : Preabsyn.ptype -> Absyn.atype Table.symboltable -> 
                         Absyn.amodule -> typeandbindings 
