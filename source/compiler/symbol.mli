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
*Symbol Module:
* Implements a simple module mapping strings to unique identifiers.
* Uses the standard library Hashtbl module.
* Associated a name for display with each entry, the default of which
* is the same as the mapped string.
**********************************************************************)
type symbol

(**********************************************************************
*symbol:
* Produces a new symbol from the given string.  The ID of symbols with
* a given name will be the same across calls to this function. The 
* print name is the same as the given string.
**********************************************************************)
val symbol : string -> symbol

(**********************************************************************
*symbolAlias:
* Produces a new symbol from the given string together with a given 
* name for display.  The ID of symbols with
* a given name will be the same across calls to this function. 
**********************************************************************)
val symbolAlias : string -> string -> symbol

(**********************************************************************
*name:
* Returns the name of the given symbol.
**********************************************************************)
val name : symbol -> string

(**********************************************************************
*printName:
* Returns the display name of the given symbol.
**********************************************************************)
val printName : symbol -> string

(**********************************************************************
*id:
* Returns an integer representing the given symbol.
**********************************************************************)
val id : symbol -> int

(**********************************************************************
*compare:
* Compares the ids of two symbols to see if the symbols are equal.
**********************************************************************)
val equal : symbol -> symbol -> bool

(**********************************************************************
*generate:
* Produces a fresh symbol.
**********************************************************************)
val generate : unit -> symbol

(**********************************************************************
*generateName:
* Produces a fresh symbol whose name is similar to the given name.
**********************************************************************)
val generateName : string -> symbol
