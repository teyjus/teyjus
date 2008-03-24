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
*Option:
* Just a few helper functions for dealing with option types.
**********************************************************************)

(**********************************************************************
*isSome/isNone:
* Functions to test the value of an option.
**********************************************************************)
val isNone : 'a option -> bool
val isSome : 'a option -> bool

(**********************************************************************
*get:
* Returns the value of an option if it is a Some; raises an internal
* error otherwise.
**********************************************************************)
val get : 'a option -> 'a

(**********************************************************************
*string_of_option:
* Prints the value of an option with the given print function if the
* option is a Some; otherwise, prints "None".
**********************************************************************)
val string_of_option : 'a option -> ('a -> string) -> string
