(**********************************************************************
* Copyright 2008-2012 Zach Snow
**********************************************************************)
(**********************************************************************
* This file is part of Parinati.
*
* Parinati is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Parinati is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Parinati.  If not, see <http://www.gnu.org/licenses/>.
**********************************************************************)
exception InternalError

type pos = Lexing.position

val none : pos
val string_of_pos : pos -> string

val any_errors : bool ref

val errors_enabled : bool ref
val warnings_enabled : bool ref
val logging_enabled : bool ref
val warnings_as_errors : bool ref

val impossible : pos -> string -> 'a
val error : pos -> string -> unit
val warning : pos -> string -> unit
val log : pos -> string -> unit

val print : pos -> string -> unit
