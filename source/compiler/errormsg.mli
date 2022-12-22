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
*Errormsg Module:
*	In the short run this is used simply to output meaningful error
*	messages with respect to character and line position.  In the long
*	run this will eventually allow for such things as specifying warnings
*	to ignore and the like.
**********************************************************************)
exception InternalError

type pos = Lexing.position

(*  none: indicates a lack of error information.  *)
val none : pos

(**********************************************************************
*string_of_pos:
* Produces a human-readable representation of a position.
**********************************************************************)
val string_of_pos : pos -> string

(**********************************************************************
*anyErrors:
* This flag is set to true any time an error is encountered.  It remains
* true until it is manually reset.
**********************************************************************)
val anyErrors : bool ref

(**********************************************************************
*errorsEnabled/warningsEnabled/loggingEnabled:
* These flags specify whether the various output modes are enabled.
**********************************************************************)
val errorsEnabled : bool ref
val warningsEnabled : bool ref
val loggingEnabled : bool ref

(**********************************************************************
*warningsAsErrors:
* This flag causes calls to warning to be forwarded to error.
**********************************************************************)
val warningsAsErrors : bool ref

(**********************************************************************
*info:
* Given an error string and information to add to the error string,
* produces a new error string with all necessary information.  Amounts
* to including tabs in the relevant places.
**********************************************************************)
val info : string -> string

(**********************************************************************
*log:
* Outputs logging information.  Can be enabled/disabled with the
* loggingEnabled flag.
**********************************************************************)
val log : pos -> string -> unit

(**********************************************************************
*warning:
* Outputs warning information.  Can be enabled/disabled with the
* warningEnabled flag.
**********************************************************************)
val warning : pos -> string -> unit

(**********************************************************************
*error:
* Outputs error information.  Can be enabled/disabled with the
* errorsEnabled flag.
**********************************************************************)
val error : pos -> string -> unit

(**********************************************************************
*impossible:
* Outputs internal error information.  Cannot be disabled. Raises
* InternalError.
**********************************************************************)
val impossible : pos -> string -> 'a 

(**********************************************************************
*see:
* Given a position and an information string, returns a string with
* the relevant position information included.
**********************************************************************)
val see : pos -> string -> string
