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
*ErrorMsg:
**********************************************************************)
exception InternalError
open Lexing

type pos = position

let anyErrors = ref false

let errorsEnabled = ref true
let warningsEnabled = ref true
let loggingEnabled = ref false

let warningsAsErrors = ref false

(********************************************************************
*none:
*  An empty pos, useful when printing internal compiler errors that
*  are unrelated to a particular file or location.
********************************************************************)
let none = { pos_fname = "none" ;
             pos_lnum = 0 ;
             pos_bol = 0 ;
             pos_cnum = 0 }
  
(********************************************************************
*string_of_pos:
*  Convert a pos to a string.
********************************************************************)
let string_of_pos pos =
  let file = pos.pos_fname in
  let line = pos.pos_lnum in
  let char = pos.pos_cnum - pos.pos_bol in
  file ^ "(" ^ (string_of_int line) ^ "," ^ (string_of_int char) ^ ")"

(********************************************************************
*printPosition:
*  Prints position information.
********************************************************************)
let printPosition = fun p ->
  prerr_string (string_of_pos p)

(********************************************************************
*info:
* Formats information for output using error or warning.
********************************************************************)
let info msg =
  "\n\t" ^ msg

(********************************************************************
*see:
* Annotates a message with a file location.
********************************************************************)
let see pos msg =
  (info "See " ^ msg ^ " at " ^ (string_of_pos pos))

(********************************************************************
*error:
* Prints an error, along with the line and character position.
********************************************************************)
let error pos msg =
  if !errorsEnabled then
    (anyErrors := true;
    printPosition pos;
    prerr_string " : Error : ";
    prerr_string msg;
    prerr_newline ())
  else
    ()

(********************************************************************
*warning:
* Prints a warning, along with the line and character position.
********************************************************************)
let warning pos msg =
  if !warningsEnabled && !errorsEnabled then
    (if !warningsAsErrors then
      anyErrors := true
    else
      ();
    printPosition pos;
    prerr_string " : Warning : ";
    prerr_string msg;
    prerr_newline ())
  else
    ()    

(********************************************************************
*log:
*  Outputs logging information.
********************************************************************)
let log pos msg =
  if !loggingEnabled then
    (printPosition pos;
    prerr_string " : Log : ";
    prerr_string msg;
    prerr_newline ())
  else
    ()

(********************************************************************
*impossible:
* Prints an internal compiler error, then throws an exception
* InternalError.
********************************************************************)
let impossible pos msg =
  (anyErrors := true;
  printPosition pos;
  prerr_string " : Internal Error : ";
  prerr_string msg; 
  prerr_newline ();
  raise InternalError)
