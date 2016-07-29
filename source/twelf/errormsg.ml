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
open Lexing

type pos = position

(**********************************************************************
* Error message options.
**********************************************************************)
let errors_enabled = ref true  (* For hacking purposes only! *)
let warnings_enabled = ref true
let logging_enabled = ref false

let warnings_as_errors = ref false

(**********************************************************************
*any_errors:
* This flag is set to true any time an error is encountered.  It remains
* true until it is manually reset.
**********************************************************************)
let any_errors = ref false

(********************************************************************
*none:
*  An empty pos, useful when printing internal compiler errors that
*  are unrelated to a particular file or location.
********************************************************************)
let none = {
  pos_fname = "none" ;
  pos_lnum = 0 ;
  pos_bol = 0 ;
  pos_cnum = 0
}

(**********************************************************************
*string_of_pos:
* Produces a human-readable representation of a position.
**********************************************************************)
let string_of_pos pos =
  if pos = none then
    ""
  else
    let file = pos.pos_fname in
    let line = pos.pos_lnum in
    let char = pos.pos_cnum - pos.pos_bol in
    file ^ "(" ^ (string_of_int line) ^ "," ^ (string_of_int char) ^ ")"

(********************************************************************
*print_position:
*  Prints position information.
********************************************************************)
let rec print_position pos msg =
  let p = string_of_pos pos in
  if p = "" && msg = "" then
    ()
  else if p = "" then
    prerr_string (msg ^ " : ")
  else if msg = "" then
    prerr_string p
  else
    prerr_string (p ^ " : " ^ msg ^ " : ")

(********************************************************************
*reset:
* Resets the error message module.
********************************************************************)
let reset () = any_errors := false

(**********************************************************************
*impossible:
* Outputs internal error information.  Cannot be disabled. Raises
* InternalError.
**********************************************************************)
let impossible pos msg =
  (any_errors := true;
  print_position pos "Internal Error";
  prerr_string msg;
  prerr_newline ();
  flush stderr;
  raise InternalError)

(**********************************************************************
*error:
* Outputs error information.  Can be enabled/disabled with the
* errorsEnabled flag.
**********************************************************************)
let error pos msg =
  if !errors_enabled then
    (any_errors := true;
    print_position pos "Error";
    prerr_string msg;
    prerr_newline ())
  else
    ()

(**********************************************************************
*warning:
* Outputs warning information.  Can be enabled/disabled with the
* warningEnabled flag.
**********************************************************************)
let warning pos msg =
  if !warnings_enabled && !errors_enabled then
    (if !warnings_as_errors then
      any_errors := true
    else
      ();
    print_position pos "Warning";
    prerr_string msg;
    prerr_newline ();
    flush stderr)
  else
    ()

(**********************************************************************
*log:
* Outputs logging information.  Can be enabled/disabled with the
* loggingEnabled flag.
**********************************************************************)
let log pos msg =
  if !logging_enabled then
    (print_position pos "Log";
    prerr_string msg;
    prerr_newline ();
    flush stderr)
  else
    ()

(**********************************************************************
*print:
* Just prints information, possibly with position.
**********************************************************************)
let print pos msg =
  (print_position pos "";
  prerr_string msg;
  prerr_newline ();
  flush stderr)
