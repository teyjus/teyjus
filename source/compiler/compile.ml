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
*
**********************************************************************)
exception Exception
open Lexing
open Lpyacc

let printPreAbsyn = ref false
let printAbsyn = ref false
let printClauses = ref false

(******************************************************************
*openFile:
* Open a file and exit if there is an error.
******************************************************************)
let openFile fname f =
  try 
    let inchannel = f fname in
    inchannel
  with Sys_error(s) -> (prerr_endline s; exit 1)

(******************************************************************
*closeFile:
* Closes a file given a closing function.
******************************************************************)
let closeFile fname f =
  f fname

let compile parse fname =
  let inchannel = openFile fname open_in in
  let lexbuf = Lexing.from_channel inchannel in
  let _ = Lplex.setFileName lexbuf fname in
  let result = parse Lplex.initial lexbuf in
  let _ = closeFile inchannel close_in in
  result

let compileModule basename =
  compile Lpyacc.parseModule (basename ^ ".mod")

let compileSignature basename =
  compile Lpyacc.parseSignature (basename ^ ".sig")

let compileString s =
  let previous = !Errormsg.anyErrors in
  let () = Errormsg.anyErrors := false in
  let lexbuf = Lexing.from_string s in
  let _ = Lplex.setFileName lexbuf "" in
  let cl = parseModClause Lplex.initial lexbuf in
  let result =
    if !Errormsg.anyErrors then
      None
    else
      Some cl
  in
  (Errormsg.anyErrors := previous || (!Errormsg.anyErrors);
  result)
  
