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
(***************************************************************************)
(*   parse a query and create relevant structures onto simulator heap      *)
(***************************************************************************)
let buildQueryTerm query amod =
  let isBooleanType ty = 
	match ty with
	  Absyn.ApplicationType(k, _) ->
		if Pervasive.iskbool k then true
		else false
	| _ -> false
  in

  (* parse the query to pre abstract syntax *)
  let preTerm = Compile.compileString query in
  if (!Errormsg.anyErrors) then false
  else
	(* parse to abstract syntax *)
	let (term, tymol, fvars, tyfvars) = Parse.translateTerm preTerm amod in
	if (!Errormsg.anyErrors) then false
	else
	  (* check whether the query has boolean type *)
	  let ty = Types.getMoleculeType tymol in
	  if (isBooleanType ty) then
    (* create the term and type onto simulator heap top *)
		  (Ccode_stubs.setTypeAndTermLocation ();
		   Readterm.readTermAndType term tymol fvars tyfvars;
		   true)
	  else 
		  (print_endline ("Error: query has type " ^
		    (Absyn.string_of_type ty) ^ " (expected type o).");
		   false)
	  
(***************************************************************************)
(*    invoke the simulator to solve a query                                *)
(***************************************************************************)
let solveQuery () = 
  try
	let _ = Simerrors.handleSimExceptions(Ccode_stubs.solveQuery()) in
	true (* should never be encountered *)
  with
	Simerrors.Query       -> false (* query was aborted *)
  | Simerrors.QueryResult -> true  (* query has some results *)
  | Simerrors.Fail        -> false (* query has no (more) results *)
  | exp                   -> raise exp

(***************************************************************************)
(*                       showing answers                                   *)
(***************************************************************************)
let showAnswers () = 
  let _ = Simerrors.handleSimExceptions(Ccode_stubs.showAnswers ()) in
  ()


let queryHasVars () =
  Ccode_stubs.queryHasVars ()
