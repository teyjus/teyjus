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
(***************************************************************************)
(*   parse a query and create relevant structures onto simulator heap      *)
(***************************************************************************)
let buildQueryTerm query amod =
  (* parse the query to pre abstract syntax *)
  let preTerm = Compile.compileString query in
  if Option.isNone preTerm then
    false
  else
  let preTerm = Option.get preTerm in
	(* parse to abstract syntax *)
	let result = Parse.translateTermTopLevel preTerm amod in
	if Option.isNone result then
	  false
	else
	  let (term, tymol, fvars, tyfvars) = Option.get result in
	  (* check whether the query has boolean type *)
	  let ty = Types.getMoleculeType tymol in
        match ty with
          | Absyn.ApplicationType(k, _) when (Pervasive.iskbool k) ->
              (* create the term and type onto simulator heap top *)
              (Ccode_stubs.setTypeAndTermLocation ();
               Readterm.readTermAndType term tymol fvars tyfvars;
               true)
          | Absyn.SkeletonVarType(_) ->
              prerr_endline ("Error: Ill-formed goal: "  ^
                               "uninstantiated variable as head.");
              false
          | _ -> 
              prerr_endline 
                 ("Error: expecting query term of type: o" ^
                  (Errormsg.info ("encountered term: " ^ 
                                  (Absyn.string_of_term term))) ^
                  (Errormsg.info ("of type: " ^ 
                                  (Types.string_of_typemolecule tymol)
                  )));
               false
	  
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

(***************************************************************************)
(*                      read term                                          *)
(*  parse a term and create relevant structures onto simulator heap        *)
(***************************************************************************)

let readTerm term amod =
 
  (* parse the term to pre abstract syntax *)
  let preTerm = Compile.compileString term in
  if Option.isNone preTerm then
    0
  else
    let preTerm = Option.get preTerm in
    (* parse to abstract syntax *)
    let result = Parse.translateTermTopLevel preTerm amod in
    if Option.isNone result then
      0
    else
      let (term, tymol, fvars, tyfvars) = Option.get result in
      Readterm.readTermAndType term tymol fvars tyfvars;
      1
	
