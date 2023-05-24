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

let abortOnError u =
  if !Errormsg.anyErrors then
    None
  else
    Some u


(***************************************************************************)
(*   parse a query and create relevant structures onto simulator heap      *)
(***************************************************************************)

(* NG: No longer in use, since queries are now compiled *)
(* let buildQueryTerm query amod =
 *   (\* parse the query to pre abstract syntax *\)
 *   let preTerm = Compile.compileString query in
 *   if Option.isNone preTerm then
 *     false
 *   else
 *   let preTerm = Option.get preTerm in
 * 	(\* parse to abstract syntax *\)
 * 	let result = Parse.translateTermTopLevel preTerm amod in
 * 	if Option.isNone result then
 * 	  false
 * 	else
 * 	  let (term, tymol, fvars, tyfvars) = Option.get result in
 * 	  (\* check whether the query has boolean type *\)
 * 	  let ty = Types.getMoleculeType tymol in
 *         match ty with
 *           | Absyn.ApplicationType(k, _) when (Pervasive.iskbool k) ->
 *               (\* create the term and type onto simulator heap top *\)
 *               (Ccode_stubs.setTypeAndTermLocation ();
 *                Readterm.readTermAndType term tymol fvars tyfvars;
 *                true)
 *           | Absyn.SkeletonVarType(_) ->
 *               prerr_endline ("Error: Ill-formed goal: "  ^
 *                                "uninstantiated variable as head.");
 *               false
 *           | _ ->
 *               prerr_endline
 *                  ("Error: expecting query term of type: o" ^
 *                   (Errormsg.info ("encountered term: " ^
 *                                   (Absyn.string_of_term term))) ^
 *                   (Errormsg.info ("of type: " ^
 *                                   (Types.string_of_typemolecule tymol)
 *                   )));
 *                false *)

(***************************************************************************)
(*    compile query term                                                   *)
(***************************************************************************)

let compileQuery query amod =
  (* Syntactic sugar for error handling *)
  let (let*) = Option.bind in

  (* Reset errors, if any -- since previous query may have failed *)
  Errormsg.anyErrors := false;
       
  (* Reinitialize compiler *)
  Clausegen.initTotImpPoints ();

  (* Parse query to pre-abstract syntax *)
  let* preTerm = Compile.compileString query in

  (* Translate pre-abstract syntax to abstract syntax *)
  let* term = Parse.translateClause ~parsingtoplevel:true
                preTerm amod in
  
  (** now compile query *)  
  (* Create a main clause for the query *)
  let (main_pred,query_term, fvars, tyfvars)  = Clauses.makeQueryClause term in

  (* Count the number of distinct type variables in tyfvars. 
   * This will be the number of type variable registers we need
   * to initialize. *)
  (* TODO: Move this somewhere else *)
  let ntyfvars = List.length(List.fold_right (fun a tyfvars ->
                                 Types.freeTypeVars a tyfvars) tyfvars [])
  in
  (* Normalize and deOrify the query *)
  let (amod, clauses, newclauses, closeddefs) =
    Clauses.translateQuery query_term amod
  in
  let* _ = abortOnError () in

  (* We cannot call Typereduction.reduceSkeletons or
   * Typereduction.reducePredicates here
   * because that would generate different neededness values.  
   * Neededness values are local to a module, so in an exported definition 
   * we must assume that every type variable is needed. *)
  let amod = Typereduction.initConstantAndSkeletonNeedednessTopLevel amod in

  (* Translate Absyn.aterm clauses to Absyn.aclause *)
  let amod = Processclauses.processClauses amod clauses newclauses closeddefs in
  let* _ = abortOnError () in

  (* Analyze variables (register allocation magic) *)
  let () = Annvariables.processClauses amod in
  let* _ = abortOnError () in

  Codegen.set_main_pred (Absyn.getConstantName main_pred);
  let cg = Codegen.generateModuleCode amod in
  let startLoc = Codegen.get_main_pred_loc () in
  let* _ = abortOnError() in

  (** Write output to pipe for processing by C code *)

  let _ = Bytecode.setWordSize () in
  let name = Codegen.getCGModuleName cg in
  
  let _ = Ccode_stubs.openPipe() in
  let out_chan = Unix.out_channel_of_descr(Ccode_stubs.getPipeIn()) in
  let _ = Bytecode.setOutChannel out_chan in

  let _ = Spitcode.writeQueryByteCode cg in
  let* _ = abortOnError () in

  (* Loader assumes Pipe is still open *)
  let _ = flush(out_chan) in

  (** Init free term variables onto the heap, and IO tables *)
  (* Note: since neededness values are maximal, we must pass
   * all type variables as arguments. The number of type variables
   * is the total number of free type variables.
   * For example,
   *   main A1 ... An T1 ... Tm :- {query A1 ... An}.
   * The type variables are initialized later, in 
   *   front/query_c.c:QUERY_solveQuery
   *)
  let _ = Readterm.initVariables fvars ntyfvars in
  Some (name,startLoc)
  
      
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

(* NG: No longer in use, since queries are now compiled *)
(* let readTerm term amod =
 *  
 *   (\* parse the term to pre abstract syntax *\)
 *   let preTerm = Compile.compileString term in
 *   if Option.isNone preTerm then
 *     0
 *   else
 *     let preTerm = Option.get preTerm in
 *     (\* parse to abstract syntax *\)
 *     let result = Parse.translateTermTopLevel preTerm amod in
 *     if Option.isNone result then
 *       0
 *     else
 *       let (term, tymol, fvars, tyfvars) = Option.get result in
 *       Readterm.readTermAndType term tymol fvars tyfvars;
 *       1 *)
	
