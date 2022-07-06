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

let abortOnError () =
  if !Errormsg.anyErrors then
    exit 1

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
(*    compile query term                                                   *)
(***************************************************************************)

(* Query.compileQuery query amod
 * - (cg, fvs, ftyvars) <- Compilequery.compileQuery query amod
 * - Query.layoutQuery cg
 * - initialize local tables to keep track of free variables
 * - layout free variables on the heap
 * - set the query free variables in the simulator
 *)
let compileQuery query amod =
  let display_bytecode instrs =
    List.iter (fun instr ->
        let (instr, i) =
          try Instr.displayInstruction instr
          with _ -> ("_\t\t",0) in
        (* Problematic instrs:
         * | Ins_try_me_else(arg) -> writeopcode 118; writeI1LX arg 
         * | Ins_execute(arg) -> writeopcode 115; writeLX arg
         * Bytecode.writeintref8;
         * Bytecode.writeint1;
         * Bytecode.writeint1; *)
        print_endline (instr ^ " => "^ (string_of_int i)))
      instrs
  in
    
  let compile_query term = 
    (* Create a main clause for the query *)
    let (pred,term)  = Clauses.makeQueryClause term in

    
    (* Normalize and deOrify the query *)
    let (amod, clauses, newclauses, closeddefs) = Clauses.translateQuery pred term amod in
    let _ = abortOnError () in

    (* TODO: Is there a way to eliminate reduceSkeletons & reducePredicates here?
     * It seems they are only needed because the new main predicate
     * has SkeletonNeededness and ConstantNeededness uninitialized...
     *)
    
    (* This is necessary otherwise we get a SkeletonNeedednesss 
     * undefined error in Codegen *)
    let amod = Typereduction.reduceSkeletons amod in
    let _ = abortOnError () in
    
    (* For some reason reducing predicates makes things work because
     * it calls makeConstantNeededness on all entries in the constant table *) 
    let amod = Typereduction.reducePredicates amod in
    let _ = abortOnError () in
    
    (* Translate Absyn.aterm clauses to Absyn.aclause *)
    let amod = Processclauses.processClauses amod clauses newclauses closeddefs in
    let _ = abortOnError () in

    (* Analyze variables (register allocation magic) *)
    let () = Annvariables.processClauses amod in
    let _ = abortOnError () in

    (* Construct a codegen module.
     * The only things we need from the generated module are:
     * 1. generated code + code size
     * 2. implication table
     * 3. string table
     * 4. hidden constants
     * everything else we can toss out since no new
     * symbols is added to the symbol tables.
     * However we must keep track of the location of the main clause.
     *)
    let cg = Codegen.generateModuleCode amod in
    let cg_name = Codegen.getCGModuleName cg in
    let cg_gconsts = Codegen.getCGModuleGlobalConstants cg in
    let cg_lconsts = Codegen.getCGModuleLocalConstants cg in
    let cg_hconsts = Codegen.getCGModuleHiddenConstants cg in
    let cg_strs = Codegen.getCGModuleStrings cg in
    let cg_impgoals = Codegen.getCGModuleImpGoalList cg in
    let cg_instrs = Codegen.getCGModuleInstructions cg in
    let Codegen.Instructions(instrs, codeSize) = cg_instrs in
    let _ = abortOnError () in

    if not (Codegen.empty_cgconsts cg_gconsts) then
      Errormsg.impossible 0 "Global constants in query!";
    if not (Codegen.empty_cgconsts cg_lconsts) then
      Errormsg.impossible 0 "Local constants in query!";
    abortOnError();

    (* TODO: See Unix.mkfifo *)
    (* write output to temporary file for processing by C code *)
    let _ = Bytecode.setWordSize () in
    let _ = Bytecode.openOutChannel (cg_name ^ ".tmp") in
    (* We need code size *)
    let _ = Spitcode.writeHeader cg_name codeSize in
    (* We need hidden consts *)
    let _ = writeConstInfo cg_gconsts cg_lconsts cg_hconsts in
    (* We need strings *)
    let _ = writeStrings cg_strs in
    (* Do we need Hash & BV tables? *)
    let _ = Spitcode.writeImpGoalInfo cg_impgoals in
    let _ = Spitcode.writeInstructions instrs in
    let _ = abortOnError () in

    display_bytecode instrs;
    
    flush stdout;
    Some ()
  in
  Option.isSome @@
    (* Parse query to pre-abstract syntax *)
    let preTerm_opt = Compile.compileString query in
    let preTerm = Option.get preTerm_opt in
    (* Translate pre-abstract syntax to abstract syntax *)
    (* let* (term, tymol, fvars, tyfvars) = Parse.translateTermTopLevel preTerm amod in *)
    let term_opt = Parse.translateClause preTerm amod in
    let term = Option.get term_opt in
    let _ = abortOnError () in
    (compile_query term)

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
	
