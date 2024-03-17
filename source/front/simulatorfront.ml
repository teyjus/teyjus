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
open Parseargs

let minSolutions = ref 0
let maxSolutions = ref max_int
let quiet = ref false
let batch = ref false
let heapSize = ref 0 
let path  = ref "./"

let queryStrings = ref []

let addQuery str =
  queryStrings := !queryStrings @ [str]

let setPath p =
  path := p


let specList = multLine
  [("-p", "--path", Arg.String setPath, 
        " Add PATH to the search path.", " \n") ;
   ("-s", "--solve", Arg.String addQuery, 
        " Solve the given query on startup. Several queries may be specified", " \n") ;
   ("-e", "--expect", Arg.Set_int minSolutions,
        " Expect at least this many solutions from each query;",
        "   error if fewer. Valid only in batch mode\n") ;
   ("-m", "--maximum", Arg.Set_int maxSolutions,
        " Halt after this many solutions to the query have been found.",
        "   Valid only in batch mode\n") ;
   ("-q", "--quiet", Arg.Set quiet, 
        " Suppress all non-error output from the system,",
        "   except variable bindings on query success\n") ;
   ("-b", "--batch", Arg.Set batch,
        "   Suppress system interaction; send all output without stopping", " \n") ;
   ("-k", "--heap", Arg.Set_int heapSize,
        " Allocate a heap of the given size (in KW, W=8B on 64 bit machine)",
        " default: 512MB; min: 64KB; max: 32GB\n") ;
   versionspec]

             
let usageMsg =
  "Usage: tjsim [options] <module-name>\n" ^
    "options are:"

(***********************************************************************
*compileAndLoadQuery:
* Compile and load a query.
***********************************************************************)
let compileAndLoadQuery query =
  match (Query.compileQuery query (Module.getCurrentModule ())) with
  | Some (modname,startLoc) ->
     let _ = Ccode_stubs.loadQuery modname in
     Ccode_stubs.setQueryEntryPoint startLoc;
     true
  | None ->
     false

  
(***********************************************************************
*solveQueries:
* Main interaction loop.
***********************************************************************)
let solveQueries () =
  let solveQueryBatch () =
    let rec solveQueryBatchAux numResults =
      if Query.solveQuery () && numResults < !maxSolutions then
        (if not !quiet then Query.showAnswers ();
         solveQueryBatchAux (numResults + 1))
      else
         numResults
    in
    if Query.queryHasVars () then
      let numResults = solveQueryBatchAux 0 in
      if numResults < !minSolutions then
        Parseargs.error "fewer answers than expected"
      else ()
    else (* query does not have free variables *)
      if Query.solveQuery () then 
        if !minSolutions > 1 then 
          Parseargs.error "fewer answers than expected"
        else (if not !quiet then print_endline "\nyes\n" else ())
      else 
        if !minSolutions > 0 then
          Parseargs.error "fewer answers than expected"
        else (if not !quiet then print_endline "\nno (more) solutions\n" else ())
  in

  let rec solveQueryInteract () =
    let rec moreAnswers () =
      print_string "\nMore solutions (y/n)? ";
      match read_line () with
        | "y" -> true
        | "n" -> false
        | _ ->
            print_endline ("\nSorry, only options are `y' or `n'.\n" ^ 
                           "Let's try it again:");
            moreAnswers ()
    in
    if (Query.solveQuery ()) then
      if (Query.queryHasVars ()) then
        (Query.showAnswers ();
         if (moreAnswers ()) then
           solveQueryInteract ()
         else
           print_endline "\nyes\n")
      else
        print_endline "\nyes\n"
    else
      print_endline "\nno (more) solutions\n"
  in
  
  (* solve one query *)
  let solveQuery query =
    if (compileAndLoadQuery query) then
      (if !batch then
        solveQueryBatch ()
      else
        solveQueryInteract ())
    else
      prerr_endline "";
    Module.cleanModule (); 
    Front.simulatorReInit false ;
    Module.initModuleContext ()  
  in

  let interactSolveQuery queries modName =
    (* first solve queries input through the command line *)
    List.iter solveQuery queries;

    (* enter interactive mode *)
    while true do   
      print_string ("[" ^ modName ^"] ?- ");
      let query = read_line () in
        solveQuery query  
    done
  in

  let print_banner () =
       print_endline "Welcome to Teyjus";
       print_endline "Copyright (C) 2008 A. Gacek, N. Guermond, \
                      S. Holte, G. Nadathur, X. Qi, Z. Snow";
       print_endline "Teyjus comes with ABSOLUTELY NO WARRANTY";
       print_endline "This is free software, and you are \
                      welcome to redistribute it";
       print_endline "under certain conditions. \
                      Please view the accompanying file";
       print_endline "COPYING for more information"

  in
  if !batch then
    List.iter solveQuery !queryStrings
  else
    let modName = if !inputName = "" then "toplevel" else !inputName in
    if !queryStrings = [] then
      print_banner (); 
    interactSolveQuery !queryStrings modName

(**********************************************************************
*readTerm:
* Reads terms with respect to the current module; registered with
* the simulator.
**********************************************************************)
(* NG: No longer in use, since queries are now compiled *)
(* let readTerm term =
 *   Query.readTerm term (Module.getCurrentModule ())
 * let _ = Callback.register "ocaml_read_term" readTerm *)
  
(**********************************************************************
*main:
* Main entrypoint; sets up the simulator based on command line
* arguments, loads the secified module, and then starts the interactive loop.
**********************************************************************)
let _ =
  Arg.parse (Arg.align specList) (setInputName ~filter:getModName) usageMsg ;

  try 
        Front.systemInit  !heapSize ;
        Module.setPath    !path;
        Module.moduleLoad !inputName;
        Front.simulatorInit () ;
        Module.moduleInstall !inputName ;
        Module.initModuleContext () ;
        solveQueries()
  with
        | Simerrors.Query    -> () (* query is done *)
        | Simerrors.TopLevel -> () (* stop  *)
        | Simerrors.Exit     ->    (* halt  *)
            exit 1
        | Simerrors.Abort    ->    (* abort *)
            exit 1
        | End_of_file        ->    (* Ctrl-D *)
            print_newline ();
            exit 0
        | exp                ->    (* other exceptions *)
            print_endline "Uncaught internal exception" ;
            exit 2 
