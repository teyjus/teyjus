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

let specList = dualArgs
  [("-p", "--path", Arg.String setPath, 
        " Add PATH to the search path.") ;
   ("-s", "--solve", Arg.String addQuery, 
        " Solve the given query on startup. Several queries may be specified") ;
   ("-e", "--expect", Arg.Set_int minSolutions,
        " Expect at least this many solutions from each query;\n" ^
      "\t\terror if fewer. Valid only in batch mode") ;
   ("-m", "--maximum", Arg.Set_int maxSolutions,
        " Halt after this many solutions to the query have been found.\n" ^
      "\t\tValid only in batch mode") ;
   ("-q", "--quiet", Arg.Set quiet, 
        " Suppress all non-error output from the system,\n" ^
      "\t\texcept variable bindings on query success") ;
   ("-b", "--batch", Arg.Set batch,
        " Suppress system interaction; send all output without stopping") ;
   ("-k", "--heap", Arg.Set_int heapSize,
        " Allocate a heap of the given size (K)") ;
   versionspec]

let usageMsg =
  "Usage: tjsim <options> <module-name>\n" ^
    "options are:"

let solveQueries () =
  (* solve a query in batch mode *)
  let solveQueryBatch () =
    let rec solveQueryBatchAux numResults =
      if Query.solveQuery () && numResults < !maxSolutions then
        (Query.showAnswers ();
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
        else ()
      else 
        if !minSolutions > 0 then
          Parseargs.error "fewer answers than expected"
        else ()
  in

  let rec solveQueryInteract () =

    let rec moreAnswers () =
      print_string "\nMore solutions (y/n)? ";
      match read_line () with
        | "y" -> true
        | "n" -> false
        | _ ->
            print_endline "\nSorry, only options are `y' or `n'.\nLet's try it again:";
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
  let rec solveQuery query =
   if Query.buildQueryTerm query (Module.getCurrentModule ()) then 
      if !batch then
        solveQueryBatch ()
      else
        solveQueryInteract ()
    else
     prerr_endline "";
    Module.cleanModule (); 
    Front.simulatorReInit false ;
    Module.initModuleContext ()
      
  in

  let rec interactSolveQuery queries modName =

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
       print_endline "Copyright (C) 2008 A. Gacek, S. Holte, \
                      G. Nadathur, X. Qi, Z. Snow";
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


let readTerm term =
  Query.readTerm term (Module.getCurrentModule ())

let _ = Callback.register "ocaml_read_term" readTerm
  
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
        | exp                ->    (* other exceptions *)
            print_endline "Uncaught internal exception" ;
            exit 2 
