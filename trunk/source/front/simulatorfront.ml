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
  (*print_string "not implemented\n"*)
  path := p

let inputName = ref ""

let specList = dualArgs
  [("-p", "--path", Arg.String setPath, 
	" Add PATH to the search path. Several paths may be specified") ;
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

let anonFunc name =
  inputName := getModName name

let usageMsg =
  "Usage: tjsim <options> <module file>\n" ^
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
  
  (* solve one query *)
  let rec solveQuery query =
    if Query.buildQueryTerm query (Module.getCurrentModule ()) then
      if !batch then
        solveQueryBatch ()
      else
        Query.interactSolveQuery ()
    else
      Parseargs.error "" ;
    Front.simulatorReInit false ;
    Module.initModuleContext ()
      
  in
    List.iter solveQuery !queryStrings

      
let _ =
  Arg.parse (Arg.align specList) anonFunc usageMsg ;

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
	| Simerrors.TopLevel -> () (* stop *)
	| Simerrors.Exit     ->    (* halt *)
            exit 1
	| exp                ->    (* other exceptions *)
	    print_endline "Uncaught internal exception" ;
            exit 2 