open Parseargs

let minSolutions = ref 0
let maxSolutions = ref max_int
let quiet = ref false
let batch = ref false
let heapSize = ref 0 

let queryStrings = ref []

let addQuery str =
  queryStrings := !queryStrings @ [str]

let popQuery () =
  match !queryStrings with
    | [] -> ""
    | query::rest ->
        queryStrings := rest ;
        query
          
let setPath path =
  print_string "not implemented\n"

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
		  (print_endline "fewer answers than expected";
		   raise Simerrors.Exit)
        else ()
	else (* query does not have free variables *)
	  if Query.solveQuery () then 
		if !minSolutions > 1 then 
		  (print_endline "fewer answers than expected";
		   raise Simerrors.Exit)
		else ()
	  else 
		if !minSolutions > 0 then
		  (print_endline "fewer answers than expected";
		   raise Simerrors.Exit)
		else ()
  in
		  
  (* solve one query *)
  let rec solveQuery () =
	(* get one query from the query "buffer" *)
	let query = popQuery () in 
	if query = "" then 0 (* no more query is left *)
	else begin
	  (* parse query and create relevant structures on simulator heap *)
	  if Query.buildQueryTerm query (Module.getCurrentModule ()) then
		if !batch then
          solveQueryBatch ()
		else
          Query.interactSolveQuery ()
	  else
        raise Simerrors.Exit (* if any problem in build query, exit *) ;

	  Front.simulatorReInit false ;
	  Module.initModuleContext () ;
	  solveQuery () (* solve the next query *)
        
    end
  in
    solveQuery ()


let _ =
  Arg.parse (Arg.align specList) anonFunc usageMsg ;

  try 
	Front.systemInit !heapSize ;  
	Module.moduleLoad !inputName ;
	Front.simulatorInit () ;
	Module.moduleInstall !inputName ;
	Module.initModuleContext () ;
	solveQueries()
  with
	| Simerrors.Query    -> 0  (* query is done *)
	| Simerrors.TopLevel -> 0  (* stop *)
	| Simerrors.Exit     ->    (* halt *)
        exit 1
	| _                  ->    (* other exceptions *)
		print_endline "Uncaught internal exception" ;
        exit 2
