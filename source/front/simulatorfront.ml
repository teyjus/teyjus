(** ********************************************************************* **)
(**                   solve queries                                       **)
(** ********************************************************************* **)
let solveQueries () =
  (* get command options *)
  let maxSolutions = !Parseargs.maxSolutions in
  let minSolutions = !Parseargs.minSolutions in
  let batch = !Parseargs.batch in
  (*  let quiet = !Parseargs.quiet in *)

  (* solve a query in batch mode *)
  let solveQueryBatch () =
	let rec solveQueryBatchAux numResults =
	  if (Query.solveQuery () && numResults < maxSolutions) then
		(Query.showAnswers ();
		 solveQueryBatchAux (numResults + 1))
	  else
		numResults
	in

	if (Query.queryHasVars ()) then
	  let numResults = solveQueryBatchAux 0 in
	  if numResults < minSolutions then
		(print_endline "fewer answers than expected";
		 raise Simerrors.Exit)
	  else
		()
	else (* query does not have free variables *)
	  if Query.solveQuery () then 
		if (minSolutions > 1) then 
		  (print_endline "fewer answers than expected";
		   raise Simerrors.Exit)
		else ()
	  else 
		if (minSolutions > 0) then
		  (print_endline "fewer answers than expected";
		   raise Simerrors.Exit)
		else ()
  in
		  
  (* solve one query *)
  let rec solveQuery () =
	(* get one query from the query "buffer" *)
	let query = Parseargs.popQuery () in 
	if query = "" then 0 (* no more query is left *)
	else
	  ((* parse query and create relevant structures on simulator heap *)
	   (if (Query.buildQueryTerm query (Module.getCurrentModule ())) then
		 if batch then solveQueryBatch ()
		 else Query.interactSolveQuery ()
	   else raise Simerrors.Exit (* if any problem in build query, exit *));
	   Front.simulatorReInit(false);
	   Module.initModuleContext();
	   solveQuery ()) (* solve the next query*)
  in
  solveQuery ()


(** *********************************************************************** **)
(** the main routine                                                        **)
(** *********************************************************************** **)
let main () =
  (* read command line *)
  if (Parseargs.parseArgs Parseargs.Tjsim) then
	try 
	  (* system initialization *)
	  Front.systemInit ();  
	  let modName = (!Parseargs.inputName) in
	  (* load module *)
	  Module.moduleLoad modName;
	  (* simulator initialization *)
	  Front.simulatorInit ();
	  (* install module *)
	  Module.moduleInstall modName;
	  (* open module context *)
	  Module.initModuleContext ();
	  (* solve queries *)
	  solveQueries()
	with
	  Simerrors.Query    -> 0 (* query is done *)
	| Simerrors.TopLevel -> 0 (* stop *)
	| Simerrors.Exit     -> 1 (* halt *)
	| _                  ->   (* other exceptions *)
		(print_endline "Uncaught internal exception"; 2)
  else 1 (* error in obtaining command line arguments *)
	

let _ = main ()
