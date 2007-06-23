(***************************************************************************)
(* compile                                                                 *)
(***************************************************************************)
let compile basename outfile =
  (*  Parse the input module and signature and generate preabsyn. *)
  let modresult = Compile.compileModule basename in
  if !Errormsg.anyErrors then 1
  else 
	let sigresult = Compile.compileSignature basename in
	if !Errormsg.anyErrors then 1
	else 

	  (*  Construct an absyn module.  At this point only the module's *)
	  (*  constant, kind, and type abbrev information is valid.       *)
	  let (absyn, sigabsyn) = (Translate.translate modresult sigresult) in
	  if !Errormsg.anyErrors then 1
	  else

		(*  Get the list of clauses and new clauses.  *)
		let (absyn, clauses, newclauses) = 
		  Clauses.translateClauses modresult absyn 
		in
		if !Errormsg.anyErrors then 1
		else

		  (* reduce skeleton *)
		  let absyn = Typereduction.reduceSkeletons absyn in
		  (*print_string "passed: reduce skeletons\n"; *)
		  if !Errormsg.anyErrors then 1
		  else 
  
			(*  Process the clauses.  *)
			let absyn = 
			  Processclauses.processClauses absyn clauses newclauses 
			in
			(*print_string "passed: processClauses\n"; *)
			if !Errormsg.anyErrors then 1
			else
 
			  (* reduce predicate types *)
			  (*let absyn = Typereduction.reducePredicates absyn in
			  (*print_string "passed: reduce predicates\n";*)			  
			  if !Errormsg.anyErrors then 1
			  else *)
				
			    (*  Process the clauses.  *)
				let _ = Annvariables.processClauses absyn in
				print_string "passed: annvariable\n";
				if !Errormsg.anyErrors then 1
				else
				  
				  (*  Construct a codegen module. *)
				  let cg = Codegen.generateModuleCode absyn in
				  print_string "passed: codegen\n";
				  if !Errormsg.anyErrors then 1
				  else
					
					(*  Open the correct output file. *)
					let _ = Bytecode.openOutChannel outfile in
					if !Errormsg.anyErrors then	1
					else
					    (Bytecode.setWordSize ();
						 (*  Write the code to the output file.  *)
						 let _ = Spitcode.writeByteCode cg in
						 if !Errormsg.anyErrors then 1
						 else 0)
					  
			  
			  


let main () =
  (******************************************************************)
  (*getOutfile:                                                     *)
  (* Gets the outfile filename.  Defaults to the input filename with*)
  (* ".lp" appended.                                                *)
  (******************************************************************)
  let getOutFile modname =
    if !Parseargs.outputFileName = "" then
      Bytecode.makeByteCodeFileName modname
    else
      !Parseargs.outputFileName
  in
  
  if (Parseargs.parseArgs Parseargs.Tjcc) then
	let modName = !(Parseargs.inputName) in
	if (modName = "") then 
	  if (!Parseargs.outputFileName = "") then 1
	  else
		(print_endline "Error: No input file specified.";
		 0)
	else
	  let outfile = getOutFile modName in
	  let result = compile modName outfile in
	  if !Errormsg.anyErrors then 
		(print_endline "Compilation failed.";
		 result)
	  else
		(print_endline "Compilation succeeded.";
		 result)
  else 1
		
(*  Execute main  *)
let _ = main ()	  
