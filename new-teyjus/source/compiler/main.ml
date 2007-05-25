let unitTests = ref false

(**********************************************************************
*compile:
* Compile the given module or signature, based on the file name.
**********************************************************************)
let compile = fun basename outfile ->
  (*  Parse the input module and signature and generate preabsyn. *)
  let modresult = Compile.compileModule basename in
  if !Errormsg.anyErrors then
    1
  else 
  
  let sigresult = Compile.compileSignature basename in
  if !Errormsg.anyErrors then
    1
  else 
  
  (*  Print the results (preabsyn module and sig) to the output file  *)
  (if (!Compile.printPreAbsyn) then
    let modchannel = Compile.openFile (basename ^ ".mod.preabsyn.txt") open_out in
    let sigchannel = Compile.openFile (basename ^ ".sig.preabsyn.txt") open_out in
    
    (Preabsyn.printPreAbsyn modresult modchannel;
    Preabsyn.printPreAbsyn sigresult sigchannel;
    Compile.closeFile modchannel close_out;
    Compile.closeFile sigchannel close_out)
  else
    ();

  if !Errormsg.anyErrors then
    1
  else 

  (*  Construct an absyn module.  At this point only the module's
      constant, kind, and type abbrev information is valid. *)
  let (absyn, sigabsyn) = (Translate.translate modresult sigresult) in
  if !Errormsg.anyErrors then
    1
  else
  
  (*  Get the list of clauses and new clauses.  *)
  let (absyn, clauses, newclauses) = Clauses.translateClauses modresult absyn in
  if !Errormsg.anyErrors then
    1
  else
  
  (*  Print the results (clauses) to the output file  *)
  (if !Compile.printClauses then
    let outchannel = Compile.openFile (basename ^ ".clauses.txt") open_out in
    (Clauses.printTranslatedClauses clauses newclauses outchannel;
    Compile.closeFile outchannel close_out)
  else
    ();
  if !Errormsg.anyErrors then
    1
  else

  (* reduce skeleton *)
  let absyn = Typereduction.reduceSkeletons absyn in
  print_string "passed: reduce skeletons\n";
  if !Errormsg.anyErrors then 1
  else 
  
  (*  Process the clauses.  *)
  let absyn = Processclauses.processClauses absyn clauses newclauses in
  print_string "passed: processClauses\n";
  if !Errormsg.anyErrors then
    1
  else 
  
  (* reduce predicate types *)
 (* let absyn = Typereduction.reducePredicates absyn in
  print_string "passed: reduce predicates\n";

  if !Errormsg.anyErrors then
    1
  else 
	*)

  (*  Print the results (absyn module) to the output file *)
  (if !Compile.printAbsyn then
    let modoutchannel = Compile.openFile (basename ^ ".mod.absyn.txt") open_out in
    let sigoutchannel = Compile.openFile (basename ^ ".sig.absyn.txt") open_out in
    (Absyn.printAbsyn absyn modoutchannel;
    Absyn.printAbsyn sigabsyn sigoutchannel;
    Compile.closeFile modoutchannel close_out;
    Compile.closeFile sigoutchannel close_out)
  else
    ();
  if !Errormsg.anyErrors then
    1
  else
  
  (*  Process the clauses.  *)
  let _ = Annvariables.processClauses absyn in
  print_string "passed: annvariable\n";
  if !Errormsg.anyErrors then
    1
  else

  (*  Construct a codegen module. *)
  let cg = Codegen.generateModuleCode absyn in
  print_string "passed: codegen\n";
  if !Errormsg.anyErrors then
    1
  else
  
  (*  Open the correct output file. *)
  let _ = Bytecode.openOutChannel outfile in
  if !Errormsg.anyErrors then
    1
  else
  (Bytecode.setWordSize ();
  (*  Write the code to the output file.  *)
  let _ = Spitcode.writeByteCode cg in
  if !Errormsg.anyErrors then
    1
  else
    0))))

let debugEnabled = ref false
let inputFilename = ref ""
let outputFilename = ref ""


(**********************************************************************
*parseArgs:
* Parses the command line.
**********************************************************************)
let parseArgs =
  fun () ->
    let speclist = [("--input", Arg.Set_string(inputFilename), "input file");
                    ("-i", Arg.Set_string(inputFilename), "input file");
                    ("--output", Arg.Set_string(outputFilename), "output file");
                    ("-o", Arg.Set_string(outputFilename), "output file");
                    
                    ("--preabsyn", Arg.Set(Compile.printPreAbsyn), "print preabstract syntax");
                    ("--absyn", Arg.Set(Compile.printAbsyn), "print abstract syntax");
                    ("--clauses", Arg.Set(Compile.printClauses), "print clauses");
                    
                    ("--no-errors", Arg.Clear(Errormsg.errorsEnabled), "hide errors and warnings");
                    ("--no-warnings", Arg.Clear(Errormsg.warningsEnabled), "hide warnings");
                    ("--log", Arg.Set(Errormsg.loggingEnabled), "show logging information");
                    ("--all-errors", Arg.Set(Errormsg.warningsAsErrors), "interpret warnings as errors");
                    
                    ("--unit-tests", Arg.Set(unitTests), "run unit tests")] in
    
    (Arg.parse speclist (fun s -> ()) "Usage: tjc --input \"filename\" --output \"filename\"")

(**********************************************************************
*runUnitTests:
* Runs unit tests for all modules.
**********************************************************************)
let runUnitTests () =
  let _ = Types.unitTests () in
  let _ = Parse.unitTests () in
  let _ = Clauses.unitTests () in
  ()

(**********************************************************************
*main:
* Main compiler loop.  Reads command line arguments to set up options,
* and then compiles the given file.
**********************************************************************)
let main () =
  (******************************************************************
  *getOutfile:
  * Gets the outfile filename.  Defaults to the input filename with
  * ".lp" appended.
  ******************************************************************)
  let getOutfile infile =
    if !outputFilename = "" then
      infile ^ ".lp"
    else
      !outputFilename
  in
  
  (*  Parse the command line arguments.  This sets up error, warning, and log output,
      and so must be called before any output needs to occur. *)
  let _ = parseArgs () in
  
  (*  Run unit tests. *)
  let _ =
    if !unitTests then
      (Errormsg.loggingEnabled := true;
      runUnitTests ())
    else
      ()
  in
  
  let infile = !inputFilename in
  let outfile = getOutfile infile in
  
  if infile = "" then
    (print_endline ("Error: No input file specified.");
    -1)
  else if (Filename.check_suffix infile ".mod") = false then
    (print_endline ("Error: Invalid input filename.  File must have extension '*.mod'.");
    -1)
  else
    let result = compile (Filename.chop_extension infile) outfile in
    if !Errormsg.anyErrors then
      (print_endline "Compilation failed.";
      result)
    else
      (print_endline "Compilation succeeded.";
      result)

(*  Execute main  *)
let _ = main ()
