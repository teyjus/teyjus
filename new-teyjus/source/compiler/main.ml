(**********************************************************************
*compile:
* Compile the given module or signature, based on the file name.
**********************************************************************)
let compile = fun basename ->
  let showTerm = function
    (Preabsyn.Clause(term)::_, amod) -> let _ = Parse.translateTerm term amod in ()
  | _ -> ()
  in
  
  let modresult = Compile.compileModule basename in
  let sigresult = Compile.compileSignature basename in

  let absyn = (Translate.translate modresult sigresult) in

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

  (*  Print the results (absyn module) to the output file *)
  if !Compile.printAbsyn then
    let outchannel = Compile.openFile (basename ^ ".absyn.txt") open_out in
    (Absyn.printAbsyn absyn outchannel;
    Compile.closeFile outchannel close_out)
  else
    ();

  let clauses = (Preabsyn.getModuleClauses modresult) in
  (showTerm (clauses, absyn);
  absyn))

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
                    
                    ("--no-errors", Arg.Clear(Errormsg.errorsEnabled), "hide errors");
                    ("--no-warnings", Arg.Clear(Errormsg.warningsEnabled), "hide warnings");
                    ("--log", Arg.Set(Errormsg.loggingEnabled), "show logging information");
                    ("--all-errors", Arg.Set(Errormsg.warningsAsErrors), "interpret warnings as errors")] in
    
    (Arg.parse speclist (fun s -> ()) "Usage: tjc --input \"filename\" --output \"filename\"")

(**********************************************************************
*main:
* Main compiler loop.  Reads command line arguments to set up options,
* and then compiles the given file.
**********************************************************************)
let main =
  fun () ->
    (******************************************************************
    *getOutfile:
    * Gets the outfile filename.  Defaults to the input filename with
    * ".bc" appended.
    ******************************************************************)
    let getOutfile = function () ->
      if !outputFilename = "" then
        !inputFilename ^ ".bc"
      else
        !outputFilename
    in
    
    let _ = parseArgs () in
    let infile = !inputFilename in

    if infile = "" then
      (print_endline ("Error: No input file specified.");
      -1)
    else if (Filename.check_suffix infile ".mod") = false then
      (print_endline ("Error: Invalid input filename.  File must have extension '*.mod'.");
      -1)
    else
      let _ = compile (Filename.chop_extension infile) in
      if !Errormsg.anyErrors then
        (print_endline "Compilation failed.";
        -1)
      else
        (print_endline "Compilation succeeded.";
        0)

(*  Execute main  *)
let _ = main ()
