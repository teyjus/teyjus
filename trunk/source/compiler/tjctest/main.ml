let testNumber = ref (-1)
let testAll = ref false
let testDirectory = ref "."
let errorsEnabled = ref true

(**********************************************************************
*runOne:
* Runs a single test.  The result is None if the test cannot be found,
* and Some(b) if it can, where b is true if the test succeeds and
* false if it doesn't.
**********************************************************************)
let runOne i =
  let errors () =
    if !errorsEnabled then
      ""
    else
      " --no-errors"
  in
  
  let read filename =
    let inchan = open_in filename in
    let len = in_channel_length inchan in
    let result = "" in
    let _ = really_input inchan result 0 len in
    (close_in inchan;
    result)
  in
  
  let testNameBase = (!testDirectory) ^ "/" ^ "test." ^(string_of_int i) in
  
  let testName = testNameBase ^ ".mod" in
  
  let modOutputName = testNameBase ^ ".out" in
  
  let modPreabsynName = testNameBase ^ ".mod.preabsyn.txt" in
  let modPreabsynExpectedName = testNameBase ^ ".mod.preabsyn" in
  
  let sigPreabsynName = testNameBase ^ ".sig.preabsyn.txt" in
  let sigPreabsynExpectedName = testNameBase ^ ".sig.preabsyn" in
  
  let modAbsynName = testNameBase ^ ".absyn.txt" in
  let modAbsynExpectedName = testNameBase ^ ".absyn" in
  
  let command = "tjc.exe --input \"" ^ testName ^ "\" --ouput \"" ^ modOutputName ^
    "\" --preabsyn --absyn" ^ (errors ()) in

  let clean () =
    (Sys.remove modPreabsynName;
    Sys.remove sigPreabsynName;
    Sys.remove modAbsynName;
    Sys.remove modOutputName)
  in
  
  (*  Find the test file. *)
  if not (Sys.file_exists testName) then
    None
  else
  
  (*  Check if the file compiles. *)
  let result = Sys.command command in
  if result == -1 then
    (print_endline ("Test " ^ (string_of_int i) ^ ": compilation failed.");
    clean ();
    Some(false))
  else
  
  (*  Check the results of the compile. *)
  let modPreabsyn = read modPreabsynName in
  let modPreabsynExpected = read modPreabsynExpectedName in
  let sigPreabsyn = read sigPreabsynName in
  let sigPreabsynExpected = read sigPreabsynExpectedName in
  let modAbsyn = read modAbsynName in
  let modAbsynExpected = read modAbsynExpectedName in
  
  if modPreabsyn <> modPreabsynExpected ||
    sigPreabsyn <> sigPreabsynExpected ||
    modAbsyn <> modAbsynExpected then
    Some(false)
  else
    Some(true)

(**********************************************************************
*runTest:
**********************************************************************)
let runTest i =
  match runOne i with
    Some(result) ->
      result
  | None ->
      (print_endline ("Test " ^ (string_of_int i) ^ " not found.");
      false)

(**********************************************************************
*runAll:
**********************************************************************)
let runAll () =
  let rec run' i success =
    match runOne i with
      Some(success') -> run' (i + 1) (success && success')
    | None -> (success, i)
  in
  let (result, num) = run' 0 true in
  if num = 0 then
    (print_endline ("No tests found.");
    false)
  else
    result

(**********************************************************************
*parseArgs:
* Parses the command line.
**********************************************************************)
let parseArgs =
  fun () ->
    let speclist = [("--directory", Arg.Set_string(testDirectory), "test directory");
                    ("--test", Arg.Set_int(testNumber), "run test by number");
                    ("--all", Arg.Set(testAll), "run all tests");
                    ("--no-errors", Arg.Clear(errorsEnabled), "hide compilation errors and warnings")] in
    
    (Arg.parse speclist (fun s -> ()) "Usage: tjctest --all")

(**********************************************************************
*main:
* Main compiler loop.  Reads command line arguments to set up options,
* and then compiles the given file.
**********************************************************************)
let main () =
    let _ = parseArgs () in
    
    if (!testNumber < 0) && (not (!testAll)) then
      (print_endline "Error: no tests specified.";
      -1)
    else if (!testNumber < 0) && (!testAll) then
      if runAll () then
        (print_endline "Tests succeeded.";
        0)
      else
        (print_endline "Tests failed.";
        -1)
    else if (!testNumber > 0) && (!testAll) then
      if runAll () then
        (print_endline "Tests succeeded.";
        0)
      else
        (print_endline "Tests failed.";
        -1)
    else if (!testNumber > 0) && (not (!testAll)) then
      if runTest (!testNumber) then
        (print_endline "Tests succeeded.";
        0)
      else
        (print_endline "Tests failed.";
        -1)
    else
      (print_endline "Error: sheer craziness.";
      -1)

(*  Execute main  *)
let _ = main ()
