(***************************************************************************)
(* different sorts of command                                              *)
(***************************************************************************)
type tjcommand =
	Tjcc
  | Tjsim
  | Tjdis
  | Teyjus

(* ignore arguments *)
let ignoreFunc args = ()

(* display version information *)
let printVersion () =
  print_string "version number:... \n"
  
(* display help message *)
let printHelpMessage () =
  print_string "blar\n"

(* set path *)
let setPath path =
  print_string "not implemented\n"

(* obtain a module name *)
let getModName name =
  let length = String.length name in
  let rec actualLength ind =
	if (ind = length) then ind
	else
	  if (String.get name ind) = '.' then ind
	  else actualLength (ind + 1)
  in
  
  String.sub name 0 (actualLength 0)
		

let inputName = ref ""

(***************************************************************************)
(* options common to all sorts of command                                  *)
(***************************************************************************)
let speclist =
  [("-v", Arg.Tuple([Arg.Unit(printVersion); Arg.Rest(ignoreFunc)]), 
   "\n    Return the system version");
   ("--version", Arg.Tuple([Arg.Unit(printVersion); Arg.Rest(ignoreFunc)]),
	"\n    Return the system version");
   ("-h", Arg.Tuple([Arg.Unit(printHelpMessage); Arg.Rest(ignoreFunc)]),
	"\n    Print this message");
   ("--help", Arg.Tuple([Arg.Unit(printHelpMessage); Arg.Rest(ignoreFunc)]), 
	"\n    Print this message")
 ]

(***************************************************************************)
(* tjcc                                                                    *)
(***************************************************************************)
let outputFileName = ref ""

(* options *)
let tjccSpecList =
  (("-o", Arg.Tuple[Arg.Set_string(outputFileName); Arg.Rest(ignoreFunc)], 
	"\n    Specifies the name of the output bytecode file.") ::
   ("--output", Arg.Tuple[Arg.Set_string(outputFileName); Arg.Rest(ignoreFunc)], 
	"\n    Specifies the name of the output bytecode file.") ::
   ("-p", Arg.Tuple[Arg.String(setPath); Arg.Rest(ignoreFunc)], 
	"\n    Add PATH to the search path for modules. Several paths may be specified.") ::
   ("--path", Arg.Tuple[Arg.String(setPath); Arg.Rest(ignoreFunc)],
	"\n    Add PATH to the search path for modules. Several paths may be specified.") ::
   speclist)

(* anonymous function *)
let tjccAnonFunc name = 
  inputName := (getModName name)


(* usage message *)
let tjccUsageMsg = 
  "tjcc [-v|--version] [-h|--help] [-p|--path PATH]* [-o|--output FILENAME]\n"  

(****************************************************************************)
(* options for tjdis                                                        *)
(****************************************************************************)
let tablesOnly = ref false
let instrOnly  = ref false
let linkedFile = ref false

(* options *)
let tjdisSpecList = 
  (("-t", Arg.Set(tablesOnly), "\n    Only print tables.") ::
   ("--t", Arg.Set(tablesOnly), "\n    Only print tables.") ::
   ("-i", Arg.Set(instrOnly), "\n    Only print instructions.") ::
   ("--instr", Arg.Set(instrOnly), "\n    Only print instructions.") ::
   ("-l", Arg.Set(linkedFile), "\n    Disassemble linked file.") ::
   ("--link", Arg.Set(linkedFile), "\n    Disassemble linked file.") ::
   speclist)
  
(* anonymous function *)
let tjdisAnonFunc name = inputName := name

(* usage message *)
let tjdisUsageMsg =
  "tjdis [-v|--version] [-h|--help] [-t|--tables] [-i|--instr] filename\n"

(****************************************************************************)
(* options for tjsim                                                        *)
(****************************************************************************)
let minSolutions = ref 0
let maxSolutions = ref max_int
let quiet = ref false
let batch = ref false
let heapSize = ref 0 

let queryStrings = ref []

let addQuery str =
  queryStrings := (!queryStrings) @ [str]

let popQuery () =
  let queries = !queryStrings in
  if (queries = []) then ""
  else
	let query = List.hd queries in
	queryStrings := List.tl queries;
	query


(* options *)
let tjsimSpecList = 
  (("-p", Arg.String(setPath), 
	"\n    Add PATH to the search path for modules. Several paths may be specified.") ::
   ("--path", Arg.String(setPath),
	"\n    Add PATH to the search path for modules. Several paths may be specified.") ::
   ("-s", Arg.String(addQuery), 
	"\n    Solve the given query on startup.  Several queries may be specified with separate --solve tags.") ::
   ("--solve", Arg.String(addQuery), 
	"\n    Solve the given query on startup.  Several queries may be specified with separate --solve tags.") ::
   ("-e", Arg.Set_int(minSolutions),
	"\n    Expect at least this many solutions from each query; error if fewer.\n     Valid only in batch mode.") :: 
   ("--expect", Arg.Set_int(minSolutions),
	"\n    Expect at least this many solutions from each query; error if fewer.\n     Valid only in batch mode.") :: 
   ("-m", Arg.Set_int(maxSolutions),
	"\n    Halt after this many solutions to the query have been found.\n    Valid only in batch mode.")::
   ("--maximum", Arg.Set_int(maxSolutions),
	"\n    Halt after this many solutions to the query have been found.\n    Valid only in batch mode."):: 
   ("-q", Arg.Set(quiet), 
	"\n    Suppress all non-error output from the system, except variable bindings on query success.") ::
   ("--quiet", Arg.Set(quiet),
	"\n    Suppress all non-error output from the system, except variable bindings on query success.") ::	
   ("-b", Arg.Set(batch),
	"\n    Suppress system interaction; send all output without stopping.") ::
   ("--batch", Arg.Set(batch),
	"\n    Suppress system interaction; send all output without stopping.") ::
   ("-k", Arg.Set_int(heapSize),
	"\n     Allocate a heap of the given size (K)") ::
   ("--heap", Arg.Set_int(heapSize),
	"\n     Allocate a heap of the given size (K)") ::	
   speclist)

(* anonymous function *)
let tjsimAnonFunc name = inputName := getModName name

(* usage message *)
let tjsimUsageMsg =
  "tjsim [-v|--version] [-h|--help] [-p|--path PATH]*\n      [-s|--solve \"QUERY.\"]* [-e|--expect NUM] [-m|--maximum NUM]\n      [-q|--quiet] [-b|--batch] [-k|--heap HEAPSIZE] [module]\n"

(***************************************************************************)
(* parse command line arguments                                            *)
(***************************************************************************)  
let parseArgs tjcommand =
  match tjcommand with
	Tjcc -> 
	  if (Array.length Sys.argv) = 1 then 
		(print_endline "Error: No input file specified.";
		 false)
	  else
		(Arg.parse tjccSpecList tjccAnonFunc tjccUsageMsg;
		 true)
  | Tjdis -> 
	  if (Array.length Sys.argv) = 1 then 
		(print_endline "Error: No input file specified.";
		 false)
	  else
		(Arg.parse tjdisSpecList tjdisAnonFunc tjdisUsageMsg;
		 true)
  | Tjsim ->
	  (Arg.parse tjsimSpecList tjsimAnonFunc tjdisUsageMsg;
	   true)
  | _ -> (print_string "not implemented\n"; false)
	  
