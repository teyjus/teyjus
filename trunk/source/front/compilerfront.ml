open Parseargs

let abort_on_error () =
  if !Errormsg.anyErrors then
    exit 1

let compile basename outfile =
  (* Parse the input module and signature and generate preabsyn. *)
  let modresult = Compile.compileModule basename in
  let _ = abort_on_error () in
    
  let sigresult = Compile.compileSignature basename in
  let _ = abort_on_error () in
      
  (* Construct an absyn module.  At this point only the module's *)
  (* constant, kind, and type abbrev information is valid.       *)
  let (absyn, sigabsyn) = Translate.translate modresult sigresult in
  let _ = abort_on_error () in

  (* Get the list of clauses and new clauses. *)
  let (absyn, clauses, newclauses, closeddefs) = 
	Clauses.translateClauses modresult absyn in
  let _ = abort_on_error () in

  (* Reduce skeleton *)
  let absyn = Typereduction.reduceSkeletons absyn in
  let _ = abort_on_error () in
  
  (* Process the clauses. *)
  let absyn = 
	Processclauses.processClauses absyn clauses newclauses closeddefs in
  let _ = abort_on_error () in
 
  (* Reduce predicate types *)
  (* let absyn = Typereduction.reducePredicates absyn in *)
  (* let _ = abort_on_error () in *)
				
  (* Process the clauses. *)
  let _ = Annvariables.processClauses absyn in
  let _ = abort_on_error () in
				  
  (* Construct a codegen module. *)
  let cg = Codegen.generateModuleCode absyn in
  let _ = abort_on_error () in
					
  (* Open the correct output file. *)
  let _ = Bytecode.openOutChannel outfile in
  let _ = abort_on_error () in
    
	Bytecode.setWordSize () ;
	Spitcode.writeByteCode cg ;
    abort_on_error ()



let inputName = ref ""
let outputName = ref ""

let setPath path =
  print_string "not implemented\n"
  
let specList = dualArgs
  [("-o", "--output", Arg.Set_string outputName,
    " Specifies the name of the output bytecode file") ;
   ("-p", "--path", Arg.String setPath,
    " Add PATH to the search path. Several paths may be specified.") ;
   versionspec]

let anonFunc name = 
  inputName := getModName name

let usageMsg = 
  "Usage: tjcc <options> <files>\n" ^
  "options are:"

let _ =
    Arg.parse (Arg.align specList) anonFunc usageMsg ;

    if !inputName = "" then
      error "No input file specified." ;

    if !outputName = "" then
      outputName := Bytecode.makeByteCodeFileName !inputName ;
    
    compile !inputName !outputName



