let main () =
  if (Parseargs.parseArgs Parseargs.Tjdis) then
	let tablesOnly = !Parseargs.tablesOnly in
	let instrOnly  = !Parseargs.instrOnly  in
	let linkedFile = !Parseargs.linkedFile in
	if (tablesOnly && instrOnly) then
	  (print_endline "Error: tables only (-t/--table) and instrunctions only (-i/--instr) can not be simultaneously selected.";
	   1)
	else
	  (Bytecode.setWordSize ();
	   Pervasiveutils.pervasiveKindIndexMappingInit ();
	   Pervasiveutils.pervasiveConstantIndexMappingInit ();
	   Disassembly.disassemble (!Parseargs.inputName) tablesOnly instrOnly 
		 linkedFile)
  else 1 

(* execute main *)
let _ = main ()
