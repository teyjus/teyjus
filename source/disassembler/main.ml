let inputFilename = ref ""

let parseArgs =
  fun () ->
	let speclist =  
	  [("--input", Arg.Set_string(inputFilename), "input file");]
	in
	(Arg.parse speclist (fun s -> ()) 
	   "Usage: tjdis --input \"filename\"")

let main filename =
  Bytecode.setWordSize ();
  Pervasiveutils.pervasiveKindIndexMappingInit ();
  Pervasiveutils.pervasiveConstantIndexMappingInit ();
  let _ = parseArgs () in
  Disassembly.disassemble (!inputFilename)
  
let _ = main ()
