open Parseargs

let tablesOnly = ref false
let instrOnly  = ref false
let inputName = ref ""
  
let specList = dualArgs
  [("-t", "--table", Arg.Set tablesOnly, " Only print tables") ;
   ("-i", "--instr", Arg.Set instrOnly, " Only print instructions") ;
   versionspec]

let anonFunc name =
  inputName := name

let usageMsg =
  "Using: tjdis <options> <object-file>\n" ^
    "options are:"

let _ =
  Arg.parse (Arg.align specList) anonFunc usageMsg ;

  if !inputName = "" then
    error "No input file specified." ;
  
  if (!tablesOnly && !instrOnly) then
    error ("tables only (-t/--table) and instructions only " ^
             "(-i/--instr) can not be selected simultaneously.") ;
  
  Bytecode.setWordSize () ;
  Pervasiveutils.pervasiveKindIndexMappingInit () ;
  Pervasiveutils.pervasiveConstantIndexMappingInit () ;
  Disassembly.disassemble !inputName !tablesOnly !instrOnly
