open Parseargs

let tablesOnly = ref false
let instrOnly  = ref false
let linkedFile = ref false
let inputName = ref ""
  
let specList = dualArgs
  [("-t", "--table", Arg.Set tablesOnly, " Only print tables") ;
   ("-i", "--instr", Arg.Set instrOnly, " Only print instructions") ;
   ("-l", "--link", Arg.Set linkedFile, " Disassemble linked file") ;
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
    error ("tables only (-t/--table) and instrunctions only " ^
             "(-i/--instr) can not be simultaneously selected.") ;
  
  Bytecode.setWordSize () ;
  Pervasiveutils.pervasiveKindIndexMappingInit () ;
  Pervasiveutils.pervasiveConstantIndexMappingInit () ;
  Disassembly.disassemble !inputName !tablesOnly !instrOnly !linkedFile