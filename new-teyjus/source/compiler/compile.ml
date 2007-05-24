(**********************************************************************
*
**********************************************************************)
exception Exception
open Lpyacc

let typeAbbrevTable = Table.SymbolTable.empty

let printPreAbsyn = ref false
let printAbsyn = ref false
let printClauses = ref false

(******************************************************************
*openFile:
* Open a file and exit if there is an error.
******************************************************************)
let openFile = fun fname f ->
  try 
    let inchannel = f fname in
    inchannel
  with Sys_error(s) -> (print_endline s; exit 1)

(******************************************************************
*closeFile:
* Closes a file given a closing function.
******************************************************************)
let closeFile = fun fname f ->
  f fname

let rec compileModule = function
  basename ->
    (*  Parse Source  *)
    let inchannel = (openFile (basename ^ ".mod") open_in) in
    let lexbuf = Lexing.from_channel inchannel in
    let result = Lpyacc.parseModule Lplex.initial lexbuf in
    (closeFile inchannel close_in;
    result)

and compileSignature = function
  basename ->
    let inchannel = openFile (basename ^ ".sig") open_in in
    let lexbuf = Lexing.from_channel inchannel in
    let result = Lpyacc.parseSignature Lplex.initial lexbuf in
    (closeFile inchannel close_in;
    result)
