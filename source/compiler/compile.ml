(**********************************************************************
*
**********************************************************************)
exception Exception
open Lexing
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

let compile = fun parse fname ->
  let inchannel = openFile fname open_in in
  let lexbuf = Lexing.from_channel inchannel in
  let _ = Lplex.set_file_name lexbuf fname in
  let result = parse Lplex.initial lexbuf in
  let _ = closeFile inchannel close_in in
    result
      
let compileModule = fun basename ->
  compile Lpyacc.parseModule (basename ^ ".mod")

let compileSignature = fun basename ->
  compile Lpyacc.parseSignature (basename ^ ".sig")
