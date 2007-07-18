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
let closeFile fname f =
  f fname

let compile parse fname =
  let inchannel = openFile fname open_in in
  let lexbuf = Lexing.from_channel inchannel in
  let _ = Lplex.set_file_name lexbuf fname in
  let result = parse Lplex.initial lexbuf in
  let _ = closeFile inchannel close_in in
    result
      
let compileModule basename =
  compile Lpyacc.parseModule (basename ^ ".mod")

let compileSignature basename =
  compile Lpyacc.parseSignature (basename ^ ".sig")

let compileString s =
  let lexbuf = Lexing.from_string s in
  let _ = Lplex.set_file_name lexbuf "" in
  parseModClause Lplex.initial lexbuf