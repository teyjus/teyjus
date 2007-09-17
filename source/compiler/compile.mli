(**********************************************************************
*Compile:
**********************************************************************)
val typeAbbrevTable : Preabsyn.ptypeabbrev Table.SymbolTable.t

(**********************************************************************
*compileModule:
* Given the filename of a lambda prolog module, parses the file and
* returns a preabsyn module.
**********************************************************************)
val compileModule : string -> Preabsyn.pmodule

(**********************************************************************
*compileModule:
* Given the filename of a lambda prolog signature, parses the file and
* returns a preabsyn signature.
**********************************************************************)
val compileSignature : string -> Preabsyn.pmodule

(**********************************************************************
*compileModule:
* Given a string containing lambda prolog code, parses the file and
* returns a preabsyn term.
**********************************************************************)
val compileString : string -> Preabsyn.pterm

(*  Flags indicating whether to print various stages of compilation;
    for testing.  *)
val printPreAbsyn : bool ref
val printAbsyn : bool ref
val printClauses : bool ref

val openFile : string -> (string -> 'a) -> 'a
val closeFile : 'a -> ('a -> unit) -> unit
