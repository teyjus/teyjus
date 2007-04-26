(**********************************************************************
*
**********************************************************************)
exception Exception

val typeAbbrevTable : Preabsyn.ptypeabbrev Table.SymbolTable.t

(*  Compile to Preabsyn *)
val compileModule : string -> Preabsyn.pmodule
val compileSignature : string -> Preabsyn.pmodule

val printPreAbsyn : bool ref
val printAbsyn : bool ref
val printClauses : bool ref

val openFile : string -> (string -> 'a) -> 'a
val closeFile : 'a -> ('a -> unit) -> unit
