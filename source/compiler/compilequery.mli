(****************************************************************************
 * Compilequery:
 *   An interface for compiling a query
 ***************************************************************************)

(****************************************************************************
 * compileQuery:
 *   Given a query and an abstract syntax module with global symbol tables.
 *   Returns a Query Codegen Module corresponding to bytecode which will
 *   execute the query.
 ***************************************************************************)

val compileQuery : string -> Absyn.amodule -> Codegen.qcqmodule
