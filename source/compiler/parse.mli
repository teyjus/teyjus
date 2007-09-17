type pos = Errormsg.pos
type symbol = Symbol.symbol

(**********************************************************************
*translateTerm:
* Given a preabstract syntax term (assumed to have been parsed at the
* top-level interface), parses the term and returns an abstract syntax
* term and associated information.
*
* Arguments:
*   a preabsyn term
*   an absyn module with kind and constant tables filled in
* Returns:
*   the translated absyn term
*   a typemolecule corresponding to this term
*   a list of free variables in the term
*   a list of new free type variables in the type
*
**********************************************************************)
val translateTerm : Preabsyn.pterm -> Absyn.amodule ->
  (Absyn.aterm * Types.typemolecule * Absyn.atypesymbol list * Absyn.atype list)
  
(**********************************************************************
*translateClause:
* Given a preabstract syntax term representing a clause (assumed to
* have been parsed while processing a file), parses the term and returns
* an abstract syntax term.
*
* Arguments:
*   a preabsyn term
*   an absyn module with kind and constant tables filled in
* Returns:
*   the translated absyn term
*
**********************************************************************)
val translateClause : Preabsyn.pterm -> Absyn.amodule -> Absyn.aterm

(**********************************************************************
*removeNestedAbstractions:
* Removes any nested abstractions in an absyn term, replacing them
* with "flattened" unnested abstractions.  It is assumed that the absyn
* term has no unnested abstractions in it.
*
* Arguments:
*   an absyn term
* Returns:
*   the unnested absyn term
*
**********************************************************************)
val removeNestedAbstractions : Absyn.aterm -> Absyn.aterm


val unitTests : unit -> unit
