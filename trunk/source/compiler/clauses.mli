(**********************************************************************
*closeddefinition:
* Represents a closed definition.  The constant is the definition's
* name, the term is its clause.
**********************************************************************)
type closeddefinition = (Absyn.aconstant * Absyn.aterm)	

(**********************************************************************
*translateClauses:
* Given a preabsyn module containing preabsyn clauses, and an absyn module
* with the appropriate kind and constant tables filled in, returns an
* absyn module with the clauses fields updated, as well clauses and
* new clauses, and a list of closed definitions.
*
* Arguments:
*   a preabsyn module
*   an absyn module with local and global constant tables filled in
* Returns:
*   the absyn module, with the hidden constants list filled in
*   the translated clauses
*   the new clauses introduced during parsing
*   the closed definitions
**********************************************************************)
val translateClauses : Preabsyn.pmodule -> Absyn.amodule -> 
  (Absyn.amodule * Absyn.aterm list * Absyn.aterm list * closeddefinition list)

(**********************************************************************
*printTranslatedClauses:
* Given a list of clauses, a list of new clauses, and an out_channel,
* prints the clauses and new clauses to the out_channel.
**********************************************************************)
val printTranslatedClauses : Absyn.aterm list -> Absyn.aterm list -> 
  out_channel -> unit

val unitTests : unit -> unit