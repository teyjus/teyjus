type symbol = Symbol.symbol
type pos = Errormsg.pos

val translateClauses : Preabsyn.pmodule -> Absyn.amodule -> (Absyn.amodule * Absyn.aterm list * Absyn.aterm list)

