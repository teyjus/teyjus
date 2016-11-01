(* This is taken from the Twelf implementation *)

  val parseQualId' : (string list * Parsing.Parsing.lexResult) Parsing.Parsing.parser
  val parseTerm' : ExtSyn.term Parsing.Parsing.parser
  val parseDec' : (string option * ExtSyn.term option) Parsing.Parsing.parser


