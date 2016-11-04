(* This is taken from the Twelf implementation *)

  val parseQualId' : (string list * Tparsing.Parsing.lexResult) Tparsing.Parsing.parser
  val parseTerm' : ExtSyn.term Tparsing.Parsing.parser
  val parseDec' : (string option * ExtSyn.term option) Tparsing.Parsing.parser


