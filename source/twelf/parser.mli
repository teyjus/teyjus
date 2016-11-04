(* This is taken from the Twelf implementation *)

  type fileParseResult =
      ConDec of ExtSyn.condec
    | FQuery of ExtSyn.query (* A *)
    (* Further declarations to be added here *)

  val parseStream: in_channel -> (fileParseResult * Paths.region) Tparsing.Parsing.Lexer'.Stream'.stream
  val parseTerminalQ : string * string -> ExtSyn.query Tparsing.Parsing.Lexer'.Stream'.stream (* reads from std input *)

