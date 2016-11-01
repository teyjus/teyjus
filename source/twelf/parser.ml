(* This is taken from the Twelf implementation *)

module type PARSER = 
sig
  type fileParseResult =
      ConDec of ExtSyn.condec
    | FQuery of ExtSyn.query (* A *)
    (* Further declarations to be added here *)

  val parseStream: in_channel -> (fileParseResult * Paths.Paths.region) Stream.Stream.stream
  val parseTerminalQ : string * string -> ExtSyn.query Stream.Stream.stream (* reads from std input *)

end  (* signature PARSER *)

module Parser  (ParseConDec : ParseCondec.PARSE_CONDEC) 
                                 (ParseQuery : ParseQuery.PARSE_QUERY) (ParseTerm : ParseTerm.PARSE_TERM) : PARSER  =
struct
  type fileParseResult =
      ConDec of ExtSyn.condec
    | FQuery of ExtSyn.query (* A *)
    (* Further declarations to be added here *)
 
  let stripDot args =
    match args with
        (Stream.Stream.Cons((Lexer.Lexer.DOT, r), s)) -> s
      | (Stream.Stream.Cons((Lexer.Lexer.RPAREN, r), s)) ->
          Parsing.Parsing.error (r, "Unexpected right parenthesis")
      | (Stream.Stream.Cons((Lexer.Lexer.RBRACE, r), s)) -> 
          Parsing.Parsing.error (r, "Unexpected right brace")
      | (Stream.Stream.Cons((Lexer.Lexer.RBRACKET, r), s)) -> 
          Parsing.Parsing.error (r, "Unexpected right bracket")
      | (Stream.Stream.Cons ((Lexer.Lexer.EOF, r), s)) -> 
          Parsing.Parsing.error (r, "Unexpected end of file")
      | (Stream.Stream.Cons ((Lexer.Lexer.EQUAL, r), s)) -> 
          Parsing.Parsing.error (r, "Unexpected `='")
      | (Stream.Stream.Cons ((t, r), s)) -> 
          Parsing.Parsing.error (r, "Expected `.', found " ^ Lexer.Lexer.toString t)
      (* Everything else should be impossible *)

  let rec parseTLStream instream =
    let finish args =
      match args with
          (Stream.Stream.Cons ((Lexer.Lexer.EOF, r), s)) -> Stream.Stream.Empty
        | (Stream.Stream.Cons ((Lexer.Lexer.RBRACE, r), s)) ->
            Parsing.Parsing.error (r, "Unmatched `}'")
    in
    Stream.Stream.delay (fun () -> parseStream'' (Stream.Stream.expose(Lexer.Lexer.lexStream instream), finish))

  and parseStream' (s, sc)  =
    Stream.Stream.delay (fun () -> parseStream'' (Stream.Stream.expose s, sc))

  (* parseStream'' : lexResult front -> fileParseResult front *)
  (* parseStream'' switches between various specialized parsers *)
  and parseStream'' (f, sc)  =
    match (f, sc) with
        (Stream.Stream.Cons ((Lexer.Lexer.ID (idCase,name), r0), s'), sc) -> parseConDec' (f, sc)
      | (Stream.Stream.Cons ((Lexer.Lexer.ABBREV, r), s'), sc) -> 
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %abbrev  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.UNDERSCORE, r), s'), sc) -> parseConDec' (f, sc)
      | (Stream.Stream.Cons ((Lexer.Lexer.INFIX, r), s'), sc) -> 
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : fixity  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.PREFIX, r), s'), sc) -> 
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : fixity  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.POSTFIX, r), s'), sc) -> 
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : fixity  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.NAME, r1), s'), sc) ->
          (*let (namePref, ((Stream.Stream.Cons ((_, r2), _)) as f')) = ParseFixity.parseNamePref' f in
          let  r = Paths.Paths.join (r1, r2) in
          Stream.Stream.Cons ((NamePref namePref, r), parseStream' (stripDot f', sc))*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r1)^" : %name  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons((Lexer.Lexer.DEFINE, r), s'), sc) ->
          (*parseSolve' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %define declarations are not treated.");
          sc f
      | (Stream.Stream.Cons((Lexer.Lexer.SOLVE, r), s'), sc) ->
          (*parseSolve' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %solve  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons((Lexer.Lexer.QUERY, r0), s'), sc) ->
          (*let (expected, s1) = parseBound' (Stream.Stream.expose s') in
          let (tri, s2) = parseBound' (Stream.Stream.expose s1) in
          let (query, ((Stream.Stream.Cons((_,r'),_)) as f3)) = ParseQuery.parseQuery' (Stream.Stream.expose s2) in
          let r = Paths.Paths.join (r0, r') in 
          Stream.Stream.Cons ((Query (expected, tri, query), r), parseStream' (stripDot f3, sc))*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r0)^" : %query  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons((Lexer.Lexer.FQUERY, r0), s'), sc) ->
          let (query, ((Stream.Stream.Cons((_,r'),_)) as f3)) = ParseQuery.parseQuery' (Stream.Stream.expose s') in
          let r = Paths.Paths.join (r0, r') in
          Stream.Stream.Cons ((FQuery query, r), parseStream' (stripDot f3, sc))
      | (Stream.Stream.Cons((Lexer.Lexer.QUERYTABLED, r0), s'), sc) ->
          (*let (numSol, s1) = parseBound' (Stream.Stream.expose s') in
          let (try, s2) = parseBound' (Stream.Stream.expose s1) in
          let (query, ((Stream.Stream.Cons((_,r'),_)) as f3)) = ParseQuery.parseQuery' (Stream.Stream.expose s2) in
          let r = Paths.Paths.join (r0, r') in
          Stream.Stream.Cons ((Querytabled (numSol, try, query), r), parseStream' (stripDot f3, sc))*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r0)^" : %querytabled  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.MODE, r), s'), sc) -> (*parseMode' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %mode  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.UNIQUE, r), s'), sc) -> (*parseUnique' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %unique  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.COVERS, r), s'), sc) -> (*parseCovers' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %covers  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.TOTAL, r), s'), sc) -> (*parseTotal' (f, sc) (* -fp *)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %total  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.TERMINATES, r), s'), sc) -> (*parseTerminates' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %terminates  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.BLOCK, r), s'), sc) -> (*parseConDec' (f, sc) (* -cs *)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %block  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.WORLDS, r), s'), sc) -> (*parseWorlds' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %worlds  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.REDUCES, r), s'), sc) -> (*parseReduces' (f, sc) (* -bp *)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %reduces  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.TABLED, r), s'), sc) -> (*parseTabled' (f, sc) (* -bp *)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %tabled  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.KEEPTABLE, r), s'), sc) -> (*parseKeepTable' (f, sc) (* -bp *)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %keepTable  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.THEOREM, r), s'), sc) -> (*parseTheorem' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %theorem  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.PROVE, r), s'), sc) -> (*parseProve' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %prove  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.ESTABLISH, r), s'), sc) -> (*parseEstablish' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %establish  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.ASSERT, r), s'), sc) -> (*parseAssert' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %assert  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.TRUSTME, r), s'), sc) -> (*parseTrustMe' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %trustme  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.FREEZE, r), s'), sc) -> (*parseFreeze' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %freeze  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.SUBORD, r), s'), sc) -> (*parseSubord' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %subord  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.THAW, r), s'), sc) -> (*parseThaw' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %thaw  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.DETERMINISTIC, r), s'), sc) -> (*parseDeterministic' (f, sc) (* -rv *)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %deterministic  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.COMPILE, r), s'), sc) -> (*parseCompile' (f, sc) (* -ABP 4/4/03 *)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %compile  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.CLAUSE, r), s'), sc) -> (*parseClause' (f, sc) (* -fp *)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %clause  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.SIG, r), s'), sc) -> (*parseSigDef' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %sig  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.STRUCT, r), s'), sc) -> (*parseStructDec' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %struct  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.INCLUDE, r), s'), sc) -> (*parseInclude' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %include  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.OPEN, r), s'), sc) -> (*parseOpen' (f, sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %open  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.USE, r), s'), sc) -> (*parseUse' (Stream.Stream.expose s', sc)*)
          Errormsg.warning Errormsg.none ("Warning: "^(Paths.Paths.toString r)^" : %use  declarations are not treated.");
          sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.EOF, _), _), sc) -> sc f
      | (Stream.Stream.Cons ((Lexer.Lexer.RBRACE, _), _), sc) -> sc f
      | (Stream.Stream.Cons ((t,r), s'), sc) ->
          Parsing.Parsing.error (r, "Expected constant name or pragma keyword, found "
                            ^ Lexer.Lexer.toString t)

  and parseConDec' (Stream.Stream.Cons ((_, r0), _) as f, sc) =
    let (conDec, ((Stream.Stream.Cons((_,r'),_)) as f')) = ParseConDec.parseConDec' (f) in
    let r = Paths.Paths.join (r0, r') in
    Stream.Stream.Cons ((ConDec conDec, r), parseStream' (stripDot f', sc))


  let rec parseQ (s) = Stream.Stream.delay (fun () -> parseQ' (Stream.Stream.expose s))
  and parseQ' (f) =
    let (query, f') = ParseQuery.parseQuery' (f) in
    Stream.Stream.Cons (query, parseQ (stripDot (f'))) 
  
 
  
  let parseStream instream = parseTLStream instream

  let parseTerminalQ prompts =  parseQ (Lexer.Lexer.lexTerminal prompts)
end
