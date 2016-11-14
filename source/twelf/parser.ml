(* This is taken from the Twelf implementation *)

  type fileParseResult =
      ConDec of ExtSyn.condec
    | FQuery of ExtSyn.query (* A *)
    (* Further declarations to be added here *)
 
  let stripDot args =
    match args with
        (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.DOT, r), s)) -> s
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RPAREN, r), s)) ->
          Tparsing.Parsing.error (r, "Unexpected right parenthesis")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RBRACE, r), s)) -> 
          Tparsing.Parsing.error (r, "Unexpected right brace")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RBRACKET, r), s)) -> 
          Tparsing.Parsing.error (r, "Unexpected right bracket")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EOF, r), s)) -> 
          Tparsing.Parsing.error (r, "Unexpected end of file")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EQUAL, r), s)) -> 
          Tparsing.Parsing.error (r, "Unexpected `='")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s)) -> 
          Tparsing.Parsing.error (r, "Expected `.', found " ^ Tparsing.Parsing.Lexer'.toString t)
      (* Everything else should be impossible *)

  let rec parseTLStream instream =
    let finish args =
      match args with
          (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EOF, r), s)) -> Tparsing.Parsing.Lexer'.Stream'.Empty
        | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.RBRACE, r), s)) ->
            Tparsing.Parsing.error (r, "Unmatched `}'")
    in
    Tparsing.Parsing.Lexer'.Stream'.delay (fun () -> parseStream'' (Tparsing.Parsing.Lexer'.Stream'.expose(Tparsing.Parsing.Lexer'.lexStream instream), finish))

  and parseStream' (s, sc)  =
    Tparsing.Parsing.Lexer'.Stream'.delay (fun () -> parseStream'' (Tparsing.Parsing.Lexer'.Stream'.expose s, sc))

  (* parseStream'' : lexResult front -> fileParseResult front *)
  (* parseStream'' switches between various specialized parsers *)
  and parseStream'' (f, sc)  =
    match (f, sc) with
        (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (idCase,name), r0), s'), sc) -> parseConDec' (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ABBREV, r), s'), sc) -> 
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %abbrev  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.UNDERSCORE, r), s'), sc) -> parseConDec' (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.INFIX, r), s'), sc) -> 
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : fixity  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.PREFIX, r), s'), sc) -> 
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : fixity  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.POSTFIX, r), s'), sc) -> 
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : fixity  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.NAME, r1), s'), sc) ->
          (*let (namePref, ((Tparsing.Parsing.Lexer'.Stream'.Cons ((_, r2), _)) as f')) = ParseFixity.parseNamePref' f in
          let  r = Paths.join (r1, r2) in
          Tparsing.Parsing.Lexer'.Stream'.Cons ((NamePref namePref, r), parseStream' (stripDot f', sc))*)
          Errormsg.warning Errormsg.none ((Paths.toString r1)^" : %name  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.DEFINE, r), s'), sc) ->
          (*parseSolve' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %define declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.SOLVE, r), s'), sc) ->
          (*parseSolve' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %solve  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.QUERY, r0), s'), sc) ->
          (*let (expected, s1) = parseBound' (Tparsing.Parsing.Lexer'.Stream'.expose s') in
          let (tri, s2) = parseBound' (Tparsing.Parsing.Lexer'.Stream'.expose s1) in
          let (query, ((Tparsing.Parsing.Lexer'.Stream'.Cons((_,r'),_)) as f3)) = ParseQuery.parseQuery' (Tparsing.Parsing.Lexer'.Stream'.expose s2) in
          let r = Paths.join (r0, r') in 
          Tparsing.Parsing.Lexer'.Stream'.Cons ((Query (expected, tri, query), r), parseStream' (stripDot f3, sc))*)
          Errormsg.warning Errormsg.none ((Paths.toString r0)^" : %query  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.FQUERY, r0), s'), sc) ->
          let (query, ((Tparsing.Parsing.Lexer'.Stream'.Cons((_,r'),_)) as f3)) = ParseQuery.parseQuery' (Tparsing.Parsing.Lexer'.Stream'.expose s') in
          let r = Paths.join (r0, r') in
          Tparsing.Parsing.Lexer'.Stream'.Cons ((FQuery query, r), parseStream' (stripDot f3, sc))
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.QUERYTABLED, r0), s'), sc) ->
          (*let (numSol, s1) = parseBound' (Tparsing.Parsing.Lexer'.Stream'.expose s') in
          let (try, s2) = parseBound' (Tparsing.Parsing.Lexer'.Stream'.expose s1) in
          let (query, ((Tparsing.Parsing.Lexer'.Stream'.Cons((_,r'),_)) as f3)) = ParseQuery.parseQuery' (Tparsing.Parsing.Lexer'.Stream'.expose s2) in
          let r = Paths.join (r0, r') in
          Tparsing.Parsing.Lexer'.Stream'.Cons ((Querytabled (numSol, try, query), r), parseStream' (stripDot f3, sc))*)
          Errormsg.warning Errormsg.none ((Paths.toString r0)^" : %querytabled  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.MODE, r), s'), sc) -> (*parseMode' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %mode  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.UNIQUE, r), s'), sc) -> (*parseUnique' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %unique  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.COVERS, r), s'), sc) -> (*parseCovers' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %covers  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.TOTAL, r), s'), sc) -> (*parseTotal' (f, sc) (* -fp *)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %total  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.TERMINATES, r), s'), sc) -> (*parseTerminates' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %terminates  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.BLOCK, r), s'), sc) -> (*parseConDec' (f, sc) (* -cs *)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %block  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.WORLDS, r), s'), sc) -> (*parseWorlds' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %worlds  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.REDUCES, r), s'), sc) -> (*parseReduces' (f, sc) (* -bp *)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %reduces  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.TABLED, r), s'), sc) -> (*parseTabled' (f, sc) (* -bp *)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %tabled  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.KEEPTABLE, r), s'), sc) -> (*parseKeepTable' (f, sc) (* -bp *)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %keepTable  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.THEOREM, r), s'), sc) -> (*parseTheorem' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %theorem  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.PROVE, r), s'), sc) -> (*parseProve' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %prove  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ESTABLISH, r), s'), sc) -> (*parseEstablish' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %establish  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ASSERT, r), s'), sc) -> (*parseAssert' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %assert  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.TRUSTME, r), s'), sc) -> (*parseTrustMe' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %trustme  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.FREEZE, r), s'), sc) -> (*parseFreeze' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %freeze  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.SUBORD, r), s'), sc) -> (*parseSubord' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %subord  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.THAW, r), s'), sc) -> (*parseThaw' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %thaw  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.DETERMINISTIC, r), s'), sc) -> (*parseDeterministic' (f, sc) (* -rv *)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %deterministic  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.COMPILE, r), s'), sc) -> (*parseCompile' (f, sc) (* -ABP 4/4/03 *)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %compile  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.CLAUSE, r), s'), sc) -> (*parseClause' (f, sc) (* -fp *)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %clause  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.SIG, r), s'), sc) -> (*parseSigDef' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %sig  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.STRUCT, r), s'), sc) -> (*parseStructDec' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %struct  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.INCLUDE, r), s'), sc) -> (*parseInclude' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %include  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.OPEN, r), s'), sc) -> (*parseOpen' (f, sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %open  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.USE, r), s'), sc) -> (*parseUse' (Tparsing.Parsing.Lexer'.Stream'.expose s', sc)*)
          Errormsg.warning Errormsg.none ((Paths.toString r)^" : %use  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EOF, _), _), sc) -> sc f
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.RBRACE, _), _), sc) -> sc f
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((t,r), s'), sc) ->
          Tparsing.Parsing.error (r, "Expected constant name or pragma keyword, found "
                            ^ Tparsing.Parsing.Lexer'.toString t)

  and parseConDec' (Tparsing.Parsing.Lexer'.Stream'.Cons ((_, r0), _) as f, sc) =
    let (conDec, ((Tparsing.Parsing.Lexer'.Stream'.Cons((_,r'),_)) as f')) = ParseCondec.parseConDec' (f) in
    let r = Paths.join (r0, r') in
    Tparsing.Parsing.Lexer'.Stream'.Cons ((ConDec conDec, r), parseStream' (stripDot f', sc))


  and parseSkip (f, sc)  =
    match f with
        Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.DOT,_),s') -> 
          parseStream'' ((Tparsing.Parsing.Lexer'.Stream'.expose s'), sc)
      | Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.EOF,_),_) -> 
          sc f 
      | Tparsing.Parsing.Lexer'.Stream'.Cons((t,r),s') -> 
          parseSkip (Tparsing.Parsing.Lexer'.Stream'.expose s', sc)

  let rec parseQ (s) = Tparsing.Parsing.Lexer'.Stream'.delay (fun () -> parseQ' (Tparsing.Parsing.Lexer'.Stream'.expose s))
  and parseQ' (f) =
    let (query, f') = ParseQuery.parseQuery' (f) in
    Tparsing.Parsing.Lexer'.Stream'.Cons (query, parseQ (stripDot (f'))) 
  
 
  
  let parseStream instream = parseTLStream instream

  let parseTerminalQ prompts =  parseQ (Tparsing.Parsing.Lexer'.lexTerminal prompts)
