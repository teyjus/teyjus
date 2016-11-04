(* This is taken from the Twelf implementation *)

  let returnQuery (optName, (tm, f)) = (ExtSyn.query (optName, tm), f)

  (* parseQuery1 (name, f, f')   ": A" from f' or "V" from f. *)

  let parseQuery1 args =
    match args with
        (name, f, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.COLON, r), s')) ->
          returnQuery (Some(name), ParseTerm.parseTerm' (Tparsing.Parsing.Lexer'.Stream'.expose s'))
      | (name, f, _) -> returnQuery (None, ParseTerm.parseTerm' f)

  (* parseQuery' : lexResult front -> ExtQuery.query * lexResult front *)
  (* parseQuery'  "X : A" | "A" *)

  (* Query parsing is ambiguous, since a term "V" might have the form "U' : V'" *)
  (* We look for an uppercase variable X followed by a `:'.
     If we find this, we parse a query of the form "X : A".
     Otherwise we parse a query of the form "A".
  *)
  let parseQuery' f =
    match f with
       (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (Tparsing.Parsing.Lexer'.Upper, name), r), s')) ->
         parseQuery1 (name, f, Tparsing.Parsing.Lexer'.Stream'.expose s')
     | f ->
         returnQuery (None, ParseTerm.parseTerm' f)
