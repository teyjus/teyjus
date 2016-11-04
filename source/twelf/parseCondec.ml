(* This is taken from the Twelf implementation *)

  (* parseConDec2  "= U" | "" *)
  let parseConDec2 args =
    match args with
        (Some(name), (tm, f)) ->
          ((ExtSyn.condec(name, tm)), f)
      | (None, (tm, Tparsing.Parsing.Lexer'.Stream'.Cons((t,r),s'))) ->
          Tparsing.Parsing.error (r, "Illegal anonymous declared constant")


    (* parseConDec1  ": V = U" | "= U" *)
  let parseConDec1 args =
    match args with
        (optName, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.COLON, r), s')) ->
          parseConDec2 (optName, ParseTerm.parseTerm' (Tparsing.Parsing.Lexer'.Stream'.expose s'))
      | (optName, Tparsing.Parsing.Lexer'.Stream'.Cons ((t,r), s')) ->
        Tparsing.Parsing.error (r, "Expected `:', found " ^ Tparsing.Parsing.Lexer'.toString t)

  (* parseConDec' : lexResult front -> ExtConDec.ConDec * lexResult front
     Invariant: first token in exposed input stream is an identifier or underscore
  *)
  let parseConDec' args =
    match args with
        (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (idCase,name), r), s')) ->
          parseConDec1 (Some(name), Tparsing.Parsing.Lexer'.Stream'.expose s')
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) ->
          Tparsing.Parsing.error (r, "Constant or block declaration expected, found token " ^ Tparsing.Parsing.Lexer'.toString t)


