(* This is taken from the Twelf implementation *)

  (* parseConDec2  "= U" | "" *)
  let parseConDec2 args =
    match args with
        (Some(name), (tm, f)) ->
          ((ExtSyn.condec(name, tm)), f)
      | (None, (tm, Parsing.Parsing.Lexer'.Stream'.Cons((t,r),s'))) ->
          Parsing.Parsing.error (r, "Illegal anonymous declared constant")


    (* parseConDec1  ": V = U" | "= U" *)
  let parseConDec1 args =
    match args with
        (optName, Parsing.Parsing.Lexer'.Stream'.Cons ((Parsing.Parsing.Lexer'.COLON, r), s')) ->
          parseConDec2 (optName, ParseTerm.parseTerm' (Parsing.Parsing.Lexer'.Stream'.expose s'))
      | (optName, Parsing.Parsing.Lexer'.Stream'.Cons ((t,r), s')) ->
        Parsing.Parsing.error (r, "Expected `:', found " ^ Parsing.Parsing.Lexer'.toString t)

  (* parseConDec' : lexResult front -> ExtConDec.ConDec * lexResult front
     Invariant: first token in exposed input stream is an identifier or underscore
  *)
  let parseConDec' args =
    match args with
        (Parsing.Parsing.Lexer'.Stream'.Cons ((Parsing.Parsing.Lexer'.ID (idCase,name), r), s')) ->
          parseConDec1 (Some(name), Parsing.Parsing.Lexer'.Stream'.expose s')
      | (Parsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) ->
          Parsing.Parsing.error (r, "Constant or block declaration expected, found token " ^ Parsing.Parsing.Lexer'.toString t)


