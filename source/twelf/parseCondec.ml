(* This is taken from the Twelf implementation *)

module type PARSE_CONDEC =
sig
  val parseConDec' : ExtSyn.condec Parsing.Parsing.parser
end  (* signature PARSE_CONDEC *)


module ParseConDec (ParseTerm : ParseTerm.PARSE_TERM)  : PARSE_CONDEC = 
struct

  (* parseConDec2  "= U" | "" *)
  let parseConDec2 args =
    match args with
        (Some(name), (tm, f)) ->
          ((ExtSyn.condec(name, tm)), f)
      | (None, (tm, Stream.Stream.Cons((t,r),s'))) ->
          Parsing.Parsing.error (r, "Illegal anonymous declared constant")


    (* parseConDec1  ": V = U" | "= U" *)
  let parseConDec1 args =
    match args with
        (optName, Stream.Stream.Cons ((Lexer.Lexer.COLON, r), s')) ->
          parseConDec2 (optName, ParseTerm.parseTerm' (Stream.Stream.expose s'))
      | (optName, Stream.Stream.Cons ((t,r), s')) ->
        Parsing.Parsing.error (r, "Expected `:', found " ^ Lexer.Lexer.toString t)

  (* parseConDec' : lexResult front -> ExtConDec.ConDec * lexResult front
     Invariant: first token in exposed input stream is an identifier or underscore
  *)
  let parseConDec' args =
    match args with
        (Stream.Stream.Cons ((Lexer.Lexer.ID (idCase,name), r), s')) ->
          parseConDec1 (Some(name), Stream.Stream.expose s')
      | (Stream.Stream.Cons ((t, r), s')) ->
          Parsing.Parsing.error (r, "Constant or block declaration expected, found token " ^ Lexer.Lexer.toString t)


end
