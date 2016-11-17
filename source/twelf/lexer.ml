(* This is taken from the Twelf implementation *)

module type LEXER = 
sig
  (* Stream is not memoizing for efficiency *)
  (* module Stream : STREAM *)
  
  module Stream' : Tstream.STREAM

  (*! structure Paths : PATHS !*)
  type idCase =
      Upper				(* [A-Z]<id> or _<id> *)
    | Lower				(* any other <id> *)
    | Quoted				(* '<id>', currently unused *)

  type token =
      EOF				(* end of file or stream, also `%.' *)
    | DOT				(* `.' *)
    | PATHSEP                           (* `.' between <id>s *)
    | COLON				(* `:' *)
    | LPAREN | RPAREN			(* `(' `)' *)
    | LBRACKET | RBRACKET		(* `[' `]' *)
    | LBRACE | RBRACE			(* `{' `}' *)
    | BACKARROW | ARROW			(* `<-' `->' *)
    | TYPE				(* `type' *)
    | EQUAL				(* `=' *)
    | ID of idCase * string		(* identifer *)
    | UNDERSCORE			(* `_' *)
    | INFIX | PREFIX | POSTFIX		(* `%infix' `%prefix' `%postfix' *)
    | NAME				(* `%name' *)
    | DEFINE				(* `%define' *) (* -rv 8/27/01 *)
    | SOLVE				(* `%solve' *)
    | QUERY				(* `%query' *)
    | FQUERY				(* `%fquery' *)
    | COMPILE                           (* '%compile' *) (* -ABP 4/4/03 *)
    | QUERYTABLED			(* `%querytabled' *)
    | MODE				(* `%mode' *)
    | UNIQUE				(* `%unique' *) (* -fp 8/17/03 *)
    | COVERS				(* `%covers' *) (* -fp 3/7/01 *)
    | TOTAL				(* `%total' *) (* -fp 3/18/01 *)
    | TERMINATES       			(* `%terminates' *)
    | BLOCK				(* `%block' *) (* -cs 5/29/01 *)
    | WORLDS       			(* `%worlds' *)
    | REDUCES       			(* `%reduces' *) (* -bp 6/5/99 *)
    | TABLED       			(* `%tabled' *)  (* -bp 6/5/99 *)
    | KEEPTABLE       			(* `%keepTable' *)  (* -bp 04/11/04 *)
    | THEOREM                           (* `%theorem' *)
    | PROVE                             (* `%prove' *)
    | ESTABLISH				(* `%establish' *)
    | ASSERT				(* `%assert' *)
    | ABBREV				(* `%abbrev' *)
    | TRUSTME			        (* `%trustme' *)
    | FREEZE                            (* `%freeze' *)
    | THAW				(* `%thaw' *)
    | SUBORD				(* `%subord' *) (* -gaw 07/11/08 *)
    | DETERMINISTIC                     (* `%deterministic' *) (* -rv 11/27/01 *)
    | CLAUSE				(* `%clause' *) (* -fp 8/9/02 *)
    | SIG                               (* `%sig' *)
    | STRUCT                            (* `%struct' *)
    | WHERE                             (* `%where' *)
    | INCLUDE                           (* `%include' *)
    | OPEN                              (* `%open' *)
    | USE                               (* `%use'    *)
    | STRING of string                  (* string constants *)

  exception Error of string

  (* lexer returns an infinite stream, terminated by EOF token *)
  val lexStream : in_channel -> (token * Paths.region) Stream'.stream
  val lexTerminal : string * string -> (token * Paths.region) Stream'.stream

  val toString : token -> string

  (* Utilities *) 
  exception NotDigit of char
  val stringToNat : string -> int
  val isUpper : string -> bool

end  (* signature LEXER *)

module LexerFunc (S : Tstream.STREAM) : LEXER =  
struct
  (* let module Stream = Stream' *)
  (*! structure Paths = Paths' !*)  

  module Stream' : Tstream.STREAM = S

(* let module Paths = P *)
  type idCase =
      Upper				(* [A-Z]<id> or _<id> *)
    | Lower				(* any other <id> *)
    | Quoted				(* '<id>', currently unused *)

  type token =
      EOF				(* end of file or stream, also `%.' *)
    | DOT				(* `.' *)
    | PATHSEP                           (* `.' between <id>s *)
    | COLON				(* `:' *)
    | LPAREN | RPAREN			(* `(' `)' *)
    | LBRACKET | RBRACKET		(* `[' `]' *)
    | LBRACE | RBRACE			(* `{' `}' *)
    | BACKARROW | ARROW			(* `<-' `->' *)
    | TYPE				(* `type' *)
    | EQUAL				(* `=' *)
    | ID of idCase * string		(* identifer *)
    | UNDERSCORE			(* `_' *)
    | INFIX | PREFIX | POSTFIX		(* `%infix' `%prefix' `%postfix' *)
    | NAME				(* `%name' *)
    | DEFINE				(* `%define' *) (* -rv 8/27/01 *)
    | SOLVE				(* `%solve' *)
    | QUERY	  			(* `%query' *)
    | FQUERY	  			(* `%fquery' *)
    | COMPILE                           (* '%compile' *) (* -ABP 4/4/03 *)
    | QUERYTABLED  			(* `%querytabled *)
    | MODE				(* `%mode' *)
    | UNIQUE				(* `%unique' *) (* -fp 8/17/03 *)
    | COVERS				(* `%covers' *) (* -fp 3/7/01 *)
    | TOTAL				(* `%total' *) (* -fp 3/18/01 *)
    | TERMINATES			(* `%terminates' *)
    | BLOCK				(* `%block' *) (* -cs 5/29/01 *)
    | WORLDS                            (* `%worlds' *)
    | REDUCES                           (* `%reduces' *) (* -bp  6/05/99 *)
    | TABLED                            (* `%tabled' *)     (* -bp 11/20/01 *)
    | KEEPTABLE                         (* `%keepTable' *)  (* -bp 11/20/01 *)
    | THEOREM                           (* `%theorem' *)
    | PROVE                             (* `%prove' *)
    | ESTABLISH				(* `%establish' *)
    | ASSERT				(* `%assert' *)
    | ABBREV				(* `%abbrev' *)
    | TRUSTME                           (* `%trustme' *) (* -fp 8/26/05 *)
    | FREEZE                            (* `%freeze' *)
    | THAW				(* `%thaw' *)
    | SUBORD				(* `%subord' *) (* -gaw 07/11/08 *)
    | DETERMINISTIC                     (* `%deterministic' *) (* -rv 11/27/01 *)
    | CLAUSE				(* `%clause' *) (* -fp 8/9/02 *)
    | SIG                               (* `%sig' *)
    | STRUCT                            (* `%struct' *)
    | WHERE                             (* `%where' *)
    | INCLUDE                           (* `%include' *)
    | OPEN                              (* `%open' *)
    | USE                               (* `%use' *)
    | STRING of string                  (* string constants *)

  exception Error of string

  let error (r, msg) = raise (Error(Paths.wrap (r, msg)))

  (* isSym (c) = B iff c is a legal symbolic identifier constituent *)
  (* excludes quote character and digits, which are treated specially *)
  (* Char.contains stages its computation *)
  let symbols = ['_';'!';'&';'$';'^';'+';'/';'<';'=';'>';'?';'@';'~';'|';'#';'*';'`';';';',';'-';'\\'] 
  let isSym : char -> bool = fun c1 -> (List.exists (fun c2 -> (Char.compare c1 c2) = 0) symbols)

  (* isUFT8 (c) = assume that if a character is not ASCII it must be
     part of a UTF8 Unicode encoding.  Treat these as lowercase
     identifiers.  Somewhat of a hack until there is native Unicode
     string support. *)
  let isUTF8 (c) = let code = (Char.code c) in not (0 <= code && code <= 127)

  (* isQuote (c) = B iff c is the quote character *)
  let isQuote (c) = (Char.compare c '\'') = 0

  let space = [' ';'\n';'\t';'\011';'\012';'\r']
  let isSpace (c) = List.exists (fun c2 -> (Char.compare c c2) = 0) space

  let isLower (c) = let code = Char.code c in 97 <= code && code <= 122

  let char_isUpper (c) = let code = Char.code c in 65 <= code && code <= 90

  let isDigit (c) = let code = Char.code c in 48 <= code && code <= 57

  (* isIdChar (c) = B iff c is legal identifier constituent *)
  let isIdChar (c) = isLower(c) || char_isUpper (c)
                     || isDigit (c) || isSym(c)
                     || isQuote (c) || isUTF8(c)

  (* stringToToken (idCase, string, region) = (token, region)
     converts special identifiers into tokens, returns ID token otherwise
  *)
  let stringToToken args =
    match args with
        (Lower, "<-", r) -> (BACKARROW, r)
      | (Lower, "->", r) -> (ARROW, r)
      | (Upper, "_", r) -> (UNDERSCORE, r)
      | (Lower, "=", r) -> (EQUAL, r)
      | (Lower, "type", r) -> (TYPE, r)
      | (idCase, s, r) -> (ID(idCase,s), r)

  (* lex (inputFun) = (token, region) stream

     inputFun maintains state, reading input one line at a time and
     returning a string terminated by <newline> each time.
     The end of the stream is signalled by a string consisting only of ^D
     Argument to inputFun is the character position.
  *)
  let lex (inputFun : int -> string) =
    let s = ref "" in			(* current string (line) *)
    let left = ref 0 in			(* position of first character in s *)
    let right = ref 0 in			(* position after last character in s *)
    let _ = Paths.resetLines () in   	(* initialize line counter *)

      (* neither lexer nor parser should ever try to look beyond EOF *)
    (* I think EOF is 26 is ASCII ... *)
    let eofString = Char.escaped '\026' in

      (* readNext () = ()
         Effect: read the next line, updating s, left, and right

         readNext relies on the invariant that identifiers are never
         spread across lines
      *)
    let readNext () =
      let nextLine = inputFun (!right) in
      let nextSize = String.length (nextLine) in
      if nextSize = 0		(* end of file? *)
      then (s := eofString;	(* fake EOF character string *)
        left := !right;
        right := !right + 1)
      else (s := nextLine;
        left := !right;
        right := !right + nextSize;
        Paths.newLine (!left)) (* remember new line position *)
    in
      (* char (i) = character at position i
         Invariant: i >= !left
	 Effects: will read input if i >= !right
      *)
    let rec char (i) =
      if i >= !right then (readNext (); char (i))
      else (!s).[(i - !left)]
    in
      (* string (i,j) = substring at region including i, excluding j
         Invariant: i >= !left and i < j and j < !right
                    Note that the relevant parts must already have been read!
	 Effects: None
      *)
    let string (i,j) = 
(*      print_endline ("String.sub: string= "^(!s)^" start= "^(string_of_int (i - !left))^" len= "^(string_of_int (j-i))); *)
      String.sub (!s) (i - !left) (j-i) in
    
    (* The remaining functions do not access the state or *)
    (* stream directly, using only functions char and string *)

    let idToToken (idCase, Paths.Reg (i,j)) = stringToToken (idCase, string (i,j), Paths.Reg (i,j)) in

    (* Quote characters are part of the name *)
    (* Treat quoted identifiers as lowercase, since they no longer *)
    (* override infix state.  Quoted identifiers are now only used *)
    (* inside pragmas *)
    let qidToToken (Paths.Reg (i,j)) = (ID(Lower, string(i,j+1)), Paths.Reg (i,j+1)) in

    (* The main lexing functions take a character c and the next
       input position i and return a token with its region
       The name convention is lexSSS, where SSS indicates the state
       of the lexer (e.g., what has been lexed so far).

       Lexing errors are currently fatal---some error recovery code is
       indicated in comments.
    *)
    let rec lexInitial (args : (char * int))=
      match args with
          (':', i) -> (COLON, Paths.Reg (i-1,i))
        | ('.', i) -> (DOT, Paths.Reg (i-1,i))
        | ('(', i) -> (LPAREN, Paths.Reg (i-1,i))
        | (')', i) -> (RPAREN, Paths.Reg (i-1,i))
        | ('[', i) -> (LBRACKET, Paths.Reg (i-1,i))
        | (']', i) -> (RBRACKET, Paths.Reg (i-1,i))
        | ('{', i) -> (LBRACE, Paths.Reg (i-1,i))
        | ('}', i) -> (RBRACE, Paths.Reg (i-1,i))
        | ('%', i) -> lexPercent (char(i), i+1)
        | ('_', i) -> lexID (Upper, Paths.Reg (i-1,i))
        | ('\'', i) -> lexID (Lower, Paths.Reg (i-1,i)) (* lexQUID (i-1,i) *)
        | ('\026', i) -> (EOF, Paths.Reg (i-1,i-1))
        | ('"', i) -> lexString (Paths.Reg(i-1, i))
        | (c, i) ->
  	  if isSpace (c) then (lexInitial (char (i),i+1))
	  else if char_isUpper(c) then lexID (Upper, Paths.Reg (i-1,i))
 	  else if isDigit(c) then lexID (Lower, Paths.Reg (i-1,i))
	  else if isLower(c) then lexID (Lower, Paths.Reg (i-1,i))
	  else if isSym(c) then lexID (Lower, Paths.Reg (i-1,i))
	  else if isUTF8(c) then lexID (Lower, Paths.Reg (i-1,i))
          else error (Paths.Reg (i-1,i), "Illegal character " ^ Char.escaped (c))
          (* recover by ignoring: lexInitial (char(i), i+1) *)

    and lexID (idCase, Paths.Reg (i,j)) =
      let rec lexID' (j) =
        if isIdChar (char(j)) then lexID' (j+1)
 	else 
          idToToken (idCase, Paths.Reg (i,j))
      in
      lexID' (j)

    (* lexQUID is currently not used --- no quoted identifiers *)
    and lexQUID (Paths.Reg (i,j)) =
      if isSpace (char(j))
      then error (Paths.Reg (i,j+1), "Whitespace in quoted identifier")
         (* recover by adding implicit quote? *)
         (* qidToToken (i, j) *)
      else if isQuote (char(j)) then qidToToken (Paths.Reg (i,j))
           else lexQUID (Paths.Reg (i, j+1)) 

    and lexPercent args =
      match args with
          ('.', i) -> (EOF, Paths.Reg (i-2,i))
        | ('{', i) -> lexPercentBrace (char(i), i+1)
        | ('%', i) -> lexComment ('%', i)
        | (c, i) ->
          if isIdChar(c) then lexPragmaKey (lexID (Quoted, Paths.Reg (i-1, i)))
	  else if isSpace(c) then lexComment (c, i)
	       else error (Paths.Reg (i-1, i), "Comment character `%' not followed by white space")

    and lexPragmaKey args =
      match args with
          (ID(_, "infix"), r) -> (INFIX, r)
        | (ID(_, "prefix"), r) -> (PREFIX, r)
        | (ID(_, "postfix"), r) -> (POSTFIX, r)
        | (ID(_, "mode"), r) -> (MODE, r)
        | (ID(_, "unique"), r) -> (UNIQUE, r) (* -fp 8/17/03 *)
        | (ID(_, "terminates"), r) -> (TERMINATES, r)
        | (ID(_, "block"), r) -> (BLOCK, r) (* -cs 6/3/01 *)
        | (ID(_, "worlds"), r) -> (WORLDS, r)
        | (ID(_, "covers"), r) -> (COVERS, r)
        | (ID(_, "total"), r) -> (TOTAL, r) (* -fp 3/18/01 *)
        | (ID(_, "reduces"), r) -> (REDUCES, r)         (* -bp 6/5/99 *)
        | (ID(_, "tabled"), r) -> (TABLED, r)           (* -bp 20/11/01 *)
        | (ID(_, "keepTable"), r) -> (KEEPTABLE, r)     (* -bp 20/11/04 *)
        | (ID(_, "theorem"), r) -> (THEOREM, r)
        | (ID(_, "prove"), r) -> (PROVE, r)
        | (ID(_, "establish"), r) -> (ESTABLISH, r)
        | (ID(_, "assert"), r) -> (ASSERT, r)
        | (ID(_, "abbrev"), r) -> (ABBREV, r)
        | (ID(_, "name"), r) -> (NAME, r)
        | (ID(_, "define"), r) -> (DEFINE, r) (* -rv 8/27/01 *)
        | (ID(_, "solve"), r) -> (SOLVE, r)
        | (ID(_, "query"), r) -> (QUERY, r)
        | (ID(_, "fquery"), r) -> (FQUERY, r)
        | (ID(_, "compile"), r) -> (COMPILE, r) (* -ABP 4/4/03 *)
        | (ID(_, "querytabled"), r) -> (QUERYTABLED, r)
        | (ID(_, "trustme"), r) -> (TRUSTME, r)
        | (ID(_, "subord"), r) -> (SUBORD, r) (* -gaw 07/11/08 *)
        | (ID(_, "freeze"), r) -> (FREEZE, r)
        | (ID(_, "thaw"), r) -> (THAW, r)
        | (ID(_, "deterministic"), r) -> (DETERMINISTIC, r) (* -rv 11/27/01 *)
        | (ID(_, "clause"), r) -> (CLAUSE, r) (* -fp 08/09/02 *)
        | (ID(_, "sig"), r) -> (SIG, r)
        | (ID(_, "struct"), r) -> (STRUCT, r)
        | (ID(_, "where"), r) -> (WHERE, r)
        | (ID(_, "include"), r) -> (INCLUDE, r)
        | (ID(_, "open"), r) -> (OPEN, r)
        | (ID(_, "use"), r) -> (USE, r)
        | (ID(_, s), r) ->
          error (r, "Unknown keyword %" ^ s ^ " (single line comment starts with `%<whitespace>' or `%%')")
        (* comments are now started by %<whitespace> *)
        (*
        | lexPragmaKey (_, (_,j)) = lexComment (char(j), j+1)
        *)

    and lexComment args =
      match args with
          ('\n', i) -> lexInitial (char(i), i+1)
        | ('%', i) -> lexCommentPercent (char(i), i+1)
        | ('\026', i) ->
            error (Paths.Reg (i-1, i-1), "Unclosed single-line comment at end of file")
	    (* recover: (EOF, (i-1,i-1)) *)
        | (c, i) -> lexComment (char(i), i+1)

    and lexCommentPercent args =
      match args with
          ('.', i) -> (EOF, Paths.Reg (i-2, i))
        | (c, i) -> lexComment (c, i)

    and lexPercentBrace (c, i) = lexDComment (c, 1, i)

    (* functions lexing delimited comments below take nesting level l *)
    and lexDComment args =
      match args with
          ('}', l, i) -> lexDCommentRBrace (char(i), l, i+1)
        | ('%', l, i) -> lexDCommentPercent (char(i), l, i+1)
        | ('\026', l, i) ->
            (* pass comment beginning for error message? *)
            error (Paths.Reg (i-1,i-1), "Unclosed delimited comment at end of file")
	    (* recover: (EOF, (i-1,i-1)) *)
        | (c, l, i) -> lexDComment (char(i), l, i+1)

    and lexDCommentPercent args =
      match args with
          ('{', l, i) -> lexDComment (char(i), l+1, i+1)
        | ('.', l, i) ->
            error (Paths.Reg (i-2, i), "Unclosed delimited comment at end of file token `%.'")
            (* recover: (EOF, (i-2,i)) *)
        | (c, l, i) -> lexDComment (c, l, i)

    and lexDCommentRBrace args =
      match args with
          ('%', 1, i) -> lexInitial (char(i), i+1)
        | ('%', l, i) -> lexDComment (char(i), l-1, i+1)
        | (c, l, i) -> lexDComment (c, l, i)

    and lexString (Paths.Reg(i, j)) =
          (match char(j) with
               ('"') -> (STRING (string (i, j+1)), Paths.Reg(i, j+1))
             | ('\n') ->
                  error (Paths.Reg (i-1, i-1), "Unclosed string constant at end of line")
	          (* recover: (EOL, (i-1,i-1)) *)
             | ('\026') ->
                  error (Paths.Reg (i-1, i-1), "Unclosed string constant at end of file")
                  (* recover: (EOF, (i-1,i-1)) *)
             | _ -> lexString (Paths.Reg(i, j+1)))
    in
    let rec lexContinue (j) = Stream'.delay (fun () -> lexContinue' (j))
    and lexContinue' (j) = lexContinue'' (lexInitial (char(j), j+1))

    and lexContinue'' mt =
      match mt with
          (ID _, Paths.Reg (i,j)) ->
            Stream'.Cons (mt, lexContinueQualId (j))
        | (token, Paths.Reg (i,j)) ->
            Stream'.Cons (mt, lexContinue (j))

    and lexContinueQualId (j) =
          Stream'.delay (fun () -> lexContinueQualId' (j))
    and lexContinueQualId' (j) =
          if (Char.compare (char (j)) '.') = 0
          then if (isIdChar (char (j+1)))
                then (Stream'.Cons((PATHSEP, Paths.Reg (j,j+1)), lexContinue (j+1)))
                else (Stream'.Cons((DOT, Paths.Reg (j,j+1)), lexContinue (j+1)))
          else lexContinue' (j)

  in
    lexContinue (0)
  (* fun lex (inputFun) = let ... in ... end *)

  (* MKS: add this to avoid implementing Compat *)
  let inputLine97 instream = 
    try ((input_line instream)^"\n")
    with
      End_of_file -> "%."

(*  let lexStream (instream) = lex (fn i => Compat.inputLine97 (instream)) *)
  let lexStream instream = lex (fun i ->  inputLine97 instream)

  let lexTerminal (prompt0, prompt1) =
        lex (fun n ->
               match n with
                   0 -> 
                     let _ = print_string (prompt0); flush stdout in
		     inputLine97 (stdin)
	         | i -> 
                     let _ = print_string (prompt1); flush stdout in
		     inputLine97 (stdin))

  let toString' token =
    match token with
        (DOT) -> "."
      | (PATHSEP) -> "."
      | (COLON) -> ":"
      | (LPAREN) -> "("
      | (RPAREN) -> ")"
      | (LBRACKET) -> "["
      | (RBRACKET) -> "]"
      | (LBRACE) -> "{"
      | (RBRACE) -> "}"
      | (BACKARROW) -> "<-"
      | (ARROW) -> "->"
      | (TYPE) -> "type"
      | (EQUAL) -> "="
      | (UNDERSCORE) -> "_"
      | (INFIX) -> "%infix"
      | (PREFIX) -> "%prefix"
      | (POSTFIX) -> "%postfix"
      | (NAME) -> "%name"
      | (DEFINE) -> "%define"    (* -rv 8/27/01 *)
      | (SOLVE) -> "%solve"
      | (QUERY) -> "%query"
      | (FQUERY) -> "%fquery"
      | (COMPILE) -> "%compile"  (* -ABP 4/4/03 *)
      | (QUERYTABLED) -> "%querytabled"
      | (MODE) -> "%mode"
      | (UNIQUE) -> "%unique"
      | (COVERS) -> "%covers"
      | (TOTAL) -> "%total"
      | (TERMINATES) -> "%terminates"
      | (BLOCK) -> "%block"	(* -cs 6/3/01. *)
      | (WORLDS) -> "%worlds"
      | (REDUCES) -> "%reduces"              (*  -bp 6/5/99. *)
      | (TABLED) -> "%tabled"                (*  -bp 20/11/01. *)
      | (KEEPTABLE) -> "%keepTable"          (*  -bp 04/11/03. *)
      | (THEOREM) -> "%theorem"
      | (PROVE) -> "%prove"
      | (ESTABLISH) -> "%establish"
      | (ASSERT) -> "%assert"
      | (ABBREV) -> "%abbrev"
      | (TRUSTME) -> "%trustme"
      | (SUBORD) -> "%subord"
      | (FREEZE) -> "%freeze"
      | (THAW) -> "%thaw"
      | (DETERMINISTIC) -> "%deterministic"  (* -rv 11/27/01. *)
      | (CLAUSE) -> "%clause" (* -fp 08/09/02 *)
      | (SIG) -> "%sig"
      | (STRUCT) -> "%struct"
      | (WHERE) -> "%where"
      | (INCLUDE) -> "%include"
      | (OPEN) -> "%open"
      | (USE) -> "%use"

 let toString arg =
   match arg with
       (ID(_,s)) -> "identifier `" ^ s ^ "'"
     | (EOF) -> "end of file or `%.'"
     | (STRING(s)) -> "constant string " ^ s
     | (token) -> "`" ^ toString' token ^ "'"

 exception NotDigit of char

 (* charToNat(c) = n converts character c to decimal equivalent *)
 (* raises NotDigit(c) if c is not a digit 0-9 *)
 let charToNat (c) =
     let digit = Char.code(c) - Char.code('0')
     in
       if digit < 0 || digit > 9
	 then raise (NotDigit(c))
       else digit
    

 (* stringToNat(s) = n converts string s to a natural number *)
 (* raises NotDigit(c) if s contains character c which is not a digit *)
 let stringToNat (s) =
     let l = String.length s in
     let rec stn (i, n) =
       if i = l then n
       else stn (i+1, 10 * n + (charToNat s.[i]))
     in
       stn (0, 0)


  (* isUpper (s) = true, if s is a string starting with an uppercase
     letter or underscore (_).
  *)
  let isUpper str = 
    match str with
        ("") -> false
      | (s) ->
        let  c = s.[0] in
	 char_isUpper c || (Char.compare c '_') = 0

end  (* functor Lexer *)



module Lexer =
  LexerFunc (Tstream.Stream)

