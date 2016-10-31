{
open Lexing
open Twelfparser

type IdCase = Upper | Lower

let setFileName lexbuf name =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name }

let incrline lexbuf =
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with
      pos_bol = lexbuf.lex_curr_p.pos_cnum ;
      pos_lnum = 1 + lexbuf.lex_curr_p.pos_lnum }

let stringToToken case id =
  match (case, id) with
      (Lower, "<-") -> BACKARROW
    | (Lower, "->") -> ARROW
    | (Upper, "_") -> UNDERSCORE
    | (Lower, "=") -> EQUAL
    | (Lower, "type") -> TYPE
    | _ -> ID(case,s)

let commentLev = ref 0

}

let DIGIT = ['0'-'9']
let LOWER = ['a'-'z']
let UPPER = ['A'-'Z' '_']
let SYMB  = ['_' '!' '&' '^' '+' '/' '<' '=' '>' '?' '@' '~' '|' '#' '*' '`' ';' ',' '-' '\\']
let IDCHAR = (DIGIT | LOWER | UPPER | SYMB | "'")

rule initial = parse
| '\n'                                  {incrline lexbuf; initial lexbuf}
| ":"                                   {COLON}
| "."                                   {DOT}
| "("                                   {LPAREN}
| ")"                                   {RPAREN}
| "["                                   {LBRACKET}
| "]"                                   {RBRACKET}
| "{"                                   {LBRACE}
| "}"                                   {RBRACE}
| "%"                                   {lexPercent lexbuf}
| ("_" IDCHAR*) as id                   {stringToToken Upper id}
| ("'" IDCHAR*) as id                   {stringToToken Lower id}
| eof                                   {EOF}
| "\""                                  {lexString lexbuf}
| ' '                                   {initial lexbuf}
| (UPPER IDCHAR*) as id                 {stringToToken Upper id}
| ((DIGIT|LOWER|SYMB) IDCHAR*) as id    {stringToToken Lower id}
| _ as c                                {Errormsg.error lexbuf.lex_curr_p ("Illegal character " ^ c)} (* what to do on error? *)

and lexString = parse
| ([^ '"' '\n']* '"') as s           {STRING(s)}
| ([^ '"' '\n']* '\n') as s          {Errormsg.error lexbuf.lex_curr_p "Unclosed string constant at end of line"}
| ([^ '"' '\n']* eof) as s           {Errormsg.error lexbuf.lex_curr_p "Unclosed string constant at end of file"}

and lexPercent = parse
| "."                                   {EOF}
| "{"                                   {commentLev := 1; lexDComment lexbuf}
| "%"                                   {lexComment lexbuf}
| "infix"                               {INFIX}
| "prefix"                              {PREFIX}
| "postfix"                             {POSTFIX}
| "mode"                                {MODE}
| "unique"                              {UNIQUE}
| "terminates"                          {TERMINATES}
| "block"                               {BLOCK}
| "worlds"                              {WORLDS}
| "covers"                              {COVERS}
| "total"                               {TOTAL}
| "reduces"                             {REDUCES}
| "tabled"                              {TABLED}
| "keepTable"                           {KEEPTABLE}
| "theorem"                             {THEOREM}
| "prove"                               {PROVE}
| "establish"                           {ESTABLISH}
| "assert"                              {ASSERT}
| "abbrev"                              {ABBREV}
| "name"                                {NAME}
| "define"                              {DEFINE}
| "solve"                               {SOLVE}
| "query"                               {QUERY}
| "fquery"                              {FQUERY}
| "compile"                             {COMPILE}
| "querytabled"                         {QUERYTABLED}
| "trustme"                             {TRUSTME}
| "subord"                              {SUBORD}
| "freeze"                              {FREEZE}
| "thaw"                                {THAW}
| "deterministic"                       {DETERMINISTIC}
| "clause"                              {CLAUSE}
| "sig"                                 {SIG}
| "struct"                              {STRUCT}
| "where"                               {WHERE}
| "include"                             {INCLUDE}
| "open"                                {OPEN}
| "use"                                 {USE}
| _ as s                                {Errormsg.error lexbuf.lex_curr_p 
                                                        ("Unknown keyword %"^ s ^
                                                         " (single line comment starts with `%<whitespace>' or `%%')")}

and lexComment = parse
| '\n'                                  {incrline lexbuf; initial lexbuf}
| "%."                                  {EOF}
| eof                                   {Errormsg.error lexbuf.lex_curr_p "Unclosed single-line comment at end of file"}
| _ as c                                {lexComment lexbuf}

and lexDComment = parse
| "%}"                                  {decr commentLev; if (!commentLev = 0) then initial lexbuf else lexDComment lexbuf}
| "%{"                                  {incr commentLev; lexDComment lexbuf}
| "%."                                  {Errormsg.error lexbuf.lex_curr_p "Unclosed delimited comment at end of file token `%.'"}
| eof                                   {Errormsg.error lexbuf.lex_curr_p "Unclosed delimited comment at end of file."}
| '\n'                                  {incrline lexbuf; lexDComment lexbuff}
| _ as c                                {lexDComment lexbuf}
