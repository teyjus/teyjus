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

module LexerFunc : functor (Stream' : Tstream.STREAM) -> LEXER

module Lexer : LEXER

