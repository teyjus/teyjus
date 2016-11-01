(* This is taken from the Twelf implementation *)


module type PARSING = 
sig
  (*
  structure Lexer : LEXER
    sharing Lexer.Stream = Stream
  *)
  module Lexer' : Lexer.LEXER


  type lexResult = (Lexer'.token * Paths.region)

  type 'a parser = lexResult Lexer'.Stream'.front -> 'a * lexResult Lexer'.Stream'.front

  (* recursive parser (allows parsing functions that need to parse
     a signature expression to temporarily suspend themselves) *)
  type 'a recParseResult =
    Done of 'a
  | Continuation of 'a recParseResult parser

  type 'a recparser = 'a recParseResult parser

  (* useful combinator for recursive parsers *)
  val recwith : 'a recparser * ('a -> 'b) -> 'b recparser

  exception Error of string
  val error : Paths.region * string -> 'a	(* always raises Error *)
end (* signature PARSING *)

module ParsingFunc : functor (L : Lexer.LEXER) -> PARSING

module Parsing : PARSING 
