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

module ParsingFunc (L : Lexer.LEXER) : PARSING =
struct

  module Lexer' : Lexer.LEXER = L

  type lexResult = Lexer'.token * Paths.region

  type 'a parser = lexResult Lexer'.Stream'.front -> 'a * lexResult Lexer'.Stream'.front

  type 'a recParseResult =
    Done of 'a
  | Continuation of 'a recParseResult parser

  type 'a recparser = 'a recParseResult parser

  let rec recwith (recparser, func) f =
      (match recparser f with
            (Done x, f') -> (Done (func x), f')
          | (Continuation k, f') -> (Continuation (recwith (k, func)), f'))

  exception Error of string
  let error (r, msg) = raise (Error(Paths.wrap (r, msg)))

end  (* functor Parsing *)

module Parsing =
  ParsingFunc (Lexer.Lexer)
