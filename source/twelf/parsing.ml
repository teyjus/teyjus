(* This is taken from the Twelf implementation *)


module type PARSING = functor (Stream : Stream.STREAM) ->
sig
  (*
  structure Lexer : LEXER
    sharing Lexer.Stream = Stream
  *)

  type lexResult = (Lexer.Lexer.token * Paths.Paths.region)

  type 'a parser = lexResult Stream.front -> 'a * lexResult Stream.front

  (* recursive parser (allows parsing functions that need to parse
     a signature expression to temporarily suspend themselves) *)
  type 'a recParseResult =
    Done of 'a
  | Continuation of 'a recParseResult parser

  type 'a recparser = 'a recParseResult parser

  (* useful combinator for recursive parsers *)
  val recwith : 'a recparser * ('a -> 'b) -> 'b recparser

  exception Error of string
  val error : Paths.Paths.region * string -> 'a	(* always raises Error *)
end;; (* signature PARSING *)

module ParsingFunc : PARSING = functor (Stream' : Stream.STREAM) ->
struct
  type lexResult = Lexer.Lexer.token * Paths.Paths.region

  type 'a parser = lexResult Stream'.front -> 'a * lexResult Stream'.front

  type 'a recParseResult =
    Done of 'a
  | Continuation of 'a recParseResult parser

  type 'a recparser = 'a recParseResult parser

  let rec recwith (recparser, func) f =
      (match recparser f with
            (Done x, f') -> (Done (func x), f')
          | (Continuation k, f') -> (Continuation (recwith (k, func)), f'))

  exception Error of string
  let error (r, msg) = raise (Error(Paths.Paths.wrap (r, msg)))

end  (* functor Parsing *)

module Parsing =
  ParsingFunc (Stream.Stream)
