module type BASIC_STREAM =
sig
  type 'a stream
  type 'a front = Empty | Cons of 'a * 'a stream

  (* Lazy stream construction and exposure *)
  val delay : (unit -> 'a front) -> 'a stream
  val expose : 'a stream -> 'a front

  (* Eager stream construction *)
  val empty : 'a stream
  val cons : 'a * 'a stream -> 'a stream
end

module BasicStream : BASIC_STREAM

module BasicMemoStream : BASIC_STREAM

module type STREAM =
sig
  include BASIC_STREAM

  exception EmptyStream
  exception Subscript

  val null : 'a stream -> bool
  val hd : 'a stream -> 'a
  val tl : 'a stream -> 'a stream

  val map : ('a -> 'b) -> 'a stream -> 'b stream
  val filter : ('a -> bool) -> 'a stream -> 'a stream
  val exists : ('a -> bool) -> 'a stream -> bool

  val take : 'a stream * int -> 'a list
  val drop : 'a stream * int -> 'a stream

  val fromList : 'a list -> 'a stream
  val toList : 'a stream -> 'a list

  val tabulate : (int -> 'a) -> 'a stream
end

module StreamFunc : functor (BasicStream : BASIC_STREAM)  -> STREAM

module Stream : STREAM

module MStream : STREAM  

