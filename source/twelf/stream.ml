(* This is taken from the Twelf implementation *)


(* BASIC_STREAM defines the visible "core" of streams *)
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
end;;

module BasicStream : BASIC_STREAM =
struct
  type 'a stream = Stream of (unit -> 'a front)
  and 'a front = Empty | Cons of 'a * 'a stream

  let delay (d) = Stream(d)
  let expose (Stream(d)) = d ()

  let empty = Stream (fun () -> Empty)
  let cons (x, s) = Stream (fun () -> Cons (x, s))
end;;

(* Note that this implementation is NOT semantically *)
(* equivalent to the plain (non-memoizing) streams, since *)
(* effects will be executed only once in this implementation *)
module BasicMemoStream : BASIC_STREAM =
struct
  type 'a stream = Stream of (unit -> 'a front)
  and 'a front = Empty | Cons of 'a * 'a stream

  exception Uninitialized

  let expose (Stream (d)) = d ()
  let delay (d) =
      let memo = ref (fun () -> raise Uninitialized) in
      let memoFun () =
        try
          let r = d () in
          ( memo := (fun () -> r) ; r )
        with
        exn -> ( memo := (fun () -> raise exn) ; raise exn )
      in
      memo := memoFun ; Stream (fun () -> !memo ())
      
  let empty = Stream (fun () -> Empty)
  let cons (x, s) = Stream (fun () -> Cons (x, s))
end;;

(* STREAM extends BASIC_STREAMS by operations *)
(* definable without reference to the implementation *)
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
end;;

module StreamFunc (BasicStream : BASIC_STREAM) : STREAM =
struct
  include BasicStream

  exception EmptyStream
  exception Subscript

  (* functions null, hd, tl, map, filter, exists, take, drop *)
  (* parallel the functions in the List structure *)

  let null (s) = 
    let null' arg =
      match arg with
          (Empty) -> true
        | (Cons _) -> false
    in
    null' (expose s)


  let hd (s) = 
    let hd' arg =
      match arg with
          (Empty) -> raise EmptyStream
        | (Cons (x,s)) -> x
    in
    hd' (expose s)


  let tl (s) = 
    let tl' arg =
      match arg with
          (Empty) -> raise EmptyStream
        | (Cons (x,s)) -> s
    in
    tl' (expose s)


  let rec map f s = 
    let map' f arg =
      match arg with
          (Empty) -> Empty
        | (Cons(x,s)) -> Cons (f(x), map f s)
    in
    delay (fun () -> map' f (expose s))

  let rec filter p s = 
    let rec filter' p arg =
      match arg with
          (Empty) -> Empty
        | (Cons(x,s)) ->
          if p(x) then Cons (x, filter p s)
  	  else filter' p (expose s)
    in
    delay (fun () -> filter' p (expose s))

  let rec exists p s = 
    let exists' p arg =
      match arg with
          (Empty) -> false
        | (Cons(x,s)) ->
          p(x) || exists p s
    in
    exists' p (expose s)

  let rec takePos arg =
    let take' arg =
      match arg with 
          (Empty, _) -> []
        | (Cons(x,s), n) -> x::takePos(s, n-1)
    in
    match arg with
        (s, 0) -> []
      | (s, n) -> take' (expose s, n)

  let take (s,n) = if n < 0 then raise Subscript else takePos (s,n)

  let rec fromList arg =
    match arg with
        ([]) -> empty
      | (x::l) -> cons(x,fromList(l))

  let rec toList (s) = 
    let toList' arg =
      match arg with
          (Empty) -> []
        | (Cons(x,s)) -> x::toList(s)
    in
    toList' (expose s)



  let drop (s,n) = 
    let rec dropPos arg =
      match arg with
        (s, 0) -> s
      | (s, n) -> drop' (expose s, n)
    and drop' arg =
      match arg with
          (Empty, _) -> empty
        | (Cons(x,s), n) -> dropPos (s, n-1)
    in
    if n < 0 then raise Subscript else dropPos (s,n)

  let rec tabulate f = 
    let tabulate' f = Cons (f(0), tabulate (fun i -> f(i+1))) in
    delay (fun () -> tabulate' f)

end;;

(* structure Stream :> STREAM --- non-memoizing *)
module Stream : STREAM =
  StreamFunc(BasicStream);;

(* structure MStream :> STREAM --- memoizing *)
module MStream : STREAM =
  StreamFunc (BasicMemoStream);;
