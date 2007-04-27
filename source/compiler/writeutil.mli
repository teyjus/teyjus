(**************************************************************************)
(* This module provides auxiliary functions for dumping bytecode files and*)
(* relevant pre-defined flags which should agree with those in the loader *)
(**************************************************************************)

(**********************************************************************)
(* management of out channel                                          *)
(**********************************************************************)
val openOutChannel : string -> unit
val closeOutChannel : unit -> unit

(**********************************************************************)
(* functions for writing certain numbers of bytes to out channel      *)
(**********************************************************************)
(* record the number of bytes contained by a word (needed for writing *)
(* a word.                                                            *)
val setMachineBits : int -> unit
val numberBytesInWord : unit -> int

val writeint1 : int -> unit           (* one   byte  *)
val writeint2 : int -> unit           (* two   bytes *)
val writeint4 : int -> unit           (* four  bytes *)
val writeint8 : int -> unit           (* eight bytes *)
val writeintref8 : int ref -> unit    (* eight bytes *)
val writeintref4 : int ref -> unit    (* four  bytes *)
(* write a word: the number of bytes depend on machine architecture  *)
val writeWord : int -> unit           
(* a float number: 8 bytes with the first being the mantissa and the *)
(* second being the exponent.                                        *)
val writefloat4 :  float -> unit      
(* write a string: the leading byte contains the length of the string*)
(* and is followed by a sequence of characters.                      *)
val writeString :  string -> unit 

val writeaconstant2 : Absyn.aconstant -> unit
val writeakind2 : Absyn.akind -> unit

(**********************************************************************)
(* byte code format flags                                             *)
(**********************************************************************)
val byteCodeVersionNumber : int

(* type skeleton representation *)
val typeMarkArrow : int
val typeMarkSkeletonVar : int
val typeMarkKind : int

(* constant/kind category *)
val local : int
val global : int
val pervasive : int
val hidden : int

(* constant fixity *)
val fixityMarkInfix : int
val fixityMarkInfixl : int
val fixityMarkInfixr : int
val fixityMarkPrefix : int
val fixityMarkPrefixr : int
val fixityMarkPostfix : int
val fixityMarkPostfixl : int
val fixityMarkNoFixity : int 

(* find code function: hash or sequence search *)
val findCodeFuncMarkHash : int
val findCodeFuncMarkSeq  : int


