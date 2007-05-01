(**************************************************************************)
(* This module provides auxiliary functions and flags for writing and     *)
(* reading bytecode files.                                                *)
(**************************************************************************)

(**********************************************************************)
(* byte code format flags                                             *)
(**********************************************************************)
val byteCodeVersionNumber : int

(* type skeleton representation *)
val typeMarkArrow         : int
val typeMarkKind          : int
val typeMarkSkeletonVar   : int

(* constant fixity *)
val fixityMarkInfix    : int 
val fixityMarkInfixl   : int 
val fixityMarkInfixr   : int 
val fixityMarkNoFixity : int 
val fixityMarkPrefix   : int 
val fixityMarkPrefixr  : int 
val fixityMarkPostfix  : int 
val fixityMarkPostfixl : int 

(* constant/kind category *)
val global    : int 
val local     : int 
val hidden    : int 
val pervasive : int 

(* find code function: hash or sequence search *)
val findCodeFuncMarkHash : int 
val findCodeFuncMarkSeq  : int 

(***********************************************************************)
(*                     IO facilities                                   *)
(***********************************************************************)
(**********************************************************************)
(* record the number of bytes contained by a word (needed for reading *)
(* or writing a word).                                                *)
(**********************************************************************)
val setWordSize : unit -> unit 
val getWordSize : unit -> int 

(******************************************************************)
(* management of output channel                                   *)
(******************************************************************)
val openOutChannel  : string -> unit
val closeOutChannel : unit   -> unit 

(******************************************************************)
(* management of input channel                                    *)
(******************************************************************)
val openInChannel   : string -> unit
val closeInChannel  : unit   -> unit

(*******************************************************************)
(* functions for writing certain numbers of bytes to output channel*)
(*******************************************************************)
val writeint1    : int -> unit       (* one byte             *)
val writeint2    : int -> unit       (* two bytes            *)
val writeint4    : int -> unit       (* four bytes           *)
val writeintref4 : int ref -> unit   (* four bytes reference *)
val writeint8    : int -> unit       (* eight bytes          *)
val writeintref8 : int ref -> unit   (* eight bytes reference*)

(* write a word:                                       *)
(* the number of bytes depend on machine architecture  *)
val writeWord : int -> unit 

(* write a float number:                               *)
(* 8 bytes with the first four being the mantissa and  *)
(* the folloing being the exponent.                    *)
val writefloat4 : float -> unit

(* write a string:                                     *)
(* the leading byte contains the length of the string  *)
(* and is followed by a sequence of characters.        *)
val writeString : string -> unit

(* write a kind index:                                 *)
(* a one byte flag indicating the kind category        *)
(* followed by two bytes kind table index.             *)
val writeakind2 : Absyn.akind -> unit

(* write a constant index:                             *)
(* a one byte flag indicating the constant category    *)
(* followed by two bytes constant table index.         *)
val writeaconstant2 : Absyn.aconstant -> unit

(********************************************************************)
(* functions for reading certain numbers of bytes from input channel*)
(********************************************************************)
val readOneByte  : unit -> int    (* read one byte  *)
val readTwoBytes : unit -> int    (* read two bytes *)
val readWord     : unit -> int    (* read a word    *)
val readString   : unit -> string (* read a string  *)

val skipNBytes : int -> unit (* skip n bytes   *)
val skipNWords : int -> unit (* skip n words   *)
