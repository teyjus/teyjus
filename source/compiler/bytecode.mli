(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Nathan Guermond, Steven Holte, 
*  Gopalan Nadathur, Xiaochu Qi, Zach Snow
****************************************************************************)
(****************************************************************************
* This file is part of Teyjus.
*
* Teyjus is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Teyjus is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************)
(**************************************************************************)
(* This module provides auxiliary functions and flags for writing and     *)
(* reading bytecode files.                                                *)
(**************************************************************************)

(** ******************************************************************** **)
(**                       BYTECODE FORMAT                                **)
(** ******************************************************************** **)
val byteCodeVersionNumber : int
val makeByteCodeFileName  : string -> string

val linkedByteCodeVersionNumber : int
val makeLinkedByteCodeName : string -> string
  
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

(** ******************************************************************** **)
(**                       IO FACILITIES                                  **)
(** ******************************************************************** **)

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
val setOutChannel : out_channel -> unit
val closeOutChannel : unit   -> unit 

(******************************************************************)
(* management of input channel                                    *)
(******************************************************************)
val openInChannel   : string -> unit
val closeInChannel  : unit   -> unit

(** ******************************************************************* **)
(**                      WRITE FUNCTIONS                                **)
(** ******************************************************************* **)

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

(* write a string:                                        *)
(* the leading 4 bytes contains the length of the string  *)
(* and is followed by a sequence of characters.           *)
val writeString : string -> unit


(* write a kind index:                                 *)
(* a one byte flag indicating the kind category        *)
(* followed by two bytes kind table index.             *)
val writeakind2 : Absyn.akind -> unit

(* write a constant index:                             *)
(* a one byte flag indicating the constant category    *)
(* followed by two bytes constant table index.         *)
val writeaconstant2 : Absyn.aconstant -> unit

(** ******************************************************************* **)
(**                       READ FUNCTIONS                                **)
(** ******************************************************************* **)

(********************************************************************)
(* functions for reading certain numbers of bytes from input channel*)
(********************************************************************)
val readOneByte  : unit -> int    (* read one byte  *)
val readTwoBytes : unit -> int    (* read two bytes *)
val readWord     : unit -> int    (* read a word    *)
val readString   : unit -> string (* read a string  *)


val skipNBytes : int -> unit (* skip n bytes   *)
val skipNWords : int -> unit (* skip n words   *)

(********************************************************************)
(* functions for reading certain data structures                    *)
(********************************************************************)
(* kind/constant indexes *)
val readKindIndex  : 
	(int -> int -> Absyn.akind option) -> Absyn.akind option
val readConstantIndex : 
	(int -> int -> Absyn.aconstant option) -> Absyn.aconstant option

(* kind data *)
val readGlobalKind : int -> Absyn.akind  (* read a global kind *)
val readLocalKind  : int -> Absyn.akind  (* read a local kind  *)

(* constant data *)
val readGlobalConstant :                 (* read a global constant *)
	(int -> Absyn.askeleton) -> int -> Absyn.aconstant
val readLocalConstant  :                 (* read a local constant  *)
	(int -> Absyn.askeleton) -> int -> Absyn.aconstant
val readHiddenConstant :                 (* read a hidden constant *)
	(int -> Absyn.askeleton) -> int -> Absyn.aconstant 

(* read a type skeleton *)
val readTypeSkeleton : (int -> int -> Absyn.akind option) -> Absyn.atype 

(* find code function *)
val readFindCodeFn : unit -> int

(* read instruction operands *)
val readint1 : unit -> int
val readint2 : unit -> int
val readint4 : unit -> int
val readint8 : unit -> int
val readintref4 : unit -> int ref
val readintref8 : unit -> int ref
val readfloat4  : unit -> float
val readakind2  : unit -> Absyn.akind
val readaconstant2 : unit -> Absyn.aconstant

val setGetKindFn : (int -> int -> Absyn.akind option) -> unit
val setGetConstantFn : (int -> int -> Absyn.aconstant option) -> unit
val setGetLabelFn : (int -> unit) -> unit

(** ******************************************************************* **)
(**          DISPLAY FUNCTIONS FOR DISASSEMBLY                          **)
(** ******************************************************************* **)
val setFindLabelFn : (int -> string) -> unit

val displayR   : int -> string
val displayE   : int -> string
val displayN   : int -> string
val displayI1  : int -> string
val displayCE  : int -> string
val displaySEG : int -> string
val displayI   : int -> string
val displayF   : float -> string
val displayS   : int -> string
val displayMT  : int -> string
val displayIT  : int -> string
val displayHT  : int -> string
val displayBVT : int -> string
val displayL   : int ref -> string

(* display a kind data     *)
val displayK : Absyn.akind -> string
(* display a constant data *)
val displayC : Absyn.aconstant -> string

(* display find code function *)
val displayFindCodeFn : int -> string


