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
let byteCodeVersionNumber = 2
let byteCodeSuffix = ".lpo"

let linkedByteCodeVersionNumber = 3
let linkedByteCodeSuffix = ".lp"
                         
let makeByteCodeFileName modName   = modName ^ byteCodeSuffix
let makeLinkedByteCodeName modName = modName ^ linkedByteCodeSuffix
                                   
(* type skeleton representation *)
let typeMarkArrow       = 0
let typeMarkKind        = 1
let typeMarkSkeletonVar = 2

(* constant fixity *)
let fixityMarkInfix = 0
let fixityMarkInfixl = 1
let fixityMarkInfixr = 2 
let fixityMarkNoFixity = 3
let fixityMarkPrefix = 4
let fixityMarkPrefixr = 5
let fixityMarkPostfix = 6
let fixityMarkPostfixl =7

(* constant/kind category *)
let global = 0
let local  = 1
let hidden = 2
let pervasive = 3

(* find code function: hash or sequence search *)
let findCodeFuncMarkHash = 1
let findCodeFuncMarkSeq  = 0

(** ******************************************************************** **)
(**                       IO FACILITIES                                  **)
(** ******************************************************************** **)

(**********************************************************************)
(* record the number of bytes contained by a word (needed for reading *)
(* or writing a word).                                                *)
(**********************************************************************)
let wordSize = ref 0

let setWordSize () = 
  let rec setWordSizeAux number wordSize =
	if (number = 0) then wordSize
	else setWordSizeAux (number lsr 8) (wordSize + 1)
  in
  wordSize := setWordSizeAux max_int 0

let getWordSize () = !wordSize

(******************************************************************)
(* management of output channel                                   *)
(******************************************************************)
let outChannel : out_channel ref = ref stderr
let getOutChannel ()      = !outChannel
let setOutChannel output  = outChannel := output

let openOutChannel name =
  try
    let outFile = open_out_bin name in
    setOutChannel outFile
  with
    Sys_error(s) -> (prerr_endline ("Error: " ^ s); exit (-1))

let closeOutChannel () =
  close_out (getOutChannel ());
  setOutChannel stderr

(******************************************************************)
(* management of input channel                                    *)
(******************************************************************)
let inChannel : in_channel ref = ref stdin
let getInChannel ()     = !inChannel
let setInChannel input  = inChannel := input

let openInChannel name = 
  try
    let inFile = open_in_bin name in
    setInChannel inFile
  with
    Sys_error(s) -> (prerr_endline ("Error: " ^ s); exit (-1))

let closeInChannel () =
  close_in (getInChannel ());
  setInChannel stdin

(** ******************************************************************* **)
(**                      WRITE FUNCTIONS                                **)
(** ******************************************************************* **)

(*******************************************************************)
(* functions for writing certain numbers of bytes to output channel*)
(*******************************************************************)
(* aux: writing n bytes to the given channel *)
let rec writeNBytes out number numBytes =
  let byte = number land 0xff in
  (if numBytes > 1 then writeNBytes out (number lsr 8) (numBytes - 1)
   else ());
  output_byte out byte

let rec writeNBytes2 out number numBytes =
  let byte = Int32.to_int (Int32.logand number (Int32.of_int 0xff)) in
   (if numBytes > 1 then 
     writeNBytes2 out (Int32.shift_right_logical number 8) (numBytes - 1)
   else ());
   output_byte out byte

  

(* one byte     *)
let writeint1 number = writeNBytes (getOutChannel ()) number 1

(* two bytes    *)
let writeint2 number = writeNBytes (getOutChannel ()) number 2

(* four bytes   *)
let writeint4 number = writeNBytes (getOutChannel ()) number 4

(* four bytes reference  *)
let writeintref4 numberRef = writeNBytes (getOutChannel ()) (!numberRef) 4

(* eight bytes  *)
let writeint8 number = writeNBytes (getOutChannel ()) number 8

(* eight bytes reference *)
let writeintref8 numberRef = writeNBytes (getOutChannel ()) (!numberRef) 8

(* write a word:                                       *)
(* the number of bytes depend on machine architecture  *)
let writeWord number = 
  writeNBytes (getOutChannel ()) number (getWordSize ())

(* write a float number:                               *)
(* 8 bytes with the first four being the mantissa and  *)
(* the folloing being the exponent.                    *)
let writefloat4 number =
  let (significant, exponent) = frexp number in
  let mantissa = Int32.of_float (ldexp significant 31) in
  let myOutChannel = getOutChannel () in
  writeNBytes2 myOutChannel mantissa 4;
  writeNBytes myOutChannel exponent 4

(* write a string:                                     *)
(* the leading byte contains the length of the string  *)
(* and is followed by a sequence of characters.        *)
let writeString str =
  writeint4 (String.length str);
  output_string (getOutChannel ()) str

let writeLongString str =
  writeint4 (String.length str);
  output_string (getOutChannel ()) str

(* write a kind index:                                 *)
(* a one byte flag indicating the kind category        *)
(* followed by two bytes kind table index.             *)
let writeakind2 kind =
  (match (Absyn.getKindType kind) with
	Absyn.LocalKind -> writeint1 local
  | Absyn.GlobalKind -> writeint1 global
  | Absyn.PervasiveKind -> writeint1 pervasive);
  writeint2 (Absyn.getKindIndex kind)

(* write a constant index:                             *)
(* a one byte flag indicating the constant category    *)
(* followed by two bytes constant table index.         *)
let writeaconstant2 const =
  let constCat = Absyn.getConstantType const in
  let constIndex = Absyn.getConstantIndex const in
  (match constCat with
	Absyn.GlobalConstant -> writeint1 global
  | Absyn.LocalConstant  -> writeint1 local
  | Absyn.PervasiveConstant(_) -> writeint1 pervasive
  | _ (* must be hidden constants*) -> writeint1 hidden);
  writeint2 constIndex

(** ******************************************************************* **)
(**                       READ FUNCTIONS                                **)
(** ******************************************************************* **)

(********************************************************************)
(* functions for reading certain numbers of bytes from input channel*)
(********************************************************************)
(* aux: read N bytes as an integer:                     *)
(* it is assumed that the number of bytes is less then  *)
(* that of an integer type.                             *)
let readNBytes input numBytes =
  let rec readNBytesAux numBytes number =
	if (numBytes = 0) then number
	else
	  let oneByte = input_byte input  in
	  readNBytesAux (numBytes - 1) ((number lsl 8) lor oneByte)
  in
  readNBytesAux numBytes 0

let readNBytes2 input numBytes =
  let rec readNBytesAux numBytes number =
    if (numBytes = 0) then number
    else
      let oneByte = input_byte input in
      readNBytesAux (numBytes - 1) 
	(Int32.logor (Int32.shift_left number 8) (Int32.of_int oneByte))
  in
  readNBytesAux numBytes (Int32.of_int 0)


(* read one byte  *)
let readOneByte  () = readNBytes (getInChannel ()) 1

(* read two bytes *)
let readTwoBytes () = readNBytes (getInChannel ()) 2

(* read a word    *)
let readWord () = readNBytes (getInChannel ()) (getWordSize ())

(* read a string  *)
let readString () =
  let input = getInChannel () in
  let length = readNBytes input 4 in
  let myString = Bytes.make length ' ' in
  let rec readStringAux index =
	if (index = length) then ()
	else 
	  (Bytes.set myString index (input_char input);
	   readStringAux (index + 1))
  in
  readStringAux 0;
  Bytes.to_string myString 

let readLongString () = 
  let input = getInChannel() in
  let length = readNBytes input 4 in
  let myString = Bytes.make length ' ' in
  let rec readStringAux index =
	if (index = length) then ()
	else 
	  (Bytes.set myString index (input_char input);
	   readStringAux (index + 1))
  in
  readStringAux 0;
  Bytes.to_string myString 

(* skip n bytes   *)
let skipNBytes numberBytes =
  let input = getInChannel () in
  seek_in input ((pos_in input) + numberBytes)

(* skip n words *)
let skipNWords numberWords = 
  skipNBytes (numberWords * (getWordSize ()))

(********************************************************************)
(* functions for reading certain data structures                    *)
(********************************************************************)

(* read kind index *)
let readKindIndex getKindFn =
  let kindCat = readOneByte  () in
  let kindInd = readTwoBytes () in
  getKindFn kindCat kindInd

(* read constant index *)
let readConstantIndex getConstFn =
  let constCat = readOneByte  () in
  let constInd = readTwoBytes () in
  getConstFn constCat constInd

(* read a global kind *)
let readGlobalKind ind =
  let arity = readOneByte () in
  let name  = readString ()   in
  Absyn.makeGlobalKind (Symbol.symbol name) arity ind

(* read a local kind *)
let readLocalKind ind =
  Absyn.makeLocalKind (Symbol.symbol "") (readOneByte ()) ind

(* read a type skeleton *)
let readTypeSkeleton getKindFn =

  let rec readTypeSkeletonAux () =
	let cat = readOneByte () in
	if cat  = typeMarkArrow then               (* arrow type *)
	  let arg = readTypeSkeletonAux () in
	  let target = readTypeSkeletonAux () in
	  Absyn.ArrowType(arg, target)
	else if cat = typeMarkSkeletonVar then     (* type skeleton variable *)
	  let offset = readOneByte () in
	  Absyn.SkeletonVarType (ref offset)
	else if cat = typeMarkKind then            (* sort or type application *)
	  let kindOpt = readKindIndex getKindFn in
	  let arity   = readOneByte () in
	  let args    = readTypeSkeletons arity [] in
	  if Option.isNone kindOpt then Absyn.ErrorType 
	  else
		Absyn.ApplicationType(Option.get kindOpt, args)
	else
	  (Errormsg.error Errormsg.none 
		 "readTypeSkeleton: invalid type skeleton in bytecode";
	   Absyn.ErrorType)

  and readTypeSkeletons number tyskels =
	if (number = 0) then (List.rev tyskels)
	else
	  readTypeSkeletons (number - 1) ((readTypeSkeletonAux ()) :: tyskels)
  in
  readTypeSkeletonAux () 

(* read fixity *)
let readFixity () =
  let number = readOneByte () in
  if (number = fixityMarkInfix) then Absyn.Infix
  else if (number = fixityMarkInfixl) then Absyn.Infixl
  else if (number = fixityMarkInfixr) then Absyn.Infixr
  else if (number = fixityMarkNoFixity) then Absyn.NoFixity
  else if (number = fixityMarkPrefix) then Absyn.Prefix
  else if (number = fixityMarkPrefixr) then Absyn.Prefixr
  else if (number = fixityMarkPostfix) then Absyn.Postfix
  else Absyn.Postfixl

(* read global constant *)
let readGlobalConstant getTypeSkelFn ind =
  let fixity    = readFixity  () in
  let prec      = readOneByte () in
  let tyEnvSize = readOneByte () in
  let symbol    = Symbol.symbol (readString ()) in
  let tySkelInd = readTwoBytes () in
  let tySkel    = getTypeSkelFn tySkelInd in
  Absyn.makeGlobalConstant symbol fixity prec false false tyEnvSize tySkel ind

(* read local constant *)
let readLocalConstant getTypeSkelFn ind =
  let fixity    = readFixity  () in
  let prec      = readOneByte () in
  let tyEnvSize = readOneByte () in
  let tySkelInd = readTwoBytes () in
  let tySkel    = getTypeSkelFn tySkelInd in
  Absyn.makeLocalConstant (Symbol.symbol "") fixity prec tyEnvSize tySkel ind

(* read hidden constant *)
let readHiddenConstant getTypeSkelFn ind =
  let tySkelInd = readTwoBytes () in
  let tySkel    = getTypeSkelFn tySkelInd in
  let const     = Absyn.makeHiddenConstant tySkel 0 in
  Absyn.setConstantIndex const ind;
  const

(* read findcode function *)
let readFindCodeFn () = readOneByte ()

(* read instruction operands *)
let readint1 () = readOneByte ()
let readint2 () = readTwoBytes ()
let readint4 () = readNBytes (getInChannel ()) 4
let readint8 () = readNBytes (getInChannel ()) 8

(* read label *)
let getLabelFn : (int -> unit) option ref = ref None
let setGetLabelFn func = getLabelFn := Some func

(* read label *)
let readintref4 () = 
  let offset = readWord () in
  (Option.get (!getLabelFn)) offset;
  (ref offset)

let readintref8 () =
  let offset = readWord () in
  (Option.get (!getLabelFn)) offset;
  (ref offset)

(* read float *)
let readfloat4 () =
  let input = getInChannel () in
  let mantissa = Int32.to_float (readNBytes2 input 4) in
  let exponent = readNBytes input 4 in
  let (significant, _) = frexp mantissa in
  ldexp significant exponent

(* read kind/constant *)
let getKindFn : (int -> int -> Absyn.akind option) option ref = ref None
let getConstantFn : (int -> int -> Absyn.aconstant option) option ref 
	= ref None
  
let setGetKindFn func = getKindFn := Some(func)
let setGetConstantFn func = getConstantFn := Some(func)

let readakind2 () =  Option.get (readKindIndex (Option.get (!getKindFn))) 
let readaconstant2 () =
	Option.get (readConstantIndex (Option.get(!getConstantFn)))

(** ******************************************************************* **)
(**          DISPLAY FUNCTIONS FOR DISASSEMBLY                          **)
(** ******************************************************************* **)
let findLabelFn : (int -> string) option ref = ref None
let setFindLabelFn func = findLabelFn := Some func

let displayR   regNum = "A" ^ (string_of_int regNum)
let displayE   envNum = "Y" ^ (string_of_int envNum)
let displayN   number = "#" ^ (string_of_int number)
let displayI1  number = "#" ^ (string_of_int number)
let displayCE  number = "Y" ^ (string_of_int number)
let displaySEG number = "#" ^ (string_of_int number)
let displayI   number = (string_of_int number)
let displayF   number = (string_of_float number)
let displayS   number = "<string #" ^ (string_of_int number) ^ ">"
let displayMT  number = "<import #" ^ (string_of_int number) ^ ">"
let displayIT  number = "<impl #" ^ (string_of_int number) ^ ">"
let displayHT  number = "<hash #" ^ (string_of_int number) ^ ">"
let displayBVT number = "<bvt #" ^ (string_of_int number) ^ ">"
let displayL   offset = (Option.get (!findLabelFn)) (!offset)

(* display a kind data *)
let displayK kind =
  match (Absyn.getKindType kind) with
	  Absyn.GlobalKind -> Absyn.getKindName kind
  | Absyn.LocalKind  -> 
	  "<local kind #" ^ (string_of_int (Absyn.getKindIndex kind)) ^ ">"
  | Absyn.PervasiveKind -> Absyn.getKindName kind

(* display a constant data *)
let displayC const =
  let cat = Absyn.getConstantType const in
  match cat with
	  Absyn.GlobalConstant -> Absyn.getConstantName const
  | Absyn.PervasiveConstant(_) -> Absyn.getConstantName const
  | Absyn.LocalConstant -> 
	  "<local const #" ^ (string_of_int (Absyn.getConstantIndex const)) ^ ">"
  | Absyn.HiddenConstant -> 
	  "<hidden const #" ^ (string_of_int (Absyn.getConstantIndex const)) ^ ">"
  | _ -> Errormsg.impossible Errormsg.none "displayaconstant2: invalid const"
	  
(* display find code function *)
let displayFindCodeFn mark =
  if mark = findCodeFuncMarkHash then "hash"
  else "sequential"
