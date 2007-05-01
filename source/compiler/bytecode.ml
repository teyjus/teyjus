(**************************************************************************)
(* This module provides auxiliary functions and flags for writing and     *)
(* reading bytecode files.                                                *)
(**************************************************************************)

(**********************************************************************)
(* byte code format flags                                             *)
(**********************************************************************)
let byteCodeVersionNumber = 2

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
let findCodeFuncMarkHash = 0
let findCodeFuncMarkSeq  = 1

(***********************************************************************)
(*                     IO facilities                                   *)
(***********************************************************************)
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
  let outFile = open_out_bin name in
  setOutChannel outFile

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
  let inFile = open_in_bin name in
  setInChannel inFile

let closeInChannel () =
  close_in (getInChannel ());
  setInChannel stdin

(*******************************************************************)
(* functions for writing certain numbers of bytes to output channel*)
(*******************************************************************)
(* aux: writing n bytes to the given channel *)
let rec writeNBytes out number numBytes =
  let byte = number land 0xff in
  (if numBytes > 1 then writeNBytes out (number lsr 8) (numBytes - 1)
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
  let mantissa = ldexp significant 31 in
  let myOutChannel = getOutChannel () in
  writeNBytes myOutChannel (int_of_float mantissa) 4;
  writeNBytes myOutChannel exponent 4

(* write a string:                                     *)
(* the leading byte contains the length of the string  *)
(* and is followed by a sequence of characters.        *)
let writeString str =
  writeint1 (String.length str);
  output_string (getOutChannel ()) str

(* write a kind index:                                 *)
(* a one byte flag indicating the kind category        *)
(* followed by two bytes kind table index.             *)
let writeakind2 kind =
  (match kind with
	Absyn.LocalKind(_) -> writeint1 local
  | Absyn.GlobalKind(_) -> writeint1 global
  | Absyn.PervasiveKind(_) -> writeint1 pervasive);
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

(* read one byte  *)
let readOneByte  () = readNBytes (getInChannel ()) 1

(* read two bytes *)
let readTwoBytes () = readNBytes (getInChannel ()) 2

(* read a word    *)
let readWord () = readNBytes (getInChannel ()) (getWordSize ())

(* read a string  *)
let readString () =
  let input = getInChannel () in
  let length = readNBytes input 1 in
  let myString = String.make length ' ' in
  let rec readStringAux index =
	if (index = length) then ()
	else 
	  (String.set myString index (input_char input);
	   readStringAux (index + 1))
  in
  readStringAux 0;
  myString  

(* skip n bytes   *)
let skipNBytes numberBytes =
  let input = getInChannel () in
  seek_in input ((pos_in input) + numberBytes)

(* skip n words *)
let skipNWords numberWords = 
  skipNBytes (numberWords * (getWordSize ()))
