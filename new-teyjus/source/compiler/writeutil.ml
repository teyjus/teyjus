
(************************************************************************)
(* the output channel                                                   *)
(************************************************************************)
let outChannel : out_channel ref = ref stderr

let getOutChannel () = !outChannel

let openOutChannel name =
  let outFile = open_out_bin name in
  outChannel := outFile

let closeOutChannel () =
  close_out (getOutChannel ());
  outChannel := stderr

(************************************************************************)
(* machine bits                                                         *)
(************************************************************************)
let machineBits = ref 0

let setMachineBits number = machineBits := number
let numberBytesInWord () = (!machineBits) / 8 

(************************************************************************)
(* write functions                                                      *)
(************************************************************************)
(* writing n bytes to the given channel *)
let rec writeNBytes out number numBytes =
  let byte = number land 0xff in
  (if numBytes > 1 then writeNBytes out (number lsr 8) (numBytes - 1)
   else ());
  output_byte out byte

(* one byte *)
let writeint1 number = writeNBytes (getOutChannel ()) number 1

(* two bytes *)
let writeint2 number = writeNBytes (getOutChannel ()) number 2

(* four bytes *)
let writeint4 number = writeNBytes (getOutChannel ()) number 4

(* eight bytes *)
let writeint8 number = writeNBytes (getOutChannel ()) number 8

(* eight bytes *)
let writeintref8 numberRef = writeNBytes (getOutChannel ()) (!numberRef) 8

(* four bytes *)
let writeintref4 numberRef = writeNBytes (getOutChannel ()) (!numberRef) 4

(* a float number: 8 bytes with the first being the mantissa and the *)
(* second being the exponent.                                        *)
let writefloat4 number =
  let (significant, exponent) = frexp number in
  let mantissa = ldexp significant 31 in
  let myOutChannel = getOutChannel () in
  writeNBytes myOutChannel (int_of_float mantissa) 4;
  writeNBytes myOutChannel exponent 4

(* write a string: the leading byte contains the length of the string*)
(* and is followed by a sequence of characters.                      *)
let writeString str =
  writeint1 (String.length str);
  output_string (getOutChannel ()) str

(* write a word: the number of bytes depend on machine architecture  *)
let writeWord number = 
  writeNBytes (getOutChannel ()) number (numberBytesInWord ())
  
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
