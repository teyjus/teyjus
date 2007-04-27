(****************************************************************************)
(* the input channel                                                        *)
(****************************************************************************)
let inChannel : in_channel ref = ref stdin

let getInChannel () = !inChannel

let openInChannel name = 
  let inFile = open_in_bin name in
  inChannel := inFile

let closeInChannel () =
  close_in (getInChannel ());
  inChannel := stdin

(***************************************************************************)
(* read functions                                                          *)
(***************************************************************************)
(* read N bytes into an integer: it is assumed that the number of bytes is *)
(* less then that of an integer type.                                      *)
let readNBytes input numBytes =
  let rec readNBytesAux numBytes number =
	if (numBytes = 0) then number
	else
	  let oneByte = input_byte input  in
	  readNBytesAux (numBytes - 1) ((number lsl 8) lor oneByte)
  in
  readNBytesAux numBytes 0


(* read one byte *)
let readOneByte  () = readNBytes (getInChannel ()) 1

(* read two bytes *)
let readTwoBytes () = readNBytes (getInChannel ()) 2

(* read a word *)
let readWord () = readNBytes (getInChannel ()) (Writeutil.numberBytesInWord ())

(* read a string *)
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

(* skip n bytes *)
let skipNBytes numberBytes =
  let input = getInChannel () in
  seek_in input ((pos_in input) + numberBytes)

(* skip n words *)
let skipNWords numberWords = skipNBytes (Writeutil.numberBytesInWord ())
