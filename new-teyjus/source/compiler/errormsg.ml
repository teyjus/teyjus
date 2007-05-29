(**********************************************************************
*ErrorMsg Module:
*	In the short run this is used simply to output meaningful error
*	messages with respect to character and line position.  In the long
*	run this will eventually allow for such things as specifying warnings
*	to ignore and the like.
*
*	This has not yet been tested almost at all.
**********************************************************************)
exception InternalError
open Lexing

type pos = position

let anyErrors = ref false

let errorsEnabled = ref true
let warningsEnabled = ref true
let loggingEnabled = ref false

let warningsAsErrors = ref false

(********************************************************************
*none:
*	An empty pos, useful when printing internal compiler errors that
*	are unrelated to a particular file or location.
********************************************************************)
let none = { pos_fname = "none" ;
             pos_lnum = 0 ;
             pos_bol = 0 ;
             pos_cnum = 0 }
  
(********************************************************************
*string_of_pos:
*	Convert a pos to a string.
********************************************************************)
let string_of_pos = function pos ->
  let file = pos.pos_fname in
  let line = pos.pos_lnum in
  let char = pos.pos_cnum - pos.pos_bol in
    file ^ "(" ^ (string_of_int line) ^ "," ^ (string_of_int char) ^ ")"

(********************************************************************
*printPosition:
*	Prints position information.
********************************************************************)
let rec printPosition = fun p ->
	print_string (string_of_pos p)

(********************************************************************
*reset:
* Just resets the error message module.
********************************************************************)
let reset = fun () -> anyErrors:=false

(********************************************************************
*info:
* Formats information for output using error or warning.
********************************************************************)
let info = fun msg ->
  "\n\t" ^ msg

(********************************************************************
*see:
* Annotates a message with a file location.
********************************************************************)
let see = fun pos msg ->
	(info "See " ^ msg ^ " at " ^ (string_of_pos pos))

(********************************************************************
*error:
* Prints an error, along with the line and character position.
********************************************************************)
let error = fun pos (msg:string) ->
  if !errorsEnabled then
		(anyErrors := true;
		printPosition pos;
		print_string " : Error : ";
		print_string msg;
        print_newline ())
  else
    ()
(********************************************************************
*warning:
* Prints a warning, along with the line and character position.
********************************************************************)
let warning = fun pos (msg:string) ->
  if !warningsEnabled && !errorsEnabled then
	  (if !warningsAsErrors then
	    anyErrors := true
	  else
	    ();
	  printPosition pos;
	  print_string " : Warning : ";
	  print_string msg;
      print_newline ())
  else
    ()		
(********************************************************************
*log:
*	Outputs logging information.
********************************************************************)
let log = fun pos msg ->
  if !loggingEnabled && !warningsEnabled && !errorsEnabled then
	  (printPosition pos;
	  print_string " : Log : ";
	  print_string msg;
      print_newline ())
  else
    ()
(********************************************************************
*impossible:
* Prints an internal compiler error, then throws an exception
* InternalError.
********************************************************************)
let impossible = fun pos msg ->
		(anyErrors := true;
		printPosition pos;
		print_string " : Internal Error : ";
		print_string msg; 
		print_newline ();
		raise InternalError)
