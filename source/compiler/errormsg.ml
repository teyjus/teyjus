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
type pos = (string * int)

let anyErrors = ref false

(**********************************************************************
*File Table:
**********************************************************************)
module Ordered =
struct
	type t = string
	let compare = compare
end
module FileTable = Map.Make(Ordered)

let fileTable = ref FileTable.empty

let find =
	fun f table ->
		try
			let v = FileTable.find f table in
			Some v
		with Not_found -> None

(********************************************************************
*none:
*	An empty pos, useful when printing internal compiler errors that
*	are unrelated to a particular file or location.
********************************************************************)
let none = ("", 0)

(********************************************************************
*string_of_pos:
*	Convert a pos to a string.
********************************************************************)
let string_of_pos = function (file, pos) ->
	let rec p = fun lines num ->
		match lines with
			a::rest ->
				if a < pos then
					(file ^ ":" ^ (string_of_int num) ^ "." ^ (string_of_int (pos - a)))
				else
					p rest (num - 1)
			| [] -> (file ^ "0.0")
	in
	if (file, pos) = none then
		"none"
	else
		match (find file (!fileTable)) with
			Some(lines) ->
				p (!lines) (List.length (!lines))
		|	None -> raise Not_found

(********************************************************************
*printPosition:
*	Prints position information.
********************************************************************)
let rec printPosition = fun p ->
	print_string (string_of_pos p)

(********************************************************************
*newLine:
* Call with a character position when a newline is reached.
********************************************************************)
let newLine = function (file, pos) ->
	match (find file (!fileTable)) with
		Some(lines) -> lines := pos :: !lines
	|	None -> raise Not_found

(********************************************************************
*reset:
* Just resets the error message module.
********************************************************************)
let reset = fun () ->
		(anyErrors:=false;
		fileTable := FileTable.empty)

(********************************************************************
*info:
* Formats information for output using error or warning.
********************************************************************)
let info = fun msg ->
  ".\n\t" ^ msg

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
		(anyErrors := true;
		printPosition pos;
		print_string " Error: ";
		print_string msg;
		print_string ".\n\n")

(********************************************************************
*warning:
* Prints a warning, along with the line and character position.
********************************************************************)
let warning = fun pos (msg:string) ->
		anyErrors := true;
		printPosition pos;
		print_string " Warning:\n\t";
		print_string msg;
		print_string ".\n\n"
		
(********************************************************************
*log:
*	Outputs logging information.
********************************************************************)
let log = fun pos msg ->
		(printPosition pos;
		print_string "Log:\n\t";
		print_string msg;
		print_string ".\n\n")

(********************************************************************
*impossible:
* Prints an internal compiler error, then throws an exception
* InternalError.
********************************************************************)
let impossible = fun pos msg ->
		anyErrors := true;
		printPosition pos;
		print_string ":\n";
		List.iter print_string ["Internal Error:\n\t"; msg; ".\n\n"];
		raise InternalError

	
(********************************************************************
*addFile:
*	Set the current file.  If the file hasn't been seen yet, add it to
*	the table.
********************************************************************)
let addFile = fun file ->
	match (find file (!fileTable)) with
		Some f -> ()
	|	None ->
			let lines = ref [1] in
			(fileTable := (FileTable.add file lines (!fileTable)))
