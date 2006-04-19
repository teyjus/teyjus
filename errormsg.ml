(**********************************************************************
*ErrorMsg Module:
*	In the short run this is used simply to output meaningful error
*	messages with respect to character and line position.  In the long
*	run this will eventually allow for such things as specifying warnings
*	to ignore and the like.
*
*	This has not yet been tested almost at all.
**********************************************************************)
module type ERRORMSG =
sig
		exception InternalError
		val anyErrors : bool ref
		val fileName : string ref
		val linePos : int list ref
		val log : int -> string -> unit
		val error : int -> string -> unit
		val impossible : int -> string -> 'a   (* raises Error *)
		val reset : unit -> unit
end

module ErrorMsg : ERRORMSG =
struct
	exception InternalError
	
	let anyErrors = ref false
	let fileName = ref ""
	
	(********************************************************************
	* Note that line numbers are assumed to start at 1 instead of 0.
	********************************************************************)
	let linePos = ref [1]
	
	(********************************************************************
	*printPosition:
	*	Prints position information.
	********************************************************************)
	let rec printPosition = fun pos ->
		let rec p = function
				(a::rest, n) ->
					if a < pos then
						List.iter print_string [":"; string_of_int n; "."; string_of_int (pos - a)]
					else
						p(rest, n - 1)
				| _ -> (print_string "0.0")
		in
			p(!linePos, List.length(!linePos))

	(********************************************************************
	*linePos:
	* Call with a character position when a newline is reached.
	********************************************************************)
	let newLine(i) =
		linePos := i :: !linePos

	(********************************************************************
	*reset:
	* Just resets the error message module.
	********************************************************************)
	let reset = fun () ->
			(anyErrors:=false;
			fileName:="";
			linePos:=[1])


	(********************************************************************
	*error:
	* Prints an error, along with the line and character position.
	********************************************************************)
	let error = fun pos (msg:string) ->
			anyErrors := true;
			print_string (!fileName);
			printPosition pos;
			print_string ":\n";
			print_string msg;
			print_string "\n\n"

	(********************************************************************
	*log:
	*	Outputs logging information.
	********************************************************************)
	let log = fun pos msg ->
			(print_string "Log: ";
			print_string (!fileName);
			print_string ":\n";
			print_string msg;
			print_string "\n\n")
		
	(********************************************************************
	*impossible:
	* Prints an internal compiler error, then throws an exception
	* InternalError.
	********************************************************************)
	let impossible = fun pos msg ->
			anyErrors := true;
			print_string (!fileName);
			printPosition pos;
			print_string ":\n";
			List.iter print_string ["Internal Error:\n"; msg; "\n\n"];
			raise InternalError

end  (* structure ErrorMsg *)
	
