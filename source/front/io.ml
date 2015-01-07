(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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


(* Channels for input/output/output and append *)
type chan = In of in_channel | Out of out_channel | OutApp of out_channel ;;

let htFiles = Hashtbl.create 2 
let _ = Hashtbl.add htFiles "stdin" (In(stdin))
let _ = Hashtbl.add htFiles "stdout" (Out(stdout))
(* TODO: check what to do with OutApp *)

let readTermStdin term =
	Query.readTermChannel stdin (Module.getCurrentModule ())

let readTermFileId fileId = 
	try match Hashtbl.find htFiles fileId with
		| In(chan) -> 
		  Query.readTermChannel chan (Module.getCurrentModule ())
		| _ -> -1
	with
		Not_found -> -1 


let inputNChars fileId n = 
    (* An error is represented as the empty string. It is OK since 
	Teyjus "input" expects at least one character to be read *)
    try match Hashtbl.find htFiles fileId with
		| In(chan) -> 
			begin
				let pos = pos_in chan in
				let buf = String.create (n-1) in
				try 
					let _ = really_input chan buf 0 (n-1) in
						buf
				with
					| End_of_file ->
						(* We want instead to read 
						all the remaining 
						characters and not n*)
						let _ = seek_in chan pos in
						let m = in_channel_length chan in
						(* Cannot fail *) 
						let _ = really_input chan buf pos m in
							buf 
					| Invalid_argument "really_input" -> 
						(* Should not be reached *)
						Printf.eprintf 
							"Teyjus encountered an internal error in inputNChars, please report a bug\n" ; 
						raise Exit
			end
		| _ -> 
			(* This should not be reachable since the abstract 
			machine has checked the type of the argument before
			giving it to the OCaml part *)
			""
	with
		| Not_found ->
			(* Is it possible to reach this point ? *) 
			""

let openFile fname mode = 
    try
        match mode with
            | "r" -> 
                let chan = open_in fname in
                let () = Hashtbl.add htFiles fname (In(chan)) in
                1
            | "w" -> 
                let chan = open_out fname in 
                let () = Hashtbl.add htFiles fname (Out(chan)) in
                1
            | "a" -> 
                let chan = open_out_gen [Open_creat; Open_append] 0o644 fname in
                let () = Hashtbl.add htFiles fname (OutApp(chan)) in
                1
            | _  -> (* Should never be reached *)
                Printf.eprintf "Internal error while opening the file %s 
                (%s is not a valid mode)\n" fname mode ; exit (-1)
    with
        Sys_error(_) -> -1


let closeFile fname =
    try
		let chan = Hashtbl.find htFiles fname in
			Hashtbl.remove htFiles fname ;
			(match chan with 
				| In(chan) -> close_in chan  
				| Out(chan) | OutApp(chan) -> close_out chan) ; 1
	with
		Not_found -> -1 


let inputLineStdin fileId = read_line ()


(* Register all the functions which can be called from the C part *)
let registerCallbacks () = 
	Callback.register "ocaml_read_term_stdin" readTermStdin;
	Callback.register "ocaml_open" openFile;
	Callback.register "ocaml_close" closeFile;
	Callback.register "ocaml_read_term_file_id" readTermFileId;
	Callback.register "ocaml_input_n_chars" inputNChars;
	Callback.register "ocaml_input_line_stdin" inputLineStdin

