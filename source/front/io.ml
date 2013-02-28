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

let readTermStdin term =
	Query.readTermChannel stdin (Module.getCurrentModule ())

let readTermFileId fileId = 
	try match Hashtbl.find htFiles fileId with
		| In(chan) -> 
		  Query.readTermChannel chan (Module.getCurrentModule ())
		| _ -> -1
	with
		Not_found -> -1 

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

(* Register all the functions which can be called from the C part *)
let registerCallbacks () = 
	Callback.register "ocaml_read_term_stdin" readTermStdin;
	Callback.register "ocaml_open" openFile;
	Callback.register "ocaml_close" closeFile;
	Callback.register "ocaml_read_term_file_id" readTermFileId

