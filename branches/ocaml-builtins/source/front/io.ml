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

type chan = In of in_channel | Out of out_channel;;

let htFiles = Hashtbl.create 2 

let readTermStdin term =
        Query.readTermChannel stdin (Module.getCurrentModule ())

let readTermFileId fileId = 
        try
                match Hashtbl.find htFiles fileId with
                | In(chan) -> 
                        Query.readTermChannel chan (Module.getCurrentModule ())
                | _ -> -1
        with
                Not_found -> -1 

let openFile fname mode = 
        let fnameHash = Hashtbl.hash fname in
        match mode with
        | "r" -> let chan = open_in fname in
                Hashtbl.add htFiles fnameHash (In(chan)) ; 
                fnameHash
        | "w" -> let chan = open_out fname in 
                Hashtbl.add htFiles fnameHash (Out(chan)) ; 
                fnameHash
        | _ -> Printf.printf "TODO: Unsupported mode\n" ; -1

let closeFile fileId  = 
        try
                let chan = Hashtbl.find htFiles fileId in
                Hashtbl.remove htFiles fileId ;
                (match chan with 
                | In(chan) -> close_in chan  
                | Out(chan) -> close_out chan) ; 1
        with
                Not_found -> -1 




let registerCallbacks () = 
        Callback.register "ocaml_read_term_stdin" readTermStdin;
        Callback.register "ocaml_open" openFile;
        Callback.register "ocaml_close" closeFile;
        Callback.register "ocaml_read_term_file_id" readTermFileId

