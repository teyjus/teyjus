(****************************************************************************
*Copyright 2008, 2009 Zach Snow
****************************************************************************)
(****************************************************************************
* This file is part of Parinati.
*
* Parinati is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Parinati is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Parinati.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************)

let fileNotFound f =
  (print_endline ("Error: unable to open file '" ^ f ^ "'.");
  exit (-1))

(******************************************************************
*openFile:
* Open a file and exit if there is an error.
******************************************************************)
let openFile fname f =
  try
    let inchannel = f fname in
    inchannel
  with Sys_error(s) -> fileNotFound fname

(******************************************************************
*closeFile:
* Closes a file given a closing function.
******************************************************************)
let closeFile fname f =
  f fname

(******************************************************************
*parse:
******************************************************************)
let parse filename =
  try
    let inchannel = (openFile filename open_in) in
    let lexbuf = Lexing.from_channel inchannel in
    let () = Lflexer.setFileName lexbuf filename in
    let result = Lfparser.parse Lflexer.initial lexbuf in
    (closeFile inchannel close_in;
    Some result)
  with
      Parsing.Parse_error -> (print_endline "Error: syntax error."; None)
    | Failure(s) -> (print_endline ("Error: " ^ s ^ "."); None)


let parse_query string =
  try
    if (Sys.file_exists string)
    then
      let inchannel = (openFile string open_in) in
      let lexbuf = Lexing.from_channel inchannel in
      let () = Lflexer.setFileName lexbuf string in
      let result = Lfparser.parseQuery Lflexer.initial lexbuf in
      let _ = closeFile inchannel close_in;
              print_endline ("The query:\n"^(Lfabsyn.string_of_query result)^"\n") in
      Some result
    else
      let lexbuf = Lexing.from_string string in
      let result = Lfparser.parseQuery Lflexer.initial lexbuf in
      let _ = print_endline ("The query:\n"^(Lfabsyn.string_of_query result)^"\n") in
      Some result
  with
      Parsing.Parse_error -> 
        (print_endline "Error: syntax error."; None)
    | Failure(s) -> (print_endline ("Error: " ^ s ^ "."); None)    
