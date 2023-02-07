(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Nathan Guermond, Steven Holte, 
*  Gopalan Nadathur, Xiaochu Qi, Zach Snow
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

let multLine specList =
  specList
  |> List.map (fun (key1, key2, spec, doc1, doc2) ->
      [(key1, spec, doc1); (key2, spec, doc2)])
  |> List.flatten
  
let version = "2.2.0"

let printVersion () =
  print_endline ("Teyjus version " ^ version) ;
  exit 0

let versionspec =
  ("-v", "--version", Arg.Unit printVersion, " Return the system version", " \n")
  
let getModName name =
  try
    Filename.chop_extension name
  with
    | Invalid_argument _ -> name

let error str =
  prerr_endline ("Error: " ^ str) ;
  exit 1


(* All of the frontend tools take a single input file *)

let inputName = ref ""

let setInputName ?(filter=(fun x -> x))  name =
  if !inputName = "" then
    inputName := filter name
  else
    error "More than one input file specified."

let ensureInputName () =
  if !inputName = "" then
    error "No input file specified."
