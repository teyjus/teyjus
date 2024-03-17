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
(***************************************************************************)
(* Implement a mapping from code offset to labels                          *)
(***************************************************************************)
type label = string 

let hashtable = Hashtbl.create 1

let nextIndex   = ref 0

let label offset =
  try
	Hashtbl.find hashtable offset
  with
	Not_found -> 
	  let index    = !nextIndex in 
	  let newlabel = ("L" ^ (string_of_int index)) in
	  nextIndex := index + 1;
	  Hashtbl.add hashtable offset newlabel;
	  newlabel


let assignLabel offset name =
  try 
	let l = Hashtbl.find hashtable offset in
	Errormsg.impossible Errormsg.none 
	  ("assignLabel: label " ^ l ^ " has already been assigned to address " ^
	   (string_of_int offset)) 
  with
	Not_found ->
	  Hashtbl.add hashtable offset name
	
let addLabel offset =
  try
	let _ = Hashtbl.find hashtable offset in
	()
  with
	Not_found ->
	  let index    = !nextIndex in 
	  let newlabel = ("L" ^ (string_of_int index)) in
	  nextIndex := index + 1;
	  Hashtbl.add hashtable offset newlabel

let findLabel offset =
  try
	Hashtbl.find hashtable offset
  with
	Not_found -> ""

