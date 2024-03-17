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
(**********************************************************************
*Symbol Module:
* Implements a simple module mapping strings to unique identifiers.
* Uses the standard library Hashtbl module.
* Associated a name for display with each entry, the default of which
* is the same as the mapped string.
**********************************************************************)
type symbol = string * string * int

let hashtable = Hashtbl.create 1

let nextsym = ref 0

let name (s,_,_) = s

let printName (_,s,_) = s

let symbol s =
  try
    (s, s, (Hashtbl.find hashtable s))
  with
    Not_found ->  let id : int = !nextsym
                  in
                    begin
                      nextsym := id + 1;
                      (Hashtbl.add hashtable s id);
                      (s, s, id)
                    end

let symbolAlias s s' =
  try 
    (s, s', (Hashtbl.find hashtable s))
  with
    Not_found ->  let id : int = !nextsym
                  in
                    begin
                      nextsym := id + 1;
                      (Hashtbl.add hashtable s id);
                      (s, s', id)
                    end

let id (_, _, n) = n

let currentId = ref 0

let generateName s =
  let s' = s ^ "_gen_" ^ (string_of_int (!currentId)) in
  let s'' = s ^ "g_gen_" ^ (string_of_int (!currentId)) in
  let _ = currentId := !currentId + 1 in
    symbolAlias s' s''

let generate () =
  generateName ""


let equal s1 s2 = (id s1) = (id s2)
