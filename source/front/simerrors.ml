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
exception Abort         (* exit the executable immediately *)
exception Exit          (* exit *)
exception TopLevel      (* return to the toplevel *)
exception Query         (* abort solving the query *)
exception QueryResult   (* query is solved; print answer *)
exception Fail          (* fail to simulator level *)

exception FatalError   

(* translate the simulator exceptions (c) into system exceptions (ocaml) *)
(* the expNumbers have to agree with those defined in system/error.h     *)
let handleSimExceptions expNumber =
  if expNumber = 0 then ()                    (* no error *)
  else if expNumber = 1 then ()               (* warnings *) 
  else if expNumber = 2 then raise Abort      (* exit executable immediately *)
  else if expNumber = 3 then raise Exit       (* exit *)
  else if expNumber = 4 then raise TopLevel   (* return to toplevel *)
  else if expNumber = 5 then raise Query      (* abort solving a query *)
  else if expNumber = 6 then raise QueryResult(* query is solved; print ans *)
  else if expNumber = 7 then raise Fail       (* failure at simulator level *)
  else raise FatalError
