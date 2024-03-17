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
(* Data structure for recording the module information from disassembling  *)
(***************************************************************************)

(***************************************************************************)
(* kind information:                                                       *)
(***************************************************************************)
type kinds = Absyn.akind option array

(***************************************************************************)
(* type skeleton information:                                              *)
(***************************************************************************)
type typeskels = Absyn.atype list

(***************************************************************************)
(* constant information:                                                   *)
(***************************************************************************)
type constants = Absyn.aconstant option array

(***************************************************************************)
(* string information:                                                     *)
(***************************************************************************)
type strings = string list

(***************************************************************************)
(* implication tables:                                                     *)
(* impltable: (next clause table, find code fn, search table)              *)
(***************************************************************************)
type impltables = impltable list
and  impltable  = (Absyn.aconstant list * int * Absyn.aconstant list)

(***************************************************************************)
(* hash tables:                                                            *)
(* hash table:  (constant, label)                                          *)
(***************************************************************************)
type hashtables = ((Absyn.aconstant * Label.label) list) array

(***************************************************************************)
(* module table:                                                           *)
(* (next clause table, exportdef pred table, local pred table,             *)
(*  find code fn, search table)                                            *)
(***************************************************************************)
type moduletable =
	(Absyn.aconstant list * Absyn.aconstant list * Absyn.aconstant list *
	   int * Absyn.aconstant list)

(***************************************************************************)
(* import table: (needed for linked code)                                  *)
(* (number of code segs, next clause table, local constant table,          *)
(*  find code fn, search table)                                            *)
(***************************************************************************)
type importtable =
	(int * Absyn.aconstant list * Absyn.aconstant list * int * 
	   Absyn.aconstant list)

type moduletables =
	ModuleTable  of moduletable
  | ImportTables of (importtable list)

(***************************************************************************)
(* renaming tables: (import/accumulate)                                    *)
(***************************************************************************)
type renamingtables = 
(string * ((string * Absyn.akind) list) * (string * Absyn.aconstant) list) list

(***************************************************************************)
(* module information:                                                     *)
(*   filename                                : string                      *)
(*   bytecode version number                 : int                         *)
(*   modname                                 : string                      *)
(*   codesize                                : int                         *)
(*   global kinds                            : kinds                       *)
(*   local  kinds                            : kinds                       *)
(*   type skeletons                          : typeskels                   *)
(*   global constants                        : constants                   *)
(*   local constants                         : constants                   *)
(*   hidden constants                        : constants                   *)
(*   strings                                 : strings                     *)
(*   implication table                       : impltables                  *)
(*   hash table                              : hashtables                  *)
(*   module table                            : moduletable                 *)
(*   accumulate renaming table               : renamingtables              *)
(*   import renaming table                   : renamingtables              *)
(*   instructions                            : Absyn.instructions          *)
(***************************************************************************)
type modcontext =
	ModContext of (string * int * string * int * kinds * kinds * typeskels *
				  constants * constants * constants * strings * impltables *
				  hashtables * moduletables * renamingtables * renamingtables *
				  Instr.instruction list)

(***************************************************************************)
(* Displaying module context                                               *)
(***************************************************************************)
val displayModContext : modcontext -> bool -> bool -> unit
