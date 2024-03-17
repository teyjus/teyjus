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
(*                      IMPLICATION DEFINITIONS                            *)
(*                                                                         *)
(* Record definitions embedded in implication goals encountered during     *)
(* instruction generation for clauses                                      *)
(* Such information is used in the codegen module for generating           *)
(* implication points.                                                     *)
(***************************************************************************) 

val initImpPointList : unit -> unit
val getImpPointList  : unit -> Absyn.adefinitions list
val getNumImpPoints  : unit -> int
val addImpPointList  : Absyn.adefinitions -> unit 
val initTotImpPoints : unit -> unit
  
(***************************************************************************)
(*     REPRESENTATION AND MANIPULATION OF BACK PATCHING DATA               *)
(*                                                                         *)
(* These data structures and functions are needed to record call and       *)
(* and execute instructions whose jump address cannot be decided           *)
(* immediately in processing a clause: when all clauses in the module are  *)
(* translated into instructions, the jump labels are filled in for the     *)
(* recorded instructions.                                                  *)
(***************************************************************************)
val initBackPatchLists : unit -> unit
val backPatch : unit -> unit

(****************************************************************************)
(*                LOCAL CONSTANTS APPEARING IN ACC MODULES                  *)
(****************************************************************************)
val initAccConsts : unit -> unit
val setAccConsts : Absyn.aconstant list -> unit 

(***************************************************************************)
(*          GENERATING INSTRUCTIONS FOR ONE CLAUSE                         *)
(*                                                                         *)
(* Instructions enhanced with those for the given clause together with     *)
(* the new "next-instruction" offset will be returned. The definitions     *)
(* of implication goal in the clause are not processed yet, but recorded   *)
(* into the implPointList as a side-effect. Further, the target            *)
(* address (offset) of call and execute instructions cannot be decided in  *)
(* the current traversal, those instructions together with their           *)
(* corresponding predicates are added to relevant back patch lists for a   *)
(* later process to fill in their target addresses.                        *)
(***************************************************************************)
val genClauseCode : Absyn.aclause -> Instr.instruction list -> int ->
  (Instr.instruction list * int)
