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
(*                      REGISTER ALLOCATION CODE                           *)
(*                                                                         *)
(* Two parallel data structures are used for register allocation:          *)
(* 1) There is a boolean array that records whether a register is or is    *)
(*    not free;                                                            *)
(* 2) There is a list that records which variable sits in a register if    *)
(*    indeed one sits in the register.                                     *)
(*  Note that situation could arise that a register is not free but not    *)
(*  assigned to a variable either: it may be used in building a term,      *)
(*  unifying a function type, temporarily holding embedded structures or   *)
(*  may be needed for incoming or outgoing arguments.                      *)
(*  If a register is used for an auxiliary purpose, it will be freeed as   *)
(*  soon as that purpose is over. If a register is used for holding a      *)
(*  variable, then it will be freed as soon as the last occurrence of      *)
(*  the variable is processed.                                             *)
(*  Conflicts arise when a register is holding a type or term variable at  *)
(*  a point when the register is needed for passing an argument that is    *)
(*  different from itself. In such a case, a new register assignment is    *)
(*  made looking at top-level argument positions for the variable in the   *)
(*  current goal and subsequent ones in the present chunk.                 *)
(*  The secondary data structure (the variable-register correspondence     *)
(*  list is necessary for this moving.                                     *)
(*  Fresh register assignments for multiple use top-level and embedded     *)
(*  variables looks only at subsequent goals in the chunk.                 *)
(*                                                                         *)
(* The assumption is  made that (a) register values are preserved within   *)
(* calls to all but the last goal in a chunk, and (b) no register beyond   *)
(* those used for parameter passing is touched by the code for any but the *)
(* last goal in the chunk.                                                 *)
(*                                                                         *)
(* Registers for intermediate structures are allocated from the high end.  *)
(* This is done so as to permit low register numbers to be allocated to    *)
(* genuine temporary variables.  As already mentioned, register            *)
(* allocation for multiple occurrence temporary variables tries to pay     *)
(* attention to top-level argument locations within a chunk at which the   *)
(* variable appears. If an assignment on this basis is not possible, then  *)
(* the first free register not needed for argument passing within the chunk*)
(* is picked. For single occurrence variables, high end registers are      *)
(* temporarily assigned and released.                                      *)
(*                                                                         *)
(*                                                                         *)
(*  The treatment of "unneeded" type variables is a little tricky:         *)
(*  first, registers are still assigned for such variables, but no         *)
(*  instructions are generated for them; thus, if a unneeded type variable *)
(*  is found in the variable-register mapping list when resolving a        *)
(*  conflict, a new register is still assigned for that type variable, but *)
(*  no data movement instruction is generated. Furthermore, the way that   *)
(*  the new register is chosen decides that when the type variable is      *)
(*  processed in generating code for the body goal (in which case, it can  *)
(*  only appear as a top-level argument of an atomic goal), it already sits*)
(*  in the correct argument register, and so no data movement instruction  *)
(*  will be needed anyway. For this reason, in generating "put"            *)
(*  instructions for setting goal arguments, there is no need to           *)
(*  differentiate unneeded type variables from the others.                 *)
(***************************************************************************)

(***************************************************************************)
(*                       AUXILIARY FLAGS                                   *)
(***************************************************************************)

(* these variables are used for communication concerning goal argument     *)
(* registers (that should not be freed even if a variable in them becomes  *)
(* dead) between atomic goals and the register allocator                   *)

(* is processing a goal ? *)
val setIsGoal : bool -> unit
val getIsGoal : unit -> bool

(* the number of arguments in the goal being processed *)
val setNumGoalArgs : int  -> unit
val getNumGoalArgs : unit -> int 


(***************************************************************************)
(*                      REGISTER  ARRAY                                    *)
(***************************************************************************)
(* register array initialization: all registers are initially free         *)
val initRegsArray : unit -> unit


(***************************************************************************)
(*                 REGISTER ASSIGNMENT FUNCTIONS                           *)
(***************************************************************************)
(*  Mark a set of registers as used; done typically for head arguments *)
val markArgRegs : int -> unit
	
(*  Freeing a register that was used for an auxiliary purpose, i.e. for     *)
(*  a purpose other than holding a temporary                                *)
val markUnusedReg : int -> unit

(* getting a free register from the high end of the register array; the     *)
(* register is used for a purpose other than to hold a temporary variable   *)
val getHighFreeReg : unit -> int

(* Freeing a register used for a temporary variable; if the variable used *)
(* a goal argument register,  regsArray will be unmarked by another       *)
(* process                                                                *)
val mkRegFree : int -> unit

(* assigning a chosen register to a term variable *)
val assignRegToTerm : Absyn.avar -> int -> unit
(* assigning a chosen register to a type variable *)
val assignRegToType : Absyn.atypevar -> bool -> int -> unit

(* selecting a register to assign to a term variable; use chunk to guide *)
(* the decision, but do not assign a register in the interval 1 through  *)
(* lowval                                                                *)
val assignTmReg : Absyn.avar -> (Absyn.agoal) list -> int -> int

(* Selecting a register to assign to a type; use chunk to guide the        *)
(* decision, but do not assign a register in the interval 1 through lowval *)
val assignTyReg : Absyn.atypevar -> bool -> (Absyn.agoal) list -> int -> int

(* resolving a potential conflict for a term argument register *)
val resolveRegTermConflict : Absyn.aterm -> int -> (Absyn.agoal) list ->
  ((Instr.instruction) list * int)

(* resolving a potential conflict for a type argument register *)
val resolveRegTypeConflict : Absyn.atype -> int -> (Absyn.agoal) list ->
  ((Instr.instruction) list * int)

(* Cleaning up goal register usage information; this is done after   *)
(* generating code for putting the arguments for a goal              *)
val cleanUpRegs : unit -> unit


(***************************************************************************)
(*                  FACTORING A GOAL INTO A LIST OF `CHUNKS'               *)
(*                                                                         *)
(* A chunk is a sequence of goals that retain their register arguments and *)
(* don't utilize any registers other that the ones in which their arguments*)
(* are provided. Presently, all builtins other than SOLVE are assumed to   *)
(* have this character. Register allocation is done across a chunk. The    *)
(* code below breaks a goal into a list of chunks that is processed as a   *)
(* parallel data structure to the goal itself for the purposes of register *)
(* allocation. Note that each chunk is represented as a list of goals.     *)
(*                                                                         *)
(***************************************************************************)
val chunkify : Absyn.agoal -> (Absyn.agoal list) list 
