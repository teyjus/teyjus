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
let isGoal : bool ref =  ref false
let setIsGoal data    =  isGoal := data
let getIsGoal ()      =  !isGoal

(* the number of arguments in the goal being processed *)
let numGoalArgs : int ref = ref 0
let setNumGoalArgs data   = numGoalArgs := data
let getNumGoalArgs ()     = !numGoalArgs

(***************************************************************************)
(*                      REGISTER  ARRAY                                    *)
(***************************************************************************)
(* number of available registers: agree with the simulator *)
let maxRegNum = 255

(* register array: the boolean value in the ith field indicates whether the *)
(* ith register is occupied or not.                                         *)
let regsArray : ((bool array) option) ref  = ref None

(* register array initialization: all registers are initially free          *)
let initRegsArray ()  = regsArray := Some(Array.make (maxRegNum+1) true)

(* set the ith entry in the register array to a given value                 *)
(* It is assumed that upon invocation of this function, regsArray must have *)
(* been initialized and the given index must be within the array length     *)
(* boundary.                                                                *)
let setRegsArray ind data =  
  match (!regsArray) with
    None -> 
      Errormsg.impossible Errormsg.none "setRegsArray: regsArray uninitialized"
  | Some(array) -> Array.set array ind data 

(* retrieve the value recorded in the nth element of the current regsArray. *)
(* Again, it is assumed that regsArray must have been initialized and the   *)
(* given index is within array length boundary.                             *)
let getRegsArrayNth ind =
  match (!regsArray) with
    None -> 
      Errormsg.impossible Errormsg.none "getRegsArrayNth: regsArray uninitialized"
  | Some(array) -> Array.get array ind 


(* return the minimal free register with index larger than i *)
let rec minimalFreeReg i =
  if (i > maxRegNum) then 
    (Errormsg.error Errormsg.none "unable to perform register assignment2";
     1)
  else  
    if (getRegsArrayNth i) then i
    else minimalFreeReg (i + 1)

(*****************************************************************************)
(*                   VARIABLE REGISTER CORRESPONDENCE LIST                   *)
(*****************************************************************************)
type regused = 
    RegVar   of int * Absyn.avar           (* (regNum, varData) *)
  | RegTyVar of int * Absyn.atypevar * bool(* (regNum, typeVarData, needed) *)

let regUseList : (regused list) ref = ref []

(* add a new reg-var/tyvar pair to the front of regUseList *)
let addRegUseList  data   = regUseList := data :: (!regUseList)



(* get the current value of the register usage list *)
let getRegUseList  ()     = !regUseList
(* set regUseList to hold the given list value *)
let setRegUseList myList = regUseList := myList 	


(* get the reg-var/tyvar pair corresponding to a given register number *)
(* It is assumed that the given regNum must be present in the current  *)
(* regUseList.                                                         *)
let getRegUseListRegNum regNum =
  let rec getRegUseListRegNumAux mylist =
    match mylist with
      [] -> 
	Errormsg.impossible Errormsg.none 
	  ("getRegUseListRegNum: unfound register number " ^ 
	   (string_of_int regNum) ^ " in reg use list")
    | (regUsage :: rest) ->
	match regUsage with
	  RegVar(regNum', _) -> 
	    if regNum' = regNum then regUsage
	    else getRegUseListRegNumAux rest
	| RegTyVar(regNum', _, _) ->
	    if regNum' = regNum then regUsage
	    else getRegUseListRegNumAux rest			
  in
  getRegUseListRegNumAux (getRegUseList ()) 

(* remove the reg-var/tyvar pair indexed by a given register number *)
(* from the current regUseList.                                     *)
let removeRegUseListRegNum regNum =
  let rec removeRegUseListAux myList =
    match myList with
      [] -> []
    | (regUsage :: rest) ->
	match regUsage with
	  RegVar(regNum', _) ->
	    if (regNum' = regNum) then rest
	    else (regUsage :: (removeRegUseListAux rest))
	| RegTyVar(regNum', _, _) ->
	    if (regNum' = regNum) then rest
	    else (regUsage :: (removeRegUseListAux rest))
  in
  setRegUseList (removeRegUseListAux (getRegUseList ()))

(***************************************************************************)
(*                 REGISTER ASSIGNMENT FUNCTIONS                           *)
(***************************************************************************)
(*  Mark a set of registers as used; done typically for head arguments *)
let rec markArgRegs n =
  if (n = 0) then ()
  else (setRegsArray n false; markArgRegs (n-1))

(*  Freeing a register that was used for an auxiliary purpose, i.e. for     *)
(*  a purpose other than holding a temporary                                *)
let markUnusedReg regNum = setRegsArray regNum true

(* getting a free register from the high end of the register array; the     *)
(* register is used for a purpose other than to hold a temporary variable   *)
let getHighFreeReg () =
  let rec getHighFreeRegAux regNum =
    if regNum = 0 then 
      (Errormsg.error Errormsg.none "unable to perform register assignment1";
       1)
    else
      if (getRegsArrayNth regNum) then (setRegsArray regNum false; regNum)
      else getHighFreeRegAux (regNum - 1)
  in
  getHighFreeRegAux maxRegNum


(* Freeing a register used for a temporary variable; if the variable used *)
(* a goal argument register,  regsArray will be unmarked by another       *)
(* process                                                                *)
let mkRegFree regNum = 
  (if not (getIsGoal() && (regNum <= getNumGoalArgs ())) 
   then setRegsArray regNum true
   else ());
  removeRegUseListRegNum regNum



(* assigning a chosen register to a term variable *)
let assignRegToTerm varData regNum =
  Absyn.setVariableDataOffset varData regNum; (* set varData offset *)
  addRegUseList (RegVar(regNum, varData));    (* record reg-var correpondence*)
  setRegsArray regNum false                   (* mark register as occupied *) 

(* assigning a chosen register to a type variable *)
let assignRegToType tyVarData need regNum =
  Absyn.setTypeVariableDataOffset tyVarData regNum; (*set tyVarData offset *)
  addRegUseList (RegTyVar(regNum, tyVarData, need));(*rcd reg-tyvar cpd    *) 
  setRegsArray regNum false                         (*mark reg as occupied *)



(* auxiliary function used in assignTmReg and assigTyReg                *)
(* calculate the sublist starting form the (n+1)th element of the given *)
(* list. Assume the given list must have length larger than n           *)
let rec subList args n =
  if (n = 0) then args
  else subList (List.tl args) (n - 1)

(* selecting a register to assign to a term variable; use chunk to guide *)
(* the decision, but do not assign a register in the interval 1 through  *)
(* lowval                                                                *)
let assignTmReg varData chunk lowval =
  (* try to decide the register number from the given argument list *)
  let rec assignTmArgReg args regNum =
	match args with
	  [] -> 0 (* fail to assign *)
	| (arg :: rest) ->
		if (Absyn.isTermFreeVariable arg) && 
		   (Absyn.getTermFreeVariableVariableData arg) == varData &&
		   (getRegsArrayNth regNum)
		then (assignRegToTerm varData regNum; regNum)
		else assignTmArgReg rest (regNum + 1)
		  
  in  
  (* try to decide the register number for this variable from the given  *)
  (* chunk.                                                              *)
  let rec assignTmRegFromChunk chunk lowval =
	match chunk with
	  [] -> (0, lowval)   (* fail to assign *) 
	| (goal :: rest) ->
		let numArgs = Absyn.getAtomicGoalNumberOfArgs goal in
		let newLowval  =  (* adjust smallest usable register number *)
		  if numArgs > lowval then numArgs
		  else lowval
		in
		if (Absyn.getAtomicGoalNumberOfTermArgs goal) > lowval then
		  let argsToTry = subList (Absyn.getAtomicGoalTermArgs goal) lowval in
		  let regNum = assignTmArgReg argsToTry (lowval + 1) in
		  if regNum = 0 then (* fail to assign in this goal; try next *)
			assignTmRegFromChunk rest newLowval
		  else (regNum, 0) (* success in assign *)
		else (* fail to assign in this goal; try next in the chunk *)
		  assignTmRegFromChunk rest newLowval			
  in
  let (regNum, newLowval) = assignTmRegFromChunk chunk lowval in
  if (regNum = 0) then (* fail to assign in previous steps *)
	  let newRegNum = minimalFreeReg (newLowval + 1) in
	  assignRegToTerm varData newRegNum;
	  newRegNum
  else regNum (* already find a suitable register *)
	
		  
		
(* Selecting a register to assign to a type; use chunk to guide the        *)
(* decision, but do not assign a register in the interval 1 through lowval *)
let assignTyReg varData needed chunk lowval =
  (* try to decide the register number from the given argument list *)
  let rec assignTyArgReg args regNum =
	match args with
	  [] -> 0 (* fail to assign *)
	| (arg :: rest) ->
		if (Absyn.isTypeFreeVariable arg) && 
		   (Absyn.getTypeFreeVariableVariableData arg) == varData &&
		   (getRegsArrayNth regNum)
		then (assignRegToType varData needed regNum; regNum)
		else assignTyArgReg rest (regNum + 1)
  in
  (* try to decide the register number for this variable from the given  *)
  (* chunk.                                                              *)
  let rec assignTyRegFromChunk chunk lowval =
    match chunk with
      [] -> (0, lowval)   (* fail to assign *) 
    | (goal :: rest) ->
	let numArgs = Absyn.getAtomicGoalNumberOfArgs goal in
	if (numArgs > lowval) 
	then
	  let numTermArgs = Absyn.getAtomicGoalNumberOfTermArgs goal in
	  let args = Absyn.getAtomicGoalTypeArgs goal in
	  let (argsToTry, startRegNum) =
	    if (lowval > numTermArgs) 
	    then (subList args (lowval - numTermArgs), lowval + 1)
	    else (args, numTermArgs + 1)
	  in
	  let regNum = assignTyArgReg argsToTry startRegNum in
	  if regNum = 0 then (* fail to assign in this goal; try next *)
	    assignTyRegFromChunk rest numArgs
	  else (regNum, 0) (* success in assign *)
	else (* fail to assign in this goal; try next in the chunk *)
	  assignTyRegFromChunk rest lowval	
  in
  let (regNum, newLowval) = assignTyRegFromChunk chunk lowval in
  if (regNum = 0) then (* fail to assign in previous steps *)
    let newRegNum = minimalFreeReg (newLowval + 1) in
    assignRegToType varData needed newRegNum;
    newRegNum
  else regNum (* already find a suitable register *)
	   

(* resolving a potential conflict for a term argument register *)
let resolveRegTermConflict term regNum chunk =
  if (getRegsArrayNth regNum) then ([], 0)
  else 
    let regUsage = getRegUseListRegNum regNum in
    match regUsage with
      RegTyVar(_, tyVarData, true) ->
	removeRegUseListRegNum regNum;
	([Instr.Ins_put_type_value_t(regNum, 
				     assignTyReg tyVarData true chunk 0)],
	 Instr.getSize_put_type_value_t)
    | RegTyVar(_, tyVarData, false) ->
	removeRegUseListRegNum regNum;
	let _ = assignTyReg tyVarData false chunk 0 in
	([], 0) (* no instruction is generated *)
    | RegVar(_, varData) ->
	if (Absyn.isTermFreeVariable term) &&
	  ((Absyn.getTermFreeVariableVariableData term) == varData)
	then ([], 0)
	else
	  (removeRegUseListRegNum regNum;
	   ([Instr.Ins_put_value_t(regNum, assignTmReg varData chunk 0)],
	    Instr.getSize_put_value_t))
			  

(* resolving a potential conflict for a type argument register *)
let resolveRegTypeConflict tyExp regNum chunk =
  if (getRegsArrayNth regNum) then ([], 0)
  else
    let regUsage = getRegUseListRegNum regNum in
    match regUsage with
      RegVar(_, varData) ->
	removeRegUseListRegNum regNum;
	([Instr.Ins_put_value_t(regNum, assignTmReg varData chunk 0)],
	 Instr.getSize_put_value_t)
    | RegTyVar(_, tyVarData, needed) ->
	if (Absyn.isTypeFreeVariable tyExp) && 
	  ((Absyn.getTypeFreeVariableVariableData tyExp) == tyVarData)
	then ([], 0)
	else
	  (removeRegUseListRegNum regNum;
	   let newRegNum = assignTyReg tyVarData true chunk 0 in
	   if (needed) then
	     ([Instr.Ins_put_type_value_t(regNum, newRegNum)],
	      Instr.getSize_put_type_value_t)
	   else 
	     ([], 0))
	    
(* Cleaning up goal register usage information; this is done after   *)
(* generating code for putting the arguments for a goal              *)
let cleanUpRegs () =
  let rec cleanUpArgRegs regNum =
	if (regNum = 0) then ()
	else (setRegsArray regNum true; cleanUpArgRegs (regNum - 1))
  in
  let rec setFromRegUseList myList =
	match myList with
	  [] -> ()
	| (RegVar(regNum, _) :: rest) ->
		setRegsArray regNum false; setFromRegUseList rest
	| (RegTyVar(regNum, _, _) :: rest) ->
		setRegsArray regNum false; setFromRegUseList rest
  in
  cleanUpArgRegs (getNumGoalArgs ());
  setNumGoalArgs 0;
  setFromRegUseList (getRegUseList ())
		  
	   
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
let chunkify goal =
  let rec chunkifyAux goal chunks =
	let rec addToLastChunk chunks mygoal =
	  match chunks with
		[] -> [[mygoal]]
	  | (chunk :: rest) -> 
		  if (rest = []) then [chunk @ [mygoal]] 
		  else (chunk :: (addToLastChunk rest mygoal))
	in
	match goal with
      Absyn.AtomicGoal(pred, _, _, _, _) ->
		if ((Pervasive.isPerv pred) && (not (Pervasive.regClobberingPerv pred))
			  && (not (Pervasive.backtrackablePerv pred)))
		then (addToLastChunk chunks goal)
		else (chunks @ [[goal]])
	| Absyn.AndGoal(lgoal, rgoal) ->
		let newChunks = chunkifyAux lgoal chunks in
		chunkifyAux rgoal newChunks
	| Absyn.ImpGoal(_, _, _, body) -> chunkifyAux body chunks
	| Absyn.AllGoal(_, body)    -> chunkifyAux body chunks
	| Absyn.SomeGoal(_, body)   -> chunkifyAux body chunks
	| _ -> Errormsg.impossible Errormsg.none "chunkify: cutfail goal"
  in
  chunkifyAux goal []
