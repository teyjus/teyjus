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
let impPointList : (Absyn.adefinitions list) ref = ref []
let numImpPoints : int ref = ref 0
let totImpPoints : int ref = ref 0

let initImpPointList () = (impPointList := [];
                           totImpPoints := !totImpPoints + !numImpPoints;
                           numImpPoints := 0)
let getImpPointList  () = (!impPointList)
let getNumImpPoints  () = (!numImpPoints)
let getTotImpPoints  () = (!totImpPoints)

let initTotImpPoints () = (totImpPoints := 0)

let addImpPointList impPoint = 
  (impPointList := impPoint :: (!impPointList); 
   numImpPoints := !numImpPoints + 1)

(***************************************************************************)
(*     REPRESENTATION AND MANIPULATION OF BACK PATCHING DATA               *)
(*                                                                         *)
(* These data structures and functions are needed to record call and       *)
(* and execute instructions whose jump address cannot be decided           *)
(* immediately in processing a clause: when all clauses in the module are  *)
(* translated into instructions, the jump labels are filled in for the     *)
(* recorded instructions.                                                  *)
(***************************************************************************)
let executePatch : ((Absyn.aconstant * Instr.instruction list) list) ref 
	= ref []
let callPatch : ((Absyn.aconstant * Instr.instruction list) list) ref = ref []
	
let initBackPatchLists () = (executePatch := []; callPatch := [])
let getExecutePatch    () = !executePatch
let setExecutePatch data  = executePatch := data
let getCallPatch       () = !callPatch
let setCallPatch data     = callPatch := data 

let rec addBackPatch pred instr patchList temp =
  match patchList with
	[] -> ((pred, [instr]) :: (List.rev temp))
  | ((pred', insts) :: rest) ->
	  if (pred' == pred) then 
		(List.rev temp) @ ((pred', (instr :: insts)) :: rest)
	  else
		addBackPatch pred instr rest ((pred', insts) :: temp)

let addBackPatchExecute pred instr =
  let newList = addBackPatch pred instr (getExecutePatch ()) []
  in setExecutePatch newList

let addBackPatchCall pred instr =
  let newList = addBackPatch pred instr (getCallPatch ()) []
  in setCallPatch newList

let backPatch () =
  let rec patchExecute insts codeLoc =
    match insts with
      [] -> ()
    | (Instr.Ins_execute(codeLocRef) :: rest) ->
	codeLocRef := codeLoc;
	patchExecute rest codeLoc
    | _ -> Errormsg.impossible Errormsg.none 
		     "backPatch: invalid execute patch list"
  in

  let rec patchCall insts codeLoc =
    match insts with
      [] -> ()
    | (Instr.Ins_call(_, codeLocRef) :: rest) ->
	codeLocRef := codeLoc;
	patchCall rest codeLoc
    | _ ->  Errormsg.impossible Errormsg.none 
		      "backPatch: invalid call patch list"
  in

  let rec backPatchAux patchList patchFunc =
    match patchList with
      [] -> ()
    | ((pred, insts)::rest) -> 
	let codeLoc = Absyn.getConstantCodeInfoClausesIndex pred in
	patchFunc insts codeLoc;
	backPatchAux rest patchFunc
  in
  backPatchAux (getExecutePatch ()) patchExecute;
  backPatchAux (getCallPatch ()) patchCall

(****************************************************************************)
(*                LOCAL CONSTANTS APPEARING IN ACC MODULES                  *)
(****************************************************************************)
let accConsts: (Absyn.aconstant list) ref = ref []
let setAccConsts consts = accConsts := consts
let initAccConsts ()    = setAccConsts []
let getAccConsts ()     = !accConsts

(****************************************************************************)
(*                  GENERATING CODE FOR TYPES                               *)
(****************************************************************************)

(************************************************************************)
(*       GENERATING CODE FOR SYTHESIZING TYPES                          *)
(************************************************************************)
(* data structure for holding a register number or type expresion in    *)
(* synthesizing types                                                   *)
type regortype = 
	RegTyReg of int
  | RegTyTy  of Absyn.atype

(************************************************************************)
(* genSTypesCode:                                                       *)
(* generate type synthesis code from a list of types paired with the    *)
(* registers in which the created structure is to be put.               *)
(************************************************************************)
let rec genSTypesCode regTypePairs chunk lowval last =
  let rec genSTypesCodeAux regTypePairs instsBlocks size =
	match regTypePairs with
	  [] -> (List.flatten (List.rev instsBlocks), size)
	| ((regNum, tyExp) :: rest) ->
		let (oneInstsBlock, oneSize) =
		  genSTypeCode regNum tyExp chunk lowval last 
		in
		genSTypesCodeAux rest (oneInstsBlock :: instsBlocks) (oneSize + size)
  in
  genSTypesCodeAux regTypePairs [] 0
	
(* generate type synthesis code from a type-register pair               *)
and genSTypeCode regNum tyExp chunk lowval last =

  (* AUX FUNC1: generate type synthesis code for a type variable *)
  let genSTypeVarCode var =
	(* first occurrence of a temporary variable *)
	let genSTypeFirstTempVar varData lastUse =
	  let newRegNum =
		if (lastUse) then regNum
		else Registers.assignTyReg varData true chunk lowval
	  in
	  ([Instr.Ins_put_type_variable_t(regNum, newRegNum)],
	   Instr.getSize_put_type_variable_t)
	in
	(* subsequent occurrence of a permanent variable *)
    let genSTypeSubPermVar varData offset =
	  if (not last) || (Absyn.getTypeVariableDataSafety varData)
	  then
		([Instr.Ins_put_type_value_p(offset, regNum)],
		 Instr.getSize_put_type_value_p)
	  else
		(Absyn.setTypeVariableDataSafety varData true;
		 ([Instr.Ins_put_type_unsafe_value(offset, regNum)],
		  Instr.getSize_put_type_unsafe_value))
	in
	(* subsequent occurrence of a temporary variable *)
	let genSTypeSubTempVar varData offset lastUse =
	  let (inst, size) =
		if (offset = regNum) then ([], 0)
		else
		  ([Instr.Ins_put_type_value_t(offset, regNum)],
		   Instr.getSize_put_type_value_t)
	  in
	  if (lastUse) then (Registers.mkRegFree offset; (inst, size))
	  else (inst, size)
	in

	(* function body of genSTypeVarCode *) 
	let varData = Absyn.getTypeFreeVariableVariableData var in
	let lastUse = (Absyn.getTypeVariableDataLastUse varData == var) in 
    match (Absyn.getTypeFreeVariableFirst var),
	      (Absyn.getTypeVariableDataPerm varData) with
	  true , true  -> (* first occurrence of a permanent variable *)
		([Instr.Ins_put_type_variable_p
			(Absyn.getTypeVariableDataOffset varData, regNum)],
		 Instr.getSize_put_type_variable_p)
    | true , false -> (* first occurrence of a temporary variable *)
		genSTypeFirstTempVar varData lastUse
    | false, true  -> (* subsequent occurrence of a permanent variable *)
		genSTypeSubPermVar varData (Absyn.getTypeVariableDataOffset varData) 
    | false, false -> (* subsequent occurrence of a temporary variable *)
		genSTypeSubTempVar varData (Absyn.getTypeVariableDataOffset varData)
		  lastUse
  in

  (* AUX FUNC2: generate type synthesis code for a sort *)
  let genSTypeSortCode kind =
	([Instr.Ins_put_type_const(regNum, kind)], Instr.getSize_put_type_const)
  in

  (* AUX FUNC3: generate type synthesis code for a type structure       *)    
  let genSTypeStructureCode kind args =
	let (argsCode, argsCodeSize, regOrTypeList) =
	  genSTypeArgsCode args chunk lowval 
	in
	let (putCode, putCodeSize) =
	  (Instr.Ins_put_type_structure(regNum,kind), 
	   Instr.getSize_put_type_structure)
	in
	let (argsSettingCode, argsSettingCodeSize) =
	  genTypeSettingCode regOrTypeList chunk lowval
	in
	(argsCode @ (putCode :: argsSettingCode), 
	 argsCodeSize + putCodeSize + argsSettingCodeSize)
  in
  
  (* AUX FUNC4: generate type synthesis code for a type arrow           *)
  let genSTypeArrowCode args =
	let (argsCode, argsCodeSize, regOrTypeList) =
	  genSTypeArgsCode args chunk lowval
	in
	let (putCode, putCodeSize) =
	  (Instr.Ins_put_type_arrow(regNum), Instr.getSize_put_type_arrow)
	in
	let (argsSettingCode, argsSettingCodeSize) =
	  genTypeSettingCode regOrTypeList chunk lowval
	in
	(argsCode @ (putCode :: argsSettingCode), 
	 argsCodeSize + putCodeSize + argsSettingCodeSize)	
  in

  (* dispatching *)
  match tyExp with
    Absyn.TypeVarType(_)              ->           (* type variable *) 
	  genSTypeVarCode tyExp 
  | Absyn.ApplicationType(kind, [])   ->           (* sort *)
	  genSTypeSortCode kind  
  | Absyn.ApplicationType(kind, args) ->           (* type structure *)
	  genSTypeStructureCode kind args 	
  | Absyn.ArrowType(arg, target)      ->           (* type arrow *) 
	  genSTypeArrowCode [arg;target] 
  | _ -> Errormsg.impossible Errormsg.none "genSTypeCode: invalid type exp"

(****************************************************************)
(*  genSTypeArgsCode:                                           *)
(*  Synthesizing the embedded types of a function or arrow type *)
(****************************************************************)
and genSTypeArgsCode args chunk lowval = 
  let rec genSTypeArgsCodeAux args insts size rtpairs =
	match args with
	  [] -> (List.flatten (List.rev insts), size, List.rev rtpairs)
	| (tyExp :: rest) ->
		match tyExp with
		  Absyn.TypeVarType(_)            -> (* type variable: delayed *)
			genSTypeArgsCodeAux rest insts size (RegTyTy(tyExp)::rtpairs)
		| Absyn.ApplicationType(kind, []) -> (* sort: delayed *)
			genSTypeArgsCodeAux rest insts size (RegTyTy(tyExp)::rtpairs)
		| _                               -> (* type structure or type arrow *)
			let regNum = Registers.getHighFreeReg () in
			let (oneInstsBlock, oneInstsBlockSize) =
			  genSTypeCode regNum tyExp chunk lowval false 
			in
			genSTypeArgsCodeAux rest (oneInstsBlock :: insts) 
			  (oneInstsBlockSize + size) (RegTyReg(regNum)::rtpairs)
  in
  genSTypeArgsCodeAux args [] 0 []

(**********************************************************************)
(* genTypeSettingCode:                                                *)
(* Actually writing out the component structures of a structured type *)
(* Note the type expression appearing in the regTypeList can only be  *)
(* sort or type variables.                                            *)
(**********************************************************************)
and genTypeSettingCode regTypeList chunk lowval =

  (* generate "set_type_variable(value)" instruction *)
  let genTypeSettingCodeVar var =

	(* first occurrence of a temporary variable *)
	let genTypeSettingCodeFirstTempVar varData lastUse =
	  let regNum =
		if (lastUse) then Registers.getHighFreeReg ()
		else Registers.assignTyReg varData true chunk lowval
	  in
	  let (inst, size) =
		(Instr.Ins_set_type_variable_t(regNum), 
		 Instr.getSize_set_type_variable_t) 
	  in
	  if (lastUse) then (Registers.markUnusedReg regNum; (inst, size))
	  else (inst, size)
	in
	(* subsequent occurrence of a permanent variable *)
	let genTypeSettingCodeSubPermVar varData offset =
	  if (Absyn.getTypeVariableDataHeapVar varData) 
	  then (Instr.Ins_set_type_value_p(offset), Instr.getSize_set_type_value_p)
	  else 
		(Absyn.setTypeVariableDataHeapVar varData true;
		 Absyn.setTypeVariableDataSafety varData true;
		 (Instr.Ins_set_type_local_value_p(offset),
		  Instr.getSize_set_type_local_value_p))
    in
	(* subsequent occurrence of a temporary variable *)
	let genTypeSettingCodeSubTempVar varData offset lastUse =
	  let (inst, size) =
		if (Absyn.getTypeVariableDataHeapVar varData) 
		then (Instr.Ins_set_type_value_t(offset),
			  Instr.getSize_set_type_value_t)
		else 
		  (Absyn.setTypeVariableDataHeapVar varData true;
		   Absyn.setTypeVariableDataSafety varData true;
		   (Instr.Ins_set_type_local_value_t(offset),
			Instr.getSize_set_type_local_value_t))
	  in
	  if (lastUse) then (Registers.mkRegFree offset; (inst, size))
	  else (inst, size)
    in

	(* function body of genTypeSettingCodeVar *)
	let varData = Absyn.getTypeFreeVariableVariableData var in
	let lastUse = (Absyn.getTypeVariableDataLastUse varData) == var in 
    match (Absyn.getTypeFreeVariableFirst var),
	      (Absyn.getTypeVariableDataPerm varData) 
	with  
	  true , true  -> (* first occurrence of a permanent variable *)
		(Instr.Ins_set_type_variable_p
		   (Absyn.getTypeVariableDataOffset varData),
		 Instr.getSize_set_type_variable_p)
    | true , false -> (* first occurrence of a temporary variable *)
		genTypeSettingCodeFirstTempVar varData lastUse
    | false, true  -> (* subsequent occurrence of a permanent variable *) 
		genTypeSettingCodeSubPermVar varData 
		  (Absyn.getTypeVariableDataOffset varData)
    | false, false -> (* subsequent occurrence of a temporary variable *)
		genTypeSettingCodeSubTempVar varData 
		  (Absyn.getTypeVariableDataOffset varData) lastUse
    in	  

  let rec genTypeSettingCodeAux regTypeList insts size =
	match regTypeList with
	  [] -> (List.rev insts, size)
	| ((RegTyReg regNum) :: rest) ->
		Registers.markUnusedReg regNum;
		genTypeSettingCodeAux rest 
		  (Instr.Ins_set_type_value_t(regNum) :: insts)
		  (Instr.getSize_set_type_value_t + size)
	| ((RegTyTy (Absyn.ApplicationType(kind, []))) :: rest) ->  (* sort *)
		genTypeSettingCodeAux rest
		  (Instr.Ins_set_type_constant(kind) :: insts)
		  (Instr.getSize_set_type_constant + size)
	| ((RegTyTy var) :: rest) -> (* type variable *)
		let (inst, instSize) = genTypeSettingCodeVar var in
		genTypeSettingCodeAux rest (inst :: insts) (instSize + size)
  in
  genTypeSettingCodeAux regTypeList [] 0


(************************************************************************)
(*       GENERATING CODE FOR ANALYZING TYPES                            *)
(************************************************************************)

(**************************************************************************)
(* genATypesCode:                                                         *)
(* Translating a delayed list of register, type pairs. The delaying could *)
(* be because the types are embedded inside a structure or because they   *)
(* are head arguments. In both cases, no variable types are delayed. For  *)
(* head arguments, constants may have been delayed.                       *)
(**************************************************************************)
let rec genATypesCode delayed chunk =
  (* for each pair in the delayed list: Note type variable won't get delayed*)
  let genATypeCode tyExp regNum =
	match tyExp with
	  Absyn.ApplicationType(kind, [])   ->  (* sort *)
		([Instr.Ins_get_type_constant(regNum, kind)],
		 Instr.getSize_get_type_constant)
	| Absyn.ApplicationType(kind, args) ->  (* structure *)
		let (argsCode, argsCodeSize) = genATypeArgsCode args chunk false in 
		(Instr.Ins_get_type_structure(regNum, kind) :: argsCode, 
		 Instr.getSize_get_type_structure + argsCodeSize)
	| Absyn.ArrowType(arg, target)      ->  (* type arrow *)
		let (argsCode, argsCodeSize) = 
		  genATypeArgsCode [arg ; target] chunk false 
		in 
		(Instr.Ins_get_type_arrow(regNum) :: argsCode, 
		 Instr.getSize_get_type_arrow + argsCodeSize) 
	| _ -> Errormsg.impossible Errormsg.none "genATypesCode: invalid type exp"
  in
  
  let rec genATypesCodeAux delayed insts size =
	match delayed with
	  [] -> (List.flatten (List.rev insts), size)
	| ((regNum, tyExp) :: rest) ->
		Registers.markUnusedReg regNum;
		let (oneInstsBlock, oneInstsBlockSize) = genATypeCode tyExp regNum in
		genATypesCodeAux rest (oneInstsBlock :: insts) (oneInstsBlockSize+size)
	
  in
  genATypesCodeAux delayed [] 0

(**************************************************************************)
(* genATypeArgsCode:                                                      *)
(* Processing the argument types of a structure (type arrow) or the type  *)
(* environment of a constant; constEnv is a boolean flag indicating which *)
(**************************************************************************)
and genATypeArgsCode args chunk constEnv =	
  let rec genATypeArgsCodeAux args delayed insts size =
	(* type variable *)
	let genATypeArgVariable var =
	  (* first occurrence of a temporary variable *)
	  let genAFirstTempTypeVar varData lastUse =
		let regNum =
		  if lastUse then
			let myRegNum = Registers.getHighFreeReg () in
			Registers.markUnusedReg myRegNum;
			myRegNum
		  else
			Registers.assignTyReg varData true chunk 0 
		in
		(Instr.Ins_unify_type_variable_t(regNum),
	     Instr.getSize_unify_type_variable_t)
	  in
	  (* subsequent occurrence of a permanent variable *)
      let genASubPermTypeVar varData offset =
		match (Absyn.getTypeVariableDataHeapVar varData), constEnv with
		  true,  true  ->
			(Instr.Ins_unify_envty_value_p(offset),
			 Instr.getSize_unify_envty_value_p)
		|	true,  false ->
			(Instr.Ins_unify_type_value_p(offset),
			 Instr.getSize_unify_type_value_p)
		| false, true  ->
			Absyn.setTypeVariableDataHeapVar varData true;
			Absyn.setTypeVariableDataSafety  varData true;
			(Instr.Ins_unify_envty_local_value_p(offset),
			 Instr.getSize_unify_envty_local_value_p)
		| false, false ->
			Absyn.setTypeVariableDataHeapVar varData true;
			Absyn.setTypeVariableDataSafety  varData true;		  			
			(Instr.Ins_unify_type_local_value_p(offset),
		     Instr.getSize_unify_type_local_value_p)
	  in
	  (* subsequent occurrence of a temporary variable *)
	  let genASubTempTypeVar varData offset lastUse =
		let (myInst, mySize) =
		  match (Absyn.getTypeVariableDataHeapVar varData), constEnv with
			true,  true  ->
			  (Instr.Ins_unify_envty_value_t(offset),
			   Instr.getSize_unify_envty_value_t)
		  |	true,  false ->
			  (Instr.Ins_unify_type_value_t(offset),
			   Instr.getSize_unify_type_value_t)
		  | false, true  ->
			  Absyn.setTypeVariableDataHeapVar varData true;
			  Absyn.setTypeVariableDataSafety  varData true;
			  (Instr.Ins_unify_envty_local_value_t(offset),
			   Instr.getSize_unify_envty_local_value_t)
		  | false, false ->
			  Absyn.setTypeVariableDataHeapVar varData true;
			  Absyn.setTypeVariableDataSafety  varData true;
			  (Instr.Ins_unify_type_local_value_t(offset),
			   Instr.getSize_unify_type_local_value_t)
		in
		if (lastUse) then (Registers.mkRegFree offset; (myInst, mySize))
		else (myInst, mySize)
	  in

	  (* function body of genATypeArgVariable *)
	  let  varData = Absyn.getTypeFreeVariableVariableData var in
	  let  lastUse = (Absyn.getTypeVariableDataLastUse varData) == var in
	  
	  match (Absyn.getTypeFreeVariableFirst var),
	        (Absyn.getTypeVariableDataPerm  varData)  
	  with
		true,  true   -> (* first occurrence of a permanent variable *)
		  (Instr.Ins_unify_type_variable_p
			 (Absyn.getTypeVariableDataOffset varData),
		   Instr.getSize_unify_type_variable_p)
	  |	true,  false  -> (* first occurrence of a temporary variable *)
		  genAFirstTempTypeVar varData lastUse
	  | false, true   -> (* subsequent occurrence of a permanent variable *)
		  genASubPermTypeVar varData (Absyn.getTypeVariableDataOffset varData)
	  | false, false  -> (* subsequent occurrence of a temporary variable *)
		  genASubTempTypeVar varData (Absyn.getTypeVariableDataOffset varData)
			lastUse
	in

	(* function body of genATypeArgsCodeAux *)
	match args with
	  [] ->
		let (myinsts, mysize) = genATypesCode (List.rev delayed) chunk in
		((List.rev insts) @ myinsts, mysize + size)
	| (tyExp :: rest) ->
		match tyExp with
		  Absyn.TypeVarType(_)            -> (* type variable *)
			let (myinst, mysize) =  genATypeArgVariable tyExp in
			genATypeArgsCodeAux rest delayed (myinst :: insts) (mysize + size)
		| Absyn.ApplicationType(kind, []) -> (* sort *)
			genATypeArgsCodeAux rest delayed 
			  (Instr.Ins_unify_type_constant(kind)::insts)
			  (Instr.getSize_unify_type_constant + size)
		| _ -> (* type structure or type arrow *)
			let regNum = Registers.getHighFreeReg () in
			genATypeArgsCodeAux rest ((regNum, tyExp)::delayed)
			  (Instr.Ins_unify_type_variable_t(regNum) :: insts)
			  (Instr.getSize_unify_type_variable_t + size)
  in
  genATypeArgsCodeAux args [] [] 0  

(****************************************************************************)
(*          GENERATING INSTRUCTIONS FOR TERMS                               *)
(****************************************************************************)
(* data structure for holding a register number or term expresion in    *)
(* synthesizing terms                                                   *)
type regorterm = 
	RegTmReg of int
  | RegTmTm  of Absyn.aterm

let getRegTermReg regOrTerm =
  match regOrTerm with
	RegTmReg (regNum) -> regNum
  | _ -> Errormsg.impossible Errormsg.none "getRegTermReg: invalid RegTermList"

(************************************************************************)
(*       GENERATING CODE FOR SYTHESIZING TERMS                          *)
(************************************************************************)

(************************************************************************)
(* genPuttingVarCode:                                                   *)
(* auxiliary function for putting a variable in sythesizing terms       *)
(************************************************************************)
let genPuttingVarCode var regNum chunk lowval last normalize hasenv =

  (* first occurrence of a temporary variable *)
  let genPuttingFirstTempVar varData regNum lastUse =
	let newRegNum =  
	  if (lastUse) then Registers.getHighFreeReg()
	  else Registers.assignTmReg varData chunk lowval
	in
	let (inst, size) = 
	  if (hasenv) then 
	    ([Instr.Ins_put_variable_te(newRegNum, regNum)],
	     Instr.getSize_put_variable_te)
	  else
	    ([Instr.Ins_put_variable_t(newRegNum, regNum)],
	     Instr.getSize_put_variable_t)
	in
	if (lastUse) then (Registers.markUnusedReg newRegNum; (inst, size))
	else (inst, size)
  in

  (* subsequent occurrence of a permanent variable *)
  let genPuttingSubPermVar varData offset regNum =
	let (inst, size) =
	  if (not(last) || (Absyn.getVariableDataSafety varData))
	  then (Instr.Ins_put_value_p(offset, regNum), Instr.getSize_put_value_p)
	  else (Absyn.setVariableDataSafety varData true;
			(Instr.Ins_put_unsafe_value(offset, regNum),
			 Instr.getSize_put_unsafe_value))
	in
	if (normalize) then 
	  ([inst ; Instr.Ins_head_normalize_t(regNum)],
	   size + Instr.getSize_head_normalize_t)
	else 
	([inst], size)
  in

  (* subsequent occurrence of a temporary variable *)
  let genPuttingSubTempVar offset regNum lastUse =
	let (inst, size) =
	  if (offset = regNum) then ([], 0)
	  else ([Instr.Ins_put_value_t(offset, regNum)], Instr.getSize_put_value_t)
	in
	(if (lastUse) then Registers.mkRegFree offset
	else ());
	if (normalize) then
	  (inst @ [Instr.Ins_head_normalize_t(regNum)],
	   size + Instr.getSize_head_normalize_t)
	else
	(inst, size)
  in

  (* function body of genPuttingVarCode *)
  let varData = Absyn.getTermFreeVariableVariableData var in
  let lastUse = (Absyn.getVariableDataLastUse varData) == var in
  match (Absyn.getTermFreeVariableFirst var), 
	    (Absyn.getVariableDataPerm varData)
  with
	true,  true  -> (* first occurrence of a  permanent variable *)
	  ([Instr.Ins_put_variable_p(Absyn.getVariableDataOffset varData, regNum)],
	   Instr.getSize_put_variable_p)
  | true,  false -> (* first occurrence of a temporary variable *)
	  genPuttingFirstTempVar varData regNum lastUse
  | false, true  -> (* subsequent occurrence of a permanent variable *)
	  genPuttingSubPermVar varData (Absyn.getVariableDataOffset varData) regNum
  | false, false -> (* subsequent occurrence of a temporary variable *)
	  genPuttingSubTempVar (Absyn.getVariableDataOffset varData) regNum lastUse


(************************************************************************)
(* genGlobalize:                                                        *)
(* Auxiliary function for putting flexible application head or variable *)
(* abstraction body.                                                    *)
(* In addition to instruction and its size, the number of the register  *)
(* used for holding this variable together with other flags indicating  *)
(* whether it should be discarded are returned.                         *)
(************************************************************************)
let genGlobalize var chunk lowval hasenv =

  (* first occurrence *)
  let genGlobalizeFirstOcc perm =
	let regNum = Registers.getHighFreeReg () in
	let (inst, size) = genPuttingVarCode var regNum chunk lowval false false hasenv in
	let (newInst, newSize) =
	  if (perm) then 
		(inst@[Instr.Ins_globalize_t(regNum)],size+Instr.getSize_globalize_t)
	  else (inst, size)
	in
	(newInst, newSize, regNum, false, true, false) 
  in

  (* subsequent occurrence of a permanent variable *)
  let genGlobalizeSubPerm varData =
	let offset = Absyn.getVariableDataOffset varData in
	let regNum = Registers.getHighFreeReg () in
	let (inst, size) =
	  if (Absyn.getVariableDataHeapVar varData) then
		([Instr.Ins_copy_value(offset, regNum)], Instr.getSize_copy_value)
	  else
		(Absyn.setVariableDataHeapVar varData true;
		 Absyn.setVariableDataSafety  varData true;
		 ([Instr.Ins_globalize_pt(offset, regNum)],Instr.getSize_globalize_pt))
	in
	(inst, size, regNum, true, true, false)
  in

  (* subsequent occurrence of a temporary variable *)
  let genGlobalizeSubTemp varData lastUse =
	let offset = Absyn.getVariableDataOffset varData in
	let (inst, size) =
	  if (Absyn.getVariableDataHeapVar varData) then ([], 0)
	  else 
		(Absyn.setVariableDataHeapVar varData true;
		 Absyn.setVariableDataSafety varData true;
		 ([Instr.Ins_globalize_t(offset)], Instr.getSize_globalize_t))
	in
	(inst, size, offset, true, false, lastUse)
  in

  (* function body of genGlobalize *)
  let varData = Absyn.getTermFreeVariableVariableData var in
  match (Absyn.getTermFreeVariableFirst var),
	    (Absyn.getVariableDataPerm varData)
  with
	true,  perm    -> (* first occurrence *)
	  genGlobalizeFirstOcc perm 
  | false, true    -> (* subsequent occurrence of a permanent variable *)    
	  genGlobalizeSubPerm varData
  | false, false   -> (* subsequent occurrence of a temporary variable *)
	  genGlobalizeSubTemp varData (Absyn.getVariableDataLastUse varData == var)

(************************************************************************)
(* genSTermsCode:                                                       *)
(* generate term synthesis code from a list of terms paired with the    *)
(* registers in which the created structure is to be put.               *)
(************************************************************************)
let rec genSTermsCode regTermPairs chunk lowval last hasenv =
  let rec genSTermsCodeAux regTermPairs instsBlocks size =
	match regTermPairs with
	  [] ->  (List.flatten (List.rev instsBlocks), size)
	| ((regNum, term) :: rest) ->
		let (oneInstsBlock, oneSize, _) =
		  genSTermCode regNum term chunk lowval last hasenv true
		in
		genSTermsCodeAux rest (oneInstsBlock :: instsBlocks) (size + oneSize)
  in
  genSTermsCodeAux regTermPairs [] 0 
  
(****************************************************************************)
(* genSTermCode and genSTermCodesansReg:                                    *)
(* The main routines for generating code for putting terms into registers.  *)
(* The first is used to set up the arguments for an atomic goal and the     *)
(* second for constructing the structured parts of a structured term.       *)
(* Two versions are needed because in the first case the registers in which *)
(* the terms have to be put are predetermined (these are the argument       *)
(* registers) whereas in the second case we hold off on assigning registers *)
(* till after the subterms have been constructed so that we may reuse the   *)
(* registers employed in the course of doing this. This is important since  *)
(* big terms require more than the available number of registers otherwise. *)
(* The difference in the two versions shows up in the fact that the first   *)
(* takes a register as input whereas the second returns the register it     *)
(* assigns to hold the term.                                                *)
(*                                                                          *)
(* The hasenv parameter is needed to distinguish between set_variable_t and *)
(* set_variable_te: one that looks at the environment for the UC value and  *)
(* the other that looks at the UC register.                                 *)
(* An instruction is emitted for head normalizing terms constructed as      *)
(* arguments of atomic goals in special cases. These cases correspond to    *)
(* applications with a  variable head where the variable is a non first     *)
(* occurrence one (i.e. may be bound to a term at runtime) or to            *)
(* abstractions with such a head. The argument normalize signals whether    *)
(* or not such a normalization instruction is to be emitted. This routine   *)
(* returns a truth value that indicates whether or not the head is a        *)
(* variable; this is needed because the routine can be called recursively   *)
(* (e.g. see genSTermCodeApp) and the decision of whether or not to         *)
(* normalize can only be made at the call site.                             *)
(****************************************************************************)
and genSTermCode regNum term chunk lowval last hasenv normalize =
  
  (* constant without type associations *)
  let genSTermCodeMConst c regNum =
	if (Pervasive.isnilConstant c) then
	  ([Instr.Ins_put_nil(regNum)], Instr.getSize_put_nil, false)
	else
	  ([Instr.Ins_put_m_const(regNum, c)], Instr.getSize_put_m_const, false)
  in

  (* constant with type associations *)
  let genSTermCodePConst c tyenv regNum =
	let (typeCode, typeCodeSize, regTypePairs) =
	  genSTypeArgsCode tyenv chunk lowval 
	in
	let (inst, size) = 
	  (Instr.Ins_put_p_const(regNum, c), Instr.getSize_put_p_const)
	in
	let (typeSettingCode, typeSettingCodeSize) =
	  genTypeSettingCode regTypePairs chunk lowval 
	in
	(typeCode @ (inst :: typeSettingCode),
	 typeCodeSize + size + typeSettingCodeSize, false)
  in

  (* list cons (special case of application term) *)
  let genSTermCodeCons args regNum =
	let (argsCode, argsCodeSize, regTermList) = 
	  genSTermArgsCode args chunk lowval hasenv 
	in
	let (inst, size) = (Instr.Ins_put_list(regNum), Instr.getSize_put_list) in
	let (argsSettingCode, argsSettingCodeSize) =
	  genTermSettingCode regTermList chunk lowval hasenv
	in
	(argsCode @ (inst :: argsSettingCode),
	 argsCodeSize + argsSettingCodeSize + size, false)
  in

  (* auxiliary function used in genSTermCodeApp and genSTermCodeAbst  *)
  (* for putting rigid application head or variable abstraction body. *)       
  let genRigid rterm  =
	let regNum = Registers.getHighFreeReg () in
	let (insts, size, _) =
	  genSTermCode regNum rterm chunk lowval false hasenv false
	in
	(insts, size, regNum, false, true, false)
  in

  (* auxiliary function used in genSTermCodeApp and genSTermCodeAbst  *)
  (* for free registers.                                              *)
  let discardRegister regNum discard freeReg =
	(if discard then Registers.markUnusedReg regNum else ());
	if freeReg then Registers.mkRegFree regNum else ()
  in
  
  (* The innards of creating an application. The main complication in this *)
  (* part is dealing with the situation where the head is a free variable; *)
  (* this value may need to be globalized.                                 *)
  let genSTermCodeApp app regNum =
	let func = Absyn.getTermApplicationHead  term in
	let args = Absyn.getTermApplicationArguments  term in
	if (Absyn.isTermConstant func) && 
	   (Pervasive.isconsConstant (Absyn.getTermConstant func))
	then genSTermCodeCons args regNum
	else                                       (* head code *)
	  let (headCode, headSize, headReg, varHead, discardReg, freeReg) = 
		if (Absyn.isTermFreeVariable func) then genGlobalize func chunk lowval hasenv
		else genRigid func 
	  in
	  let (argsCode, argsSize, regTermList) = (* args code *)
		genSTermArgsCode args chunk lowval hasenv 
	  in
	  let (inst, size) =                       (* put_app code *)
		(Instr.Ins_put_app(regNum,headReg, Absyn.getTermApplicationArity term),
		 Instr.getSize_put_app)
	  in
	  discardRegister headReg discardReg freeReg;
	  let (argsSettingCode, argsSettingSize) = (* args setting code *)
		genTermSettingCode regTermList chunk lowval hasenv 
	  in
	  if (normalize && varHead) then
		(headCode @ argsCode @ (inst :: argsSettingCode) @ 
		 [Instr.Ins_head_normalize_t(regNum)], 
		 headSize+argsSize+size+argsSettingSize+Instr.getSize_head_normalize_t,
		 varHead)
	  else
		(headCode @ argsCode @ (inst :: argsSettingCode),
		 headSize + argsSize + size + argsSettingSize, varHead)
  in

  (*The innards of creating an abstraction term. The main complication in  *)
  (*this part is that of dealing with the case when the body is a variable.*)
  let genSTermCodeAbst abst regNum =
	let body   = Absyn.getTermAbstractionBody term in
	let numAbs = Absyn.getTermAbstractionNumberOfLambda term in
	let (bodyCode, bodySize, bodyReg, varHead, discardReg, freeReg) =
	  if (Absyn.isTermFreeVariable body) then genGlobalize body chunk lowval hasenv
	  else genRigid body
	in
	let (inst, size) = 
	  if (normalize && varHead) then
		([Instr.Ins_put_lambda(regNum, bodyReg, numAbs);
		  Instr.Ins_head_normalize_t(regNum)],
		 Instr.getSize_put_lambda + Instr.getSize_head_normalize_t)
	  else 
		([Instr.Ins_put_lambda(regNum, bodyReg, numAbs)], 
		 Instr.getSize_put_lambda)
	in
	discardRegister bodyReg discardReg freeReg;
	(bodyCode @ inst, bodySize + size, varHead)
  in

  (* function body of genSTermCode *)
  match term with 
	Absyn.IntTerm(i, _)             ->
	  ([Instr.Ins_put_integer (regNum, i)], Instr.getSize_put_integer, false)
  | Absyn.RealTerm(r, _)            ->
	  ([Instr.Ins_put_float(regNum, r)], Instr.getSize_put_float, false)
  | Absyn.StringTerm(s, _)          ->
	  ([Instr.Ins_put_string(regNum, Absyn.getStringInfoIndex s)],
	   Instr.getSize_put_string, false)
  | Absyn.BoundVarTerm(_)              ->
	  ([Instr.Ins_put_index(regNum, Absyn.getTermBoundVariableDBIndex term)],
	   Instr.getSize_put_index,  false)
  | Absyn.ConstantTerm(c, [], _)    -> 
	  genSTermCodeMConst c regNum 
  | Absyn.ConstantTerm(c, tyenv, _) ->
	  genSTermCodePConst c tyenv regNum
  | Absyn.FreeVarTerm(_)               ->
	  let (inst, size) =
		genPuttingVarCode term regNum chunk lowval last normalize hasenv
	  in
	  (inst, size, false) 
  | Absyn.ApplicationTerm(_)           ->  genSTermCodeApp term regNum 
  | _ -> genSTermCodeAbst term regNum (* must be abstraction then*) 


(****************************************************************************)
(* Here begins the second version, genSTermCodesansReg                      *)
(****************************************************************************)
and genSTermCodesansReg term chunk lowval last hasenv normalize =
  
  (* constant without type associations *)
  let genSTermCodeMConstsansReg c =
	let regNum = Registers.getHighFreeReg () in
	if (Pervasive.isnilConstant c) then
	  ([Instr.Ins_put_nil(regNum)], Instr.getSize_put_nil, regNum, false)
	else
	  ([Instr.Ins_put_m_const(regNum, c)], Instr.getSize_put_m_const, regNum, false)
  in

  (* constant with type associations *)
  let genSTermCodePConstsansReg c tyenv =
	let (typeCode, typeCodeSize, regTypePairs) =
	  genSTypeArgsCode tyenv chunk lowval 
	in
        let regNum = Registers.getHighFreeReg () in
	let (inst, size) = 
	  (Instr.Ins_put_p_const(regNum, c), Instr.getSize_put_p_const)
	in
	let (typeSettingCode, typeSettingCodeSize) =
	  genTypeSettingCode regTypePairs chunk lowval 
	in
	(typeCode @ (inst :: typeSettingCode),
	 typeCodeSize + size + typeSettingCodeSize, regNum, false)
  in

  (* list cons (special case of application term) *)
  let genSTermCodeConssansReg args =
	let (argsCode, argsCodeSize, regTermList) = 
	  genSTermArgsCode args chunk lowval hasenv 
	in
        let regNum = Registers.getHighFreeReg () in
	let (inst, size) = (Instr.Ins_put_list(regNum), Instr.getSize_put_list) in
	let (argsSettingCode, argsSettingCodeSize) =
	  genTermSettingCode regTermList chunk lowval hasenv
	in
	(argsCode @ (inst :: argsSettingCode),
	 argsCodeSize + argsSettingCodeSize + size, regNum, false)
  in

  (* auxiliary function used in genSTermCodeApp and genSTermCodeAbst  *)
  (* for putting rigid application head or variable abstraction body. *)       
  let genRigidsansReg rterm  =
	let (insts, size, regNum, _) =
	  genSTermCodesansReg rterm chunk lowval false hasenv false
	in
	(insts, size, regNum, false, true, false)
  in

  (* auxiliary function used in genSTermCodeApp and genSTermCodeAbst  *)
  (* for free registers.                                              *)
  let discardRegister regNum discard freeReg =
	(if discard then Registers.markUnusedReg regNum else ());
	if freeReg then Registers.mkRegFree regNum else ()
  in
  
  (* The innards of creating an application. The main complication in this *)
  (* part is dealing with the situation where the head is a free variable; *)
  (* this value may need to be globalized.                                 *)
  let genSTermCodeAppsansReg app =
	let func = Absyn.getTermApplicationHead  term in
	let args = Absyn.getTermApplicationArguments  term in
	if (Absyn.isTermConstant func) && 
	   (Pervasive.isconsConstant (Absyn.getTermConstant func))
	then genSTermCodeConssansReg args
	else                                       (* head code *)
	  let (headCode, headSize, headReg, varHead, discardReg, freeReg) = 
		if (Absyn.isTermFreeVariable func) then genGlobalize func chunk lowval hasenv
		else genRigidsansReg func 
	  in
	  let (argsCode, argsSize, regTermList) = (* args code *)
		genSTermArgsCode args chunk lowval hasenv 
	  in
	  let regNum = Registers.getHighFreeReg () in
          let (inst, size) =                       (* put_app code *)
		(Instr.Ins_put_app(regNum,headReg, Absyn.getTermApplicationArity term),
		 Instr.getSize_put_app)
	  in
	  discardRegister headReg discardReg freeReg;
	  let (argsSettingCode, argsSettingSize) = (* args setting code *)
		genTermSettingCode regTermList chunk lowval hasenv 
	  in
	  if (normalize && varHead) then
		(headCode @ argsCode @ (inst :: argsSettingCode) @ 
		 [Instr.Ins_head_normalize_t(regNum)], 
		 headSize+argsSize+size+argsSettingSize+Instr.getSize_head_normalize_t,
                 regNum,
		 varHead)
	  else
		(headCode @ argsCode @ (inst :: argsSettingCode),
		 headSize + argsSize + size + argsSettingSize, regNum, varHead)
  in

  (*The innards of creating an abstraction term. The main complication in  *)
  (*this part is that of dealing with the case when the body is a variable.*)
  let genSTermCodeAbstsansReg abst =
	let body   = Absyn.getTermAbstractionBody term in
	let numAbs = Absyn.getTermAbstractionNumberOfLambda term in
	let (bodyCode, bodySize, bodyReg, varHead, discardReg, freeReg) =
	  if (Absyn.isTermFreeVariable body) then genGlobalize body chunk lowval hasenv
	  else genRigidsansReg body
	in
        let regNum = Registers.getHighFreeReg () in
	let (inst, size) = 
	  if (normalize && varHead) then
		([Instr.Ins_put_lambda(regNum, bodyReg, numAbs);
		  Instr.Ins_head_normalize_t(regNum)],
		 Instr.getSize_put_lambda + Instr.getSize_head_normalize_t)
	  else 
		([Instr.Ins_put_lambda(regNum, bodyReg, numAbs)], 
		 Instr.getSize_put_lambda)
	in
	discardRegister bodyReg discardReg freeReg;
	(bodyCode @ inst, bodySize + size, regNum, varHead)
  in

  (* function body of genSTermCodesansReg *)
  match term with 
	Absyn.IntTerm(i, _)             ->
	  let regNum = Registers.getHighFreeReg () in
          ([Instr.Ins_put_integer (regNum, i)], Instr.getSize_put_integer, regNum, false)
  | Absyn.RealTerm(r, _)            ->
	  let regNum = Registers.getHighFreeReg () in
          ([Instr.Ins_put_float(regNum, r)], Instr.getSize_put_float, regNum, false)
  | Absyn.StringTerm(s, _)          ->
	  let regNum = Registers.getHighFreeReg () in
	  ([Instr.Ins_put_string(regNum, Absyn.getStringInfoIndex s)],
	   Instr.getSize_put_string, regNum, false)
  | Absyn.BoundVarTerm(_)              ->
	  let regNum = Registers.getHighFreeReg () in
	  ([Instr.Ins_put_index(regNum, Absyn.getTermBoundVariableDBIndex term)],
	   Instr.getSize_put_index, regNum, false)
  | Absyn.ConstantTerm(c, [], _)    -> 
	  genSTermCodeMConstsansReg c
  | Absyn.ConstantTerm(c, tyenv, _) ->
	  genSTermCodePConstsansReg c tyenv
  | Absyn.FreeVarTerm(_)               ->
	  let regNum = Registers.getHighFreeReg () in
	  let (inst, size) =
		genPuttingVarCode term regNum chunk lowval last normalize hasenv
	  in
	  (inst, size, regNum, false) 
  | Absyn.ApplicationTerm(_)           ->  genSTermCodeAppsansReg term
  | _ -> genSTermCodeAbstsansReg term (* must be abstraction then*) 


(****************************************************************************)
(* genSTermArgsCode:                                                        *)
(* Generating code for creating the arguments of a structure term. For      *)
(* atoms, nothing needs to be done. For structures, these need to be created*) 
(* and put into registers to be used by the setting process. Note that this *) 
(* routine is also invoked to create head terms that are higher-order       *)
(****************************************************************************)
and genSTermArgsCode args chunk lowval hasenv  =
  (* for complex term *)
  let rec genSTermArgComplex term rest instsBlocks instsSize regTermList =
	let (inst, size, regNum, _) =
	  genSTermCodesansReg term chunk lowval false hasenv false
	in
	genSTermArgsCodeAux rest (inst :: instsBlocks) (size + instsSize) 
	                    ((RegTmReg regNum) :: regTermList)
  (* recurse over arguments *)
  and genSTermArgsCodeAux args instsBlocks instsSize regTermList =
	match args with
	  [] -> (List.flatten (List.rev instsBlocks), instsSize, 
			 List.rev regTermList)
	| (term :: rest) ->
		match term with
		  Absyn.ApplicationTerm(_) -> 
			genSTermArgComplex term rest instsBlocks instsSize regTermList
		| Absyn.AbstractionTerm(_) -> 
			genSTermArgComplex term rest instsBlocks instsSize regTermList
		| _ -> (* int, real, string, constant, freevar, boundvar *)
			genSTermArgsCodeAux rest instsBlocks instsSize 
			  ((RegTmTm term) :: regTermList)			
  in
  genSTermArgsCodeAux args [] 0 []

(***********************************************************************)
(* genTermSettingCode:                                                 *)
(* producing code for setting arguments of a structure                 *)
(***********************************************************************)
and genTermSettingCode regTermList chunk lowval hasenv = 
  
  (* generate term setting code for one argument *)
  let genTermSettingCodeTerm term =
	
	(* constant without type associations *)
	let genSettingMConstCode c =
	  if (Pervasive.isnilConstant c) then
		([Instr.Ins_set_nil], Instr.getSize_set_nil)
	  else 
		([Instr.Ins_set_m_const(c)], Instr.getSize_set_m_const)
	in
	
	(* constant with type association *)
	let genSettingPConstCode c tyenv =
	  let (tyCode,tySize,regTypeList) = genSTypeArgsCode tyenv chunk lowval in
	  let (inst, size) = 
		(Instr.Ins_set_p_const(c), Instr.getSize_set_p_const)
	  in
	  let (tySettingCode, tySettingSize) =
		genTypeSettingCode regTypeList chunk lowval
	  in
	  (tyCode @ (inst :: tySettingCode), tySize + size + tySettingSize)
	in

	(* variables *)
	let genSettingVarCode var =

	  (* first occurrence of a temporary variable *)
	  let genSettingFirstTempVarCode varData lastUse =
	    let regNum = 
	      if lastUse then Registers.getHighFreeReg ()
	      else Registers.assignTmReg varData chunk lowval
	    in
	    let (inst, size) =
	      if hasenv then 
			(Instr.Ins_set_variable_te(regNum), Instr.getSize_set_variable_te)
	      else (Instr.Ins_set_variable_t(regNum), Instr.getSize_set_variable_t)
	    in
	    if lastUse then (Registers.markUnusedReg regNum; ([inst], size))
	    else ([inst], size)
	  in

	  (* subsequent occurrence of a permanent variable *)
	  let genSettingSubPermVarCode varData offset =
	    if (Absyn.getVariableDataHeapVar varData)
	    then ([Instr.Ins_set_value_p(offset)], Instr.getSize_set_value_p)
	    else
		  (Absyn.setVariableDataHeapVar varData true;
		   Absyn.setVariableDataSafety varData true;
		   let regNum = Registers.getHighFreeReg () in
		   Registers.markUnusedReg regNum;
		   ([Instr.Ins_globalize_pt(offset,regNum);
			 Instr.Ins_set_value_t(regNum)],
			Instr.getSize_globalize_pt + Instr.getSize_set_value_t))
	  in
	  
	  (* subsequent occurrence of a temporary variable *)
	  let genSettingSubTempVarCode varData offset lastUse =
	    let (inst, size) =
	      if (Absyn.getVariableDataHeapVar varData)
	      then ([Instr.Ins_set_value_t(offset)], Instr.getSize_set_value_t)
	      else
			(Absyn.setVariableDataHeapVar varData true;
			 Absyn.setVariableDataSafety varData true;
			 ([Instr.Ins_globalize_t(offset); Instr.Ins_set_value_t(offset)],
			  Instr.getSize_globalize_t + Instr.getSize_set_value_t))
	    in
	    if lastUse then (Registers.mkRegFree offset; (inst, size))
	    else (inst, size)
	  in

	  (* function body of genSettingVarCode *)
	  let varData = Absyn.getTermFreeVariableVariableData var in
	  let lastUse = (Absyn.getVariableDataLastUse varData) == var in
	  match (Absyn.getTermFreeVariableFirst var),
		    (Absyn.getVariableDataPerm  varData)
	  with
		true  , true  -> (* first occurrence of a permanent variable *)
		  ([Instr.Ins_set_variable_p(Absyn.getVariableDataOffset varData)], 
		   Instr.getSize_set_variable_p)
	  | true  , false -> (* first occurrence of a temporary variable *)
	      genSettingFirstTempVarCode varData lastUse
	  | false , true  -> (* subsequent occurrence of a permanent variable *)
	      genSettingSubPermVarCode varData 
			(Absyn.getVariableDataOffset varData)
	  | false , false -> (* subsequent occurrence of a temporary variable *)
	      genSettingSubTempVarCode varData 
			(Absyn.getVariableDataOffset varData)
			(Absyn.getVariableDataLastUse varData == var)
	in

	(* function body of genTermSettingCodeTerm *)
	match term with
	  Absyn.IntTerm(i, _)          ->
		([Instr.Ins_set_integer(i)], Instr.getSize_set_integer)
	| Absyn.RealTerm(r, _)         ->
		([Instr.Ins_set_float(r)], Instr.getSize_set_float)
	| Absyn.StringTerm(s, _)       ->
		([Instr.Ins_set_string(Absyn.getStringInfoIndex s)],
		 Instr.getSize_set_string)
	| Absyn.BoundVarTerm(_, _)     ->
		([Instr.Ins_set_index(Absyn.getTermBoundVariableDBIndex term)],
		 Instr.getSize_set_index)
	| Absyn.ConstantTerm(c, [], _) ->  
		genSettingMConstCode c 
	| Absyn.ConstantTerm(c, tys, _)->  
		genSettingPConstCode c tys
	| _  -> (* must be free variable then *) 
		genSettingVarCode term 
  in		

  let rec genTermSettingCodeAux regTermList insts size =
	match regTermList with
	  [] -> (List.flatten (List.rev insts), size)
	| ((RegTmReg regNum) :: rest) ->
		Registers.markUnusedReg regNum;
		genTermSettingCodeAux rest ([Instr.Ins_set_value_t(regNum)]::insts)
		  (Instr.getSize_set_value_t + size)
	| ((RegTmTm term) :: rest) ->
		let (inst, oneSize) = genTermSettingCodeTerm term in
		genTermSettingCodeAux rest (inst :: insts) (size + oneSize)
  in
  genTermSettingCodeAux regTermList [] 0

(************************************************************************)
(*       GENERATING CODE FOR ANALYZING TERMS                            *)
(************************************************************************)
(****************************************************************************)
(* genATermsCode:                                                           *)
(* Generating analysis code for a list of argument terms paired with        *)
(* registers; these could be top-level head arguments or embedded structures*)
(****************************************************************************)
let rec genATermsCode delayed chunk insts startLoc =

  (* free variable *)
  let genATermCodeFreeVar var regNum =
	let varData = Absyn.getTermFreeVariableVariableData var in
	let offset  = Absyn.getVariableDataOffset varData   in
	if (Absyn.getVariableDataPerm varData) then
	  (Registers.markUnusedReg regNum;
	   (insts @ [Instr.Ins_pattern_unify_p(offset, regNum)],
		startLoc + Instr.getSize_pattern_unify_p))
	else
	  let (newInsts, newStartLoc) = 
		(insts @ [Instr.Ins_pattern_unify_t(offset, regNum)],
		 startLoc + Instr.getSize_pattern_unify_t)
	  in
	  Registers.markUnusedReg regNum;
	  if (Absyn.getVariableDataLastUse varData == var) then
		(Registers.mkRegFree offset; (newInsts, newStartLoc))
	  else (newInsts, newStartLoc)
  in
  
  (* constant without type association *)
  let genATermCodeMConst c regNum =
	let (newInsts, newStartLoc) =
	  if (Pervasive.isnilConstant c) then 
		(insts @ [Instr.Ins_get_nil(regNum)], startLoc + Instr.getSize_get_nil)
	  else 
		(insts @ [Instr.Ins_get_m_constant(regNum, c)],
		 startLoc + Instr.getSize_get_m_constant)
	in
	Registers.markUnusedReg regNum;
	(newInsts, newStartLoc)
  in

  (* constant with type association *)
  let genATermCodePConst c tyEnv regNum =
	let typeCodeNextRef = ref 0 in
	let instr = Instr.Ins_get_p_constant(regNum, c, typeCodeNextRef) in
	let (typesCode, typesSize) = genATypeArgsCode tyEnv chunk true in
	let typesCodeNextLoc = startLoc+typesSize+Instr.getSize_get_p_constant  in
	Registers.markUnusedReg regNum;
	typeCodeNextRef := typesCodeNextLoc;
	(insts @ (instr :: typesCode), typesCodeNextLoc)
  in

  (* structure (first order application) *)
  let genATermCodeStructure func args arity regNum =
	  let funcConst = Absyn.getTermConstant func in
	  let (funcCode, funcCodeNext) =
	    if (Pervasive.isconsConstant funcConst) then 
		  (insts@[Instr.Ins_get_list(regNum)], startLoc + Instr.getSize_get_list)
	    else
		  let tyenv    = Absyn.getTermConstantTypeEnv func in
		  if (tyenv = []) then 
		    (insts @ [Instr.Ins_get_m_structure(regNum, funcConst, arity)],
		     startLoc + Instr.getSize_get_m_structure)
		  else
		    let (tyenvCode, tyenvSize) = genATypeArgsCode tyenv chunk true in
		    (insts @ 
		     (Instr.Ins_get_p_structure(regNum, funcConst, arity) :: tyenvCode),
		     startLoc + Instr.getSize_get_p_structure + tyenvSize)
	  in
	  Registers.markUnusedReg regNum;
	  genAStrTermArgsCode args chunk funcCode funcCodeNext
  in

  (* higher-order structure: flexible application or abstraction *)
  let genATermCodeHigherOrder term regNum =
	  let (termCode, termCodeSize, regTermList) =
	    genSTermArgsCode [term] chunk 0 false 
	  in
	  let newRegNum = getRegTermReg (List.hd regTermList) in
	  Registers.markUnusedReg(newRegNum); 
	  Registers.markUnusedReg(regNum);
	  (insts @ termCode @ [Instr.Ins_pattern_unify_t(newRegNum, regNum)],
       startLoc + termCodeSize + Instr.getSize_pattern_unify_t)
    in  

  (* for each pair in the delayed list *)
  let genATermCode regNum term =
	match term with
	  Absyn.IntTerm(i, _)           -> 
		Registers.markUnusedReg regNum;
		(insts @ [Instr.Ins_get_integer(regNum, i)],
		 startLoc + Instr.getSize_get_integer)		
	| Absyn.RealTerm(r, _)          -> 
		Registers.markUnusedReg regNum;
		(insts @ [Instr.Ins_get_float(regNum, r)],
		 startLoc + Instr.getSize_get_float)
	| Absyn.StringTerm(s, _)        -> 
		Registers.markUnusedReg regNum;
		(insts @ [Instr.Ins_get_string(regNum, (Absyn.getStringInfoIndex s))],
		 startLoc + Instr.getSize_get_string)
	| Absyn.FreeVarTerm(_, _)       -> genATermCodeFreeVar term regNum 
	| Absyn.ConstantTerm(c, [], _)  -> genATermCodeMConst c regNum
	| Absyn.ConstantTerm(c, tys, _) -> 
		genATermCodePConst c tys regNum
	| Absyn.ApplicationTerm(_, pos)   ->
		let func = Absyn.getTermApplicationHead term in
		if (Absyn.isTermFreeVariable func) then
		  genATermCodeHigherOrder term regNum 
		else if Absyn.isTermConstant func then
		  genATermCodeStructure func (Absyn.getTermApplicationArguments term) 
  			(Absyn.getTermApplicationArity term) regNum
    else
      Errormsg.impossible pos "Clausegen.genATermCode: invalid application head."
	| _ -> genATermCodeHigherOrder term regNum (* must be an abstraction then*)
  in

  (* function body of genATermsCode *)
  match delayed with
	[] -> (insts, startLoc)
  | ((regNum, term)::rest) ->
	  let (newInsts, newStartLoc) = genATermCode regNum term in
      genATermsCode rest chunk newInsts newStartLoc

(*****************************************************************************)
(* genAStrTermArgsCode:                                                      *)
(* generating analysis code for arguments of a structured term               *)
(*****************************************************************************)
and genAStrTermArgsCode args chunk insts startLoc =

  let rec genAStrTermArgsCodeAux args insts startLoc delayed =

	(* constant argument without type associations *)
	let genAStrTermArgMConst c =
	  let (inst, size) =
		if (Pervasive.isnilConstant c)
		then (Instr.Ins_unify_nil, Instr.getSize_unify_nil)
		else (Instr.Ins_unify_m_constant(c), Instr.getSize_unify_m_constant)
	  in
	  (insts @ [inst], startLoc + size, delayed)
	in
	(* constant argument with type associations *)
	let genAStrTermArgPConst c tyenv =
	  let typeCodeNextRef = ref 0 in
	  let inst = Instr.Ins_unify_p_constant(c, typeCodeNextRef) in
	  let (typesCode, typesSize) = genATypeArgsCode tyenv chunk true in
	  let typeCodeNextLoc = 
		startLoc + typesSize +  Instr.getSize_unify_p_constant
	  in
	  typeCodeNextRef := typeCodeNextLoc;
	  (insts @ (inst :: typesCode), typeCodeNextLoc, delayed)
	in
	
	(* variable argument *)
	let genAStrTermArgVar var =

	  (* first occurrence of a temporary variabkle *)
	  let genAFirstTempVar varData lastUse =
		let regNum = 
		  if lastUse then
			let myRegNum = Registers.getHighFreeReg () in
			Registers.markUnusedReg myRegNum;
			myRegNum
		  else Registers.assignTmReg varData chunk 0
		in
		(Instr.Ins_unify_variable_t(regNum), Instr.getSize_unify_variable_t)
	  in

	  (* subsequent occurrence of a permanent variable *)
	  let genASubPermVar varData offset =
		if (Absyn.getVariableDataHeapVar varData) then
		  (Instr.Ins_unify_value_p(offset), Instr.getSize_unify_value_p)
		else
		  (Absyn.setVariableDataHeapVar varData true;
		   Absyn.setVariableDataSafety varData true;
		   (Instr.Ins_unify_local_value_p(offset),
			Instr.getSize_unify_local_value_p))
	  in

	  (* subsequent occurrence of a temporary variable *)
	  let genASubTempVar varData offset lastUse =
		let (inst, size) =
		  if (Absyn.getVariableDataHeapVar varData) then
			(Instr.Ins_unify_value_t(offset), Instr.getSize_unify_value_t)
		  else
			(Absyn.setVariableDataHeapVar varData true;
			 Absyn.setVariableDataSafety varData true;
			 (Instr.Ins_unify_local_value_t(offset),
			  Instr.getSize_unify_local_value_t))
		in
		if (lastUse) then (Registers.mkRegFree offset; (inst, size))
		else (inst, size)
	  in
	  
	  (* function body of genAStrTermArgVar *)
	  let varData = Absyn.getTermFreeVariableVariableData var in
	  let lastUse = (Absyn.getVariableDataLastUse varData) == var in
	  let (inst, size) =
		match (Absyn.getTermFreeVariableFirst var),
		      (Absyn.getVariableDataPerm varData)
		with
		  true , true  -> (* first occurrence of a permanent variable *)
			(Instr.Ins_unify_variable_p(Absyn.getVariableDataOffset varData),
			 Instr.getSize_unify_variable_p)
		| true , false -> (* first occurrence of a temporary variable *)
			genAFirstTempVar varData lastUse
		| false, true  -> (* subsequent occurrence of a permanent variable *)
			genASubPermVar varData (Absyn.getVariableDataOffset varData)
		| false, false -> (* subsequent occurrence of a temporary variable *)
			genASubTempVar varData (Absyn.getVariableDataOffset varData)
			  lastUse
	  in
	  (insts @ [inst], startLoc + size, delayed)
	in

	(* function body of genAStrTermArgsCodeAux *)
	match args with
	  [] -> genATermsCode (List.rev delayed) chunk insts startLoc
	| (term :: rest) ->
		let (newInsts, newStartLoc, newDelayed) =
		  match term with
			Absyn.IntTerm(i, _)           -> 
			  (insts @ [Instr.Ins_unify_integer(i)],
			   startLoc + Instr.getSize_unify_integer, delayed)
		  | Absyn.RealTerm(r, _)          -> 
			  (insts @ [Instr.Ins_unify_float(r)],
			   startLoc + Instr.getSize_unify_float, delayed)
		  | Absyn.StringTerm(s, _)        -> 
			  (insts @ [Instr.Ins_unify_string(Absyn.getStringInfoIndex s)],
			   startLoc + Instr.getSize_unify_string, delayed) 
		  | Absyn.ConstantTerm(c, [], _)  ->  genAStrTermArgMConst c
		  | Absyn.ConstantTerm(c, tys, _) ->  genAStrTermArgPConst c tys
		  | Absyn.FreeVarTerm(_, _)       ->  genAStrTermArgVar term 
		  | _ -> 
			  let regNum = Registers.getHighFreeReg () in
			  (insts @ [Instr.Ins_unify_variable_t(regNum)],
			   startLoc + Instr.getSize_unify_variable_t,
			   ((regNum, term) :: delayed))
		in
		genAStrTermArgsCodeAux rest newInsts newStartLoc newDelayed
  in
  genAStrTermArgsCodeAux args insts startLoc []

(****************************************************************************)
(*          GENERATING INSTRUCTIONS FOR CLAUSE HEAD                         *)
(****************************************************************************)
(*************************************************************************)
(* genHeadTyVarsCode:                                                    *)
(* Producing code for type variables that appear at the top level in the *)
(* type environment of the clause head. In the situation that the        *)
(* corresponding position in the neededness vector is false, register is *)
(* allocated but no code is generated. Otherwise, first occurrences      *)
(* simply yield a register allocation. Subsequent occurrences translate  *)
(* to unification and a freeing of the argument register. Translation of *)
(* all other types is delayed until after this initial register          *)
(* assignment.                                                           *)
(*************************************************************************)
let genHeadTyVarsCode tyargs regNum neededness =
  let rec genHeadTyVarsCodeAux tyargs regNum insts size delayed index =	
    (* for one type variable *)
    let genHeadTyVarCode var =
      
      (* first occurrence of a permanent variable as a head argument *)
      let genHeadFirstPermTypeVar varData offset needed =
	let (inst, size) =
	  if (needed) then 
	    ([Instr.Ins_get_type_variable_p(offset, regNum)],
	     Instr.getSize_get_type_variable_p)
	  else ([], 0)
	in
	Registers.markUnusedReg regNum;
	(inst, size)
      in
      
      (* first occurrence of a temporary variable as a head argument *)
      let genHeadFirstTempTypeVar varData lastUse needed  =
	(if lastUse then Registers.markUnusedReg regNum
	else Registers.assignRegToType varData needed regNum);
	([], 0)
      in
      
      (* subsequent occurrence of a permanent variable as a head argument *)
      let genHeadSubPermTypeVar varData offset needed=
	let (inst, size) =
	  if (needed) then 
	    ([Instr.Ins_get_type_value_p(offset, regNum)],
	     Instr.getSize_get_type_value_p)
	  else ([], 0)
	in
	Registers.markUnusedReg regNum;
	(inst, size)
      in
      
      (* subsequent occurrence of a temporary variable as a head argument *)
      let genHeadSubTempTypeVar varData offset lastUse needed =
	let (inst, size) =
	  if (needed) then 
	    ([Instr.Ins_get_type_value_t(offset, regNum)],
	     Instr.getSize_get_type_value_t)
	  else ([], 0)
	in
	Registers.markUnusedReg regNum;
	if (lastUse) then (Registers.mkRegFree offset; (inst, size))
	else (inst, size)
      in
      
      (* function body of genHeadTyVarCode *)
      let varData = Absyn.getTypeFreeVariableVariableData var in
      let lastUse = (Absyn.getTypeVariableDataLastUse varData) == var in
      let needed  = (Array.get neededness index) in
      match (Absyn.getTypeFreeVariableFirst var), 
	        (Absyn.getTypeVariableDataPerm varData) 
      with
      |  true,   true  -> (* first occurrence of a permanent variable *)
	    genHeadFirstPermTypeVar varData 
	    (Absyn.getTypeVariableDataOffset varData) needed
      | true,   false -> (* first occurrence of a temporay variable *)
	    genHeadFirstTempTypeVar varData lastUse needed
      | false,  true  -> (* subsequent occurrence of a permanent variable *)
	    genHeadSubPermTypeVar varData 
	    (Absyn.getTypeVariableDataOffset varData) needed
      | false,  false -> (* subsequent occurrence of a tempory variable *)
	    genHeadSubTempTypeVar varData 
	      (Absyn.getTypeVariableDataOffset varData) lastUse needed	
    in	  
    (* function body of genHeadTyVarsCode *)
    match tyargs with
      [] -> (List.flatten (List.rev insts), size, List.rev delayed)
    | (ty :: rest) ->
	if (Absyn.isTypeFreeVariable ty) then
	  let (inst, oneSize) = genHeadTyVarCode ty in
	  genHeadTyVarsCodeAux rest (regNum + 1) (inst :: insts) (oneSize+size)
	    delayed (index + 1)
	else
	  genHeadTyVarsCodeAux rest (regNum + 1) insts size 
	    ((regNum, ty)::delayed) (index + 1)
  in
  genHeadTyVarsCodeAux tyargs regNum [] 0 [] 0 
  
(*****************************************************************************)
(* genHeadTmVarsCode:                                                        *)
(* generating code for head variables; first occurrences of tempory variables*)
(* translate into a no-op and possible register assignment.                  *)
(*****************************************************************************)
let genHeadTmVarsCode args = 
  let rec genHeadTmVarsCodeAux args insts size delayed regNum =
	let genHeadTmVarCode var =
	  let varData = Absyn.getTermFreeVariableVariableData var in
	  if (Absyn.getVariableDataPerm varData) then
		let (inst, size) =
		  ([Instr.Ins_get_variable_p(Absyn.getVariableDataOffset varData, 
									 regNum)],
		   Instr.getSize_get_variable_p)
		in
		Registers.markUnusedReg regNum;
		(inst, size)
	  else (*temporary *)
		((if (Absyn.getVariableDataLastUse varData) == var 
		  then Registers.markUnusedReg regNum
		  else Registers.assignRegToTerm varData regNum); 
		 ([], 0))
	in

	match args with
	  [] -> (List.flatten (List.rev insts), size, List.rev delayed)
	| (arg :: rest) ->
		if (Absyn.isTermFreeVariable arg) && 
		   (Absyn.getTermFreeVariableFirst arg) then
		  let (inst, oneSize) = genHeadTmVarCode arg in
		  genHeadTmVarsCodeAux rest (inst :: insts) (size + oneSize)
			delayed (regNum + 1)
		else
		  genHeadTmVarsCodeAux rest insts size ((regNum, arg)::delayed) 
		    (regNum + 1)
	in
  genHeadTmVarsCodeAux args [] 0 [] 1


(*****************************************************************************)
(*  genClauseHeadCode                                                        *)
(*                                                                           *)
(*  The code below does all the processing relevant to the head of a clause. *)
(*  In particular, it inserts code for allocating an environment record if   *)
(*  necessary, inserts instructions for initializing closure variables and   *)
(*  inserts the necessary instructions for head type and term unification.   *)
(*  If any binding could have taken place to incoming term variables during  *)
(*  head unification, a finish_unify instruction is inserted at the end of   *)
(*  the head unification instructions.                                       *)
(*                                                                           *)
(*  This routine also calculates the start address of the new set of         *)
(*  instructions together with the new next code locations.                  *)
(*****************************************************************************)
let genClauseHeadCode cl chunk insts startLoc isFact =
  let numArgs = (Absyn.getClauseNumberOfArgs cl) in

  (* generate allocate if clause has environment *)
  let genAllocate insts startLoc =
    (if (Absyn.getClauseHasEnv cl) then
      let envSize = 
		(Absyn.getGoalEnvAssocNthEnvSize (Absyn.getClauseGespList cl) 1) + 1
      in
      (insts @ [Instr.Ins_allocate(envSize)],startLoc + Instr.getSize_allocate)
    else (insts, startLoc))
  in
  
  (* generate code for initialization of closure variables *)
  let genClauseInitCode insts startLoc =
    
    (* generate code for each type variable mapping *)
    let genOneTyVarCode (fromVar, toVar) =
      let fromOffset = Absyn.getTypeVariableDataOffset fromVar in
      if (Absyn.getTypeVariableDataPerm toVar) then 
		(Instr.Ins_init_type_variable_p(Absyn.getTypeVariableDataOffset toVar,
										fromOffset),
		 Instr.getSize_init_type_variable_p)
      else
		let _ = Registers.assignTyReg toVar true chunk numArgs in
		(Instr.Ins_init_type_variable_t(Absyn.getTypeVariableDataOffset toVar, 
										fromOffset),
		 Instr.getSize_init_type_variable_t)
    in
    (* generate code for each term variable mapping *)
    let genOneTmVarCode (fromVar, toVar) =
      let fromOffset = Absyn.getVariableDataOffset fromVar in
      if (Absyn.getVariableDataPerm toVar) then
	(Instr.Ins_init_variable_p(Absyn.getVariableDataOffset toVar, 
				   fromOffset),
	 Instr.getSize_init_variable_p)
      else
	let _ = Registers.assignTmReg toVar chunk numArgs in
	(Instr.Ins_init_variable_t(Absyn.getVariableDataOffset toVar, 
				   fromOffset),
	 Instr.getSize_init_variable_t)
    in
    (* for fold mapping result *)
	let collectMapCode (insts, totalSize) (inst, size) =
	  (inst :: insts, totalSize + size)
	in
    
	(* generate type variable mapping code *)
    let (Absyn.TypeVarMap(tyVarMaps)) = Absyn.getClauseTypeVarMaps cl in
    let (tyVarMapCode, tyVarMapCodeSize) =
	  List.fold_left collectMapCode ([], 0) 
		(List.map genOneTyVarCode tyVarMaps)
	in
	(* generate term variable mapping code *)
	let (Absyn.TermVarMap(tmVarMaps)) = Absyn.getClauseTermVarMaps cl in
	let (tmVarMapCode, tmVarMapCodeSize) =
	  List.fold_left collectMapCode ([], 0) 
		(List.map genOneTmVarCode tmVarMaps)
	in (insts @ (List.rev tyVarMapCode) @ (List.rev tmVarMapCode),
		startLoc + tyVarMapCodeSize + tmVarMapCodeSize)
  in

  (* generate type and term arguments unification code *)
  let genUnifCode insts startLoc =
	(* mark registers "perserved" for arguments passing *)
    Registers.markArgRegs numArgs; 

    (* unification code for type arguments *)
    let (tyArgsCode, tyArgsSize, regTypePairs) =
      genHeadTyVarsCode (Absyn.getClauseTypeArgs cl)
       	((Absyn.getClauseNumberOfTermArgs cl)+1)
	    (Absyn.getConstantNeedednessValue (Absyn.getClausePred cl)) 
    in
	let (tyCode, tySize) = genATypesCode regTypePairs chunk in
    
    (* unification code for term arguments *)
    let (tmArgsCode, tmArgsSize, regTermPairs) =
      genHeadTmVarsCode (Absyn.getClauseTermArgs cl) 
    in
    let (tmCode, tmCodeNext) =
      genATermsCode regTermPairs chunk 
		(insts @ tyArgsCode @ tyCode @ tmArgsCode) 
		(startLoc + tyArgsSize + tySize + tmArgsSize)
    in
    if (regTermPairs = []) then (tmCode, tmCodeNext)
    else (tmCode @ [Instr.Ins_finish_unify], 
		  tmCodeNext + Instr.getSize_finish_unify)
  in
  
  (* function body of genClauseHeadCode *)
  Absyn.setClauseOffset cl startLoc;
  Registers.setIsGoal false;
  let (allocCode, allocCodeNext) = 
	if (isFact) then (insts, startLoc)
	else genAllocate insts startLoc                
  in
  let (initCode, initCodeNext)   = genClauseInitCode allocCode allocCodeNext in
  let (unifCode, unifCodeNext)   = genUnifCode initCode initCodeNext         in
  (unifCode, unifCodeNext) 

(****************************************************************************)
(*          GENERATING INSTRUCTIONS FOR CLAUSE BODY                         *)
(****************************************************************************)

(****************************************************************************)
(* setUpGoalArgs:                                                           *)
(* Setting up the arguments of an atomic goal; the chunk sans this goal is  *)
(* returned together with new instructions and instruction size.            *)
(****************************************************************************)
let setUpGoalArgs goal chunk last hasenv =
  (* eagerly deal with register assignment for unneeded type variables *)
  let assignRegUnNeeded var =
    let varData = Absyn.getTypeFreeVariableVariableData var in
    let lastUse = (Absyn.getTypeVariableDataLastUse varData) == var in
    if (Absyn.getTypeVariableDataPerm varData) then 
      (*nothing to do w perm*)
      ()
    else 
      let mychunk = List.tl chunk in
      let mylowval = Registers.getNumGoalArgs () in
      if (Absyn.getTypeFreeVariableFirst var) then (* first appearence *)
        if lastUse then 
          ()
        else 
          let _ = Registers.assignTyReg varData false mychunk mylowval in ()
      else
        if lastUse then  
	      let regNum = Absyn.getTypeVariableDataOffset varData in
	        Registers.mkRegFree regNum;
	        Registers.markUnusedReg regNum
	    else 
          ()
  in

  (* pair up the type arguments with registers, resolving conflicts if   *)
  (* needed                                                              *)
  let genRegTypePairs regNumStart regNumEnd tyargs neededness
      : ((int * Absyn.atype) list) * (Instr.instruction list) * int =
    let rec genRegTypePairsAux regNum tyargs index insts size regTyPairs =
      if (regNum > regNumEnd) then 
	(List.rev regTyPairs, List.flatten (List.rev insts), size)
      else
	let ty  = List.hd tyargs in
	let needed = Array.get neededness index in
	let (myInst,mySize)=Registers.resolveRegTypeConflict ty regNum chunk in
	if (needed) then
	  genRegTypePairsAux (regNum + 1) (List.tl tyargs) (index + 1)
	    (myInst :: insts) (size + mySize) ((regNum, ty) :: regTyPairs)
	else
	  ((if (Absyn.isTypeFreeVariable ty) then assignRegUnNeeded ty
	  else ());
	   genRegTypePairsAux (regNum + 1) (List.tl tyargs) (index + 1)
	     (myInst :: insts) (size + mySize) regTyPairs)
    in
    genRegTypePairsAux regNumStart tyargs 0 [] 0 []
  in

  (* pair up the term arguments with registers, resolving conflicts if  *)
  (* needed                                                             *)
  let genRegTermPairs regNumEnd args =
    let rec genRegTermPairsAux regNum args insts size regTmPairs =
      if (regNum > regNumEnd) then
	(List.rev regTmPairs, List.flatten (List.rev insts), size)
      else
	let tm = List.hd args in
	let (myInst,mySize)=Registers.resolveRegTermConflict tm regNum chunk in
	genRegTermPairsAux (regNum + 1) (List.tl args) (myInst :: insts)
	  (size + mySize) ((regNum, tm)::regTmPairs)
    in
    genRegTermPairsAux 1 args [] 0 []
  in

  (* function body of setUpGoalArgs *)

  let numTermArgs = Absyn.getAtomicGoalNumberOfTermArgs goal in
  let neededness  = 
	Absyn.getConstantNeedednessValue (Absyn.getAtomicGoalPredicate goal)
  in
  let numGoalArgs = Registers.getNumGoalArgs () in
  let (regTypePairs, typeConflictCode, typeConflictSize) =
    genRegTypePairs (numTermArgs + 1) numGoalArgs 
      (Absyn.getAtomicGoalTypeArgs goal) neededness 
  in
  let (regTermPairs, termConflictCode, termConflictSize) =
    genRegTermPairs numTermArgs (Absyn.getAtomicGoalTermArgs goal) 
  in
  let newChunk = List.tl chunk in   (* remove this goal from the chunk *)
  (* mark the argument registers *)
  Registers.markArgRegs (Absyn.getAtomicGoalNumberOfArgs goal);
  let (typesCode, typesSize) = (* set up (needed) type arguments *)
    genSTypesCode regTypePairs newChunk numGoalArgs last
  in
  let (termsCode, termsSize) = (* set up term arguments *)
    genSTermsCode regTermPairs newChunk numGoalArgs last hasenv 
  in
  (* unmark argument registers if not occupied by a temporary *)
  Registers.cleanUpRegs (); 
  (typeConflictCode @ termConflictCode @ typesCode @ termsCode,
   typeConflictSize + termConflictSize + typesSize + termsSize,
   newChunk)

(****************************************************************)
(* getGoalEnvSize:                                              *)
(* finding out the environment size at the corrent goal         *)
(****************************************************************)
let getGoalEnvSize clause goalNum =
  let gesplist = Absyn.getClauseGespList clause in
  match gesplist with
    Absyn.GoalEnvAssoc([]) -> 0
  | Absyn.GoalEnvAssoc((goalInd, envSize)::rest) ->
      if (goalInd < goalNum) then 
	(Absyn.setClauseGespList clause (Absyn.GoalEnvAssoc rest);	 
	 match rest with
	   [] -> 0
	 | ((goalInd', envSize')::rest') -> envSize')
      else envSize
		  
(*****************************************************************)
(* getChunk:                                                     *)
(* get the chunk the current goal belonging to                   *)
(*****************************************************************)
let getChunk chunk chunks =
  if (chunk = []) then ((List.hd chunks), (List.tl chunks))
  else (chunk, chunks)		

(****************************************************************************)
(* genGoal:                                                                 *)
(* generate code for a goal                                                 *)
(****************************************************************************)
let rec genGoal goal cl goalNum last chunk chunks insts startLoc = 
  match goal with
    Absyn.AtomicGoal(_) -> 
      genAtomicGoal goal cl goalNum last chunk chunks insts startLoc
  | Absyn.AndGoal(_)    ->
      genAndGoal goal cl goalNum last chunk chunks insts startLoc
  | Absyn.ImpGoal(_)    ->
      genImpGoal goal cl goalNum last chunk chunks insts startLoc 
  | Absyn.AllGoal(_)    ->
      genAllGoal goal cl goalNum last chunk chunks insts startLoc
  | Absyn.SomeGoal(_)   ->
      genSomeGoal goal cl goalNum last chunk chunks  insts startLoc
  | Absyn.CutFailGoal -> 
      Errormsg.impossible Errormsg.none "genGoal: cutfail goal"


(****************************************************************************)
(* genAtomicGoal:                                                           *)
(* generating code for an atomic goal.                                      *)
(****************************************************************************)
and genAtomicGoal goal cl goalNum last chunk chunks insts startLoc =

  (* generate code for arguments and deallocate *)
  let genAtomicGoalArgs chunk hasenv =
	let (argsCode, argsCodeSize, newChunk) =
	  setUpGoalArgs goal chunk last (not(last) && hasenv)
	in
	if (last && hasenv) then
	  (argsCode @ [Instr.Ins_deallocate],argsCodeSize+Instr.getSize_deallocate,
	   newChunk)
	else
	  (argsCode, argsCodeSize, newChunk)
  in

  (* generate code for cut goal *)
  let genCutCode chunk =
	let (cutInst, cutInstSize) =
	  if (goalNum = 1) then (Instr.Ins_neck_cut, Instr.getSize_neck_cut)
	  else 
		(Instr.Ins_cut(Absyn.getVariableDataOffset (Absyn.getClauseCutVar cl)),
		 Instr.getSize_cut)
	in
	let (inst, size) =
	  if (last) then
		if (Absyn.getClauseHasEnv cl) then 
		  ([cutInst ; Instr.Ins_deallocate ; Instr.Ins_proceed], 
		   cutInstSize + Instr.getSize_deallocate + Instr.getSize_proceed)
		else
		  ([cutInst ; Instr.Ins_proceed],cutInstSize + Instr.getSize_proceed)
	  else ([cutInst], cutInstSize)
	in
	(insts @ inst, startLoc + size, List.tl chunk, goalNum)
  in
  (* generate code for true goal *)
  let genTrueCode chunk =
	let (inst, size) =
	  if (last) then 
		if (Absyn.getClauseHasEnv cl) then
		  ([Instr.Ins_deallocate], Instr.getSize_deallocate)
		else
		  ([Instr.Ins_proceed], Instr.getSize_proceed)
	  else ([], 0)
	in
	(insts @ inst, startLoc + size, List.tl chunk, goalNum)
  in
  (* generate code for fail goal *)
  let genFailCode chunk =
	(insts @ [Instr.Ins_fail], startLoc + Instr.getSize_fail, 
	 List.tl chunk, goalNum)
  in
  (* generate code for halt goal *)
  let genHaltCode chunk =
	(insts @ [Instr.Ins_halt], startLoc + Instr.getSize_halt,
	 List.tl chunk, goalNum)
  in
  (* generate code for stop goal *)
  let genStopCode chunk =
	(insts @ [Instr.Ins_stop], startLoc + Instr.getSize_stop, 
	 List.tl chunk, goalNum)
  in
  (* generate code for other pervasives *)
  let genPervCode chunk pred envSize =
	let hasEnv =  Absyn.getClauseHasEnv cl in
	let builtinInd = Absyn.getConstantCodeInfoBuiltinIndex pred in
	let (argsCode, argsCodeSize, newChunk) = genAtomicGoalArgs chunk hasEnv in
	let (callCode, callCodeSize, newGoalNum) =
	  if (last) then 
		([Instr.Ins_builtin(builtinInd)], Instr.getSize_builtin, goalNum)
	  else
		([Instr.Ins_call_builtin(envSize, builtinInd)],
		 Instr.getSize_call_builtin,
		 if (Pervasive.regClobberingPerv pred) ||
		 (Pervasive.backtrackablePerv pred)
		 then goalNum + 1
		 else goalNum)
	in
	(insts @ argsCode @ callCode, startLoc + argsCodeSize + callCodeSize,
	 newChunk, newGoalNum)
  in
  
  (* generate code for pervasives *)
  let genPervasiveGoal pred chunk envSize =
	if (Pervasive.iscutConstant pred)
	then genCutCode chunk 
	else if (Pervasive.istrueConstant pred)
	then genTrueCode chunk 
	else if (Pervasive.isfailConstant pred)
	then genFailCode chunk
	else if (Pervasive.ishaltConstant pred)
	then genHaltCode chunk
	else if (Pervasive.isstopConstant pred)
	then genStopCode chunk
	else genPervCode chunk pred envSize		            
  in

  (* generate "execute" code *)
  let genExecute pred = 
	if (Absyn.getConstantClosed pred) then
	  let expdef = (Absyn.getConstantExportDef pred) in
	  if (Absyn.constantHasCode pred) &&
	    ((not (Absyn.isGlobalConstant pred)) || expdef) then
	    if (Absyn.isAnonymousConstant pred) then
	      let (instr, size) =
		(Instr.Ins_execute(ref 0), Instr.getSize_execute)
	      in
	      addBackPatchExecute pred instr;
	      ([instr], size, goalNum)
	    else
	      ([Instr.Ins_execute_link_only(pred)], Instr.getSize_execute_link_only,
	       goalNum)
	  else (* do not have code *)
	    if (List.memq pred (getAccConsts ())) then
	      ([Instr.Ins_execute_link_only(pred)], Instr.getSize_execute_link_only,
	       goalNum)
	    else
	      if (expdef) then 
		 ([Instr.Ins_fail], Instr.getSize_fail, goalNum)
	      else 
		([Instr.Ins_execute_name(pred)], Instr.getSize_execute_name, 
		 goalNum)
	else (* not closed *)
	   ([Instr.Ins_execute_name(pred)], Instr.getSize_execute_name,
	   goalNum) 
  in

  (* generate "call" code *)
  let genCall pred envsize =
    let myGoalNum = goalNum + 1 in
    if (Absyn.getConstantClosed pred) then
      let expdef = (Absyn.getConstantExportDef pred) in
      if (Absyn.constantHasCode pred) && 
	((not (Absyn.isGlobalConstant pred)) || expdef) then
	if (Absyn.isAnonymousConstant pred) then
	  let (instr, size) = 
	    (Instr.Ins_call(envsize, ref 0), Instr.getSize_call)
	  in
	  addBackPatchCall pred instr;
	  ([instr], size, myGoalNum)
	else
	  ([Instr.Ins_call_link_only(envsize, pred)], Instr.getSize_call_link_only,
	   myGoalNum)
      else (* do not have code *)
	if (List.memq pred (getAccConsts ())) then 
	  ([Instr.Ins_call_link_only(envsize, pred)], Instr.getSize_call_link_only,
	   myGoalNum)
	else
	  if (expdef) then 
	    ([Instr.Ins_fail], Instr.getSize_fail, myGoalNum)
	  else 
	    ([Instr.Ins_call_name(envsize, pred)], Instr.getSize_call_name, 
	     myGoalNum)
    else (* not closed *)
      ([Instr.Ins_call_name(envsize, pred)], Instr.getSize_call_name,
       myGoalNum)
  in
	  
  (* generate code for non pervasive predicates *)
  let genNonPervasiveAtomicGoal pred chunk envSize =
    let hasEnv =  Absyn.getClauseHasEnv cl in
    let (argsCode, argsCodeSize, newChunk) = genAtomicGoalArgs chunk hasEnv in
    let (callCode, callCodeSize, newGoalNum) =
      if (last) then genExecute pred 
      else genCall pred envSize
    in
    (insts @ argsCode @ callCode, startLoc + argsCodeSize + callCodeSize,
     newChunk, newGoalNum)
  in
  
  (* function body of genAtomicGoal *)
  Registers.setIsGoal true;
  Registers.setNumGoalArgs (Absyn.getAtomicGoalNumberOfArgs goal);
  (* get correct chunk to work on *)
  let (myChunk, myChunks) = getChunk chunk chunks in
  (* find environment size of the current goal *)
  let envSize = getGoalEnvSize cl goalNum in
  (* find the predicate of this goal *)
  let pred = (Absyn.getAtomicGoalPredicate goal) in

  let (newInsts, newStartLoc, newChunk, newGoalNum) =
    if (Pervasive.isPerv pred) then genPervasiveGoal pred myChunk envSize
    else genNonPervasiveAtomicGoal pred myChunk envSize
  in
  (newInsts, newStartLoc, newChunk, myChunks, newGoalNum)
  
  
(************************************************************************)
(* genAndGoal:                                                          *)
(* gnerating code for an and goal                                       *)
(************************************************************************)
and genAndGoal goal cl goalNum last chunk chunks insts startLoc =
  let (newInsts, newStartLoc, newChunk, newChunks, newGoalNum) =
    genGoal (Absyn.getAndGoalLeftOperand goal) cl goalNum false chunk
      chunks insts startLoc 
  in
  genGoal (Absyn.getAndGoalRightOperand goal) cl newGoalNum last newChunk
    newChunks newInsts newStartLoc

(************************************************************************)
(* genAllGoal:                                                          *)
(* gnerating code for an all goal                                       *)
(************************************************************************)
and genAllGoal goal cl goalNum last chunk chunks insts startLoc =
  (* for each univ quantified variable *)
  let genAllGoalHCVar (varData, constData) =
	(Instr.Ins_set_univ_tag (Absyn.getVariableDataOffset varData, constData),
	 Instr.getSize_set_univ_tag)
  in
  (* for fold mapping result *)
  let collectMapCode (insts, totalSize) (inst, size) =
	(inst :: insts, totalSize + size)
  in

  let (newChunk, newChunks) = getChunk chunk chunks in
  (* increase universe instruction *)
  let (incUniv, incUnivSize) =
	(Instr.Ins_incr_universe, Instr.getSize_incr_universe) 
  in
  (* instructions for initializing universally quantified variables *)
  let Absyn.HCVarAssocs(hcvars) = Absyn.getAllGoalHCVarAssocs goal in
  let (hcVarCode, hcVarCodeSize) =
	List.fold_left collectMapCode ([incUniv], incUnivSize) 
      (List.map genAllGoalHCVar hcvars)
  in
  (* instructions for goal body *)
  let (bodyCode, bodyCodeNext, newChunk', newChunks', newGoalNum) = 
	genGoal (Absyn.getAllGoalBody goal) cl goalNum false newChunk newChunks
	  (insts @ (List.rev hcVarCode)) (startLoc + hcVarCodeSize)
  in
  (* decrease universe and continuation if necessary*) 
  let (endCode, endSize) =
	if (last) then 
	  if (Absyn.getClauseHasEnv cl) then 
		([Instr.Ins_decr_universe ; Instr.Ins_deallocate; Instr.Ins_proceed],
		 Instr.getSize_decr_universe + Instr.getSize_deallocate +
		   Instr.getSize_proceed)
	  else
		([Instr.Ins_decr_universe ; Instr.Ins_proceed],
		 Instr.getSize_decr_universe + Instr.getSize_proceed)
	else ([Instr.Ins_decr_universe], Instr.getSize_decr_universe)
  in
  (bodyCode @ endCode, bodyCodeNext + endSize, newChunk', newChunks', 
   newGoalNum)

(************************************************************************)
(* genSomeGoal:                                                         *)
(* gnerating code for a some goal                                       *)
(************************************************************************)
and genSomeGoal goal cl goalNum last chunk chunks insts startLoc =
  let (newChunk, newChunks) = getChunk chunk chunks in
  (* instruction for tagging existential quantified variable *)
  let qvarData = Absyn.getSomeGoalQuantVar goal in
  let (tagCode, tagCodeSize) =
	if (Absyn.getVariableDataPerm qvarData) then
	  (Instr.Ins_tag_exists_p(Absyn.getVariableDataOffset qvarData), 
	   Instr.getSize_tag_exists_p)
	else
	  let _ = Registers.assignTmReg qvarData newChunk 0 in
	  (Instr.Ins_tag_exists_t(Absyn.getVariableDataOffset qvarData), 
	   Instr.getSize_tag_exists_t)
  in
  genGoal (Absyn.getSomeGoalBody goal) cl goalNum last newChunk newChunks
	(insts @ [tagCode]) (startLoc + tagCodeSize)
  
(************************************************************************)
(* genImpGoal:                                                          *)
(* gnerating code for an implication goal                               *)
(************************************************************************)
and genImpGoal goal cl goalNum last chunk chunks insts startLoc =
  (* tag init variables *)
  let genVarInit varData =
	 Instr.Ins_tag_variable(Absyn.getVariableDataOffset varData), 
	 Instr.getSize_tag_variable
  in
  (* create init type variable *)
  let genTyVarInit varData =
	(Instr.Ins_create_type_variable(Absyn.getTypeVariableDataOffset varData),
	 Instr.getSize_create_type_variable)
  in
  (* for fold mapping result *)
  let collectMapCode (insts, totalSize) (inst, size) =
	(inst :: insts, totalSize + size)
  in

  let envSize = getGoalEnvSize cl goalNum in
  let (newChunk, newChunks) = getChunk chunk chunks in
  (* instructions for tagging variable init list *)
  let Absyn.VarInits(varList) = Absyn.getImpGoalVarInits goal in
  let (varInitCode, varInitCodeSize) =
	List.fold_left collectMapCode ([], 0) (List.map genVarInit varList)
  in
 
  (* instructions for creating type variable init *)
  let Absyn.TypeVarInits(tyVarList) = Absyn.getImpGoalTypeVarInits goal in
  let (tyvarInitCode, tyvarInitCodeSize) =
	List.fold_left collectMapCode ([], 0) (List.map genTyVarInit tyVarList)
  in
  (* record definitions to be processed later *)
  addImpPointList (Absyn.getImpGoalClauses goal);
  (* instructions for push impl point and for the goal body *)
  let (bodyCode, bodyCodeNext, newChunk', newChunks', newGoalNum) =
    genGoal (Absyn.getImpGoalBody goal) cl goalNum false newChunk newChunks
	  (insts @ (List.rev 
				  (Instr.Ins_push_impl_point(envSize, (getNumImpPoints () + getTotImpPoints ()) -1) ::
				   (tyvarInitCode @ varInitCode))))
	  (startLoc + varInitCodeSize + tyvarInitCodeSize + 
		 Instr.getSize_push_impl_point)
  in
  (* pop impl point instructions and those for continuation if necessary *)
  let (endCode, endCodeSize) =
    if (last) then
      if (Absyn.getClauseHasEnv cl) then
		([Instr.Ins_pop_impl_point ; Instr.Ins_deallocate; Instr.Ins_proceed],
		 Instr.getSize_pop_impl_point + Instr.getSize_deallocate + 
		   Instr.getSize_proceed)
      else
		([Instr.Ins_pop_impl_point ; Instr.Ins_proceed],
		 Instr.getSize_pop_impl_point + Instr.getSize_proceed)
    else
      ([Instr.Ins_pop_impl_point], Instr.getSize_pop_impl_point)
  in
  (bodyCode @ endCode, bodyCodeNext + endCodeSize, newChunk', newChunks',
   newGoalNum)
            

(*****************************************************************************)
(* genClauseBodyCode:                                                        *)
(* generate instructions for a clause body                                   *)
(*****************************************************************************)
let genClauseBodyCode cl chunks insts startLoc =
  let goal   = Absyn.getClauseGoal cl in
  let impseg = Absyn.getClauseImports cl in

  (* generate add_imports and push_import's *)
  let genPushImports insts startLoc =
    let rec genPushImportsAux imports ind insts startLoc = 
      match imports with
		[] -> (insts, startLoc) 
      | (import :: rest) ->
		  genPushImportsAux rest (ind+1) (insts @ [Instr.Ins_push_import(ind)])
	        (startLoc + Instr.getSize_push_import)
    in
    let envSize = 
      (Absyn.getGoalEnvAssocNthEnvSize (Absyn.getClauseGespList cl) 1)
    in
    (*      add_imports 1 envSize L     *)
    let nextCodeLocRef = ref 0 in
    let add_imports = Instr.Ins_add_imports(1, envSize, nextCodeLocRef) in
    (*      push_imports instructions   *)
    (*   L:                             *)
    let (newInsts, nextCodeLoc) =
      genPushImportsAux impseg 0 (insts @ [add_imports]) 
	                (startLoc + Instr.getSize_add_imports)
    in
    nextCodeLocRef := nextCodeLoc;
    (newInsts, nextCodeLoc)
  in  

  (* generate remove_imports, pop_imports, deallocate and proceed *)
  let genPopImports insts startLoc =
    let numImports = List.length impseg in
    let deallocCodeLoc = 
      startLoc + Instr.getSize_remove_imports + Instr.getSize_pop_imports 
    in
    (*      remove_imports 1 L          *)
    let remove_imports = Instr.Ins_remove_imports(1, ref deallocCodeLoc) in
    (*      pop_imports numImports      *)
    let pop_imports    = Instr.Ins_pop_imports(numImports)               in
    (*  L:  deallocate                  *)
    let deallocate     = Instr.Ins_deallocate                            in
    (*      proceed                     *)
    let proceed        = Instr.Ins_proceed                               in
    (insts @ [remove_imports ; pop_imports ; deallocate ; proceed],
     deallocCodeLoc + Instr.getSize_deallocate + Instr.getSize_proceed)
  in
			   
  match impseg with
    [] ->
	  let (bodyCode, bodyCodeNext, _, _, _) =
		genGoal goal cl 1 true (List.hd chunks) (List.tl chunks) insts startLoc
	  in
	  (bodyCode, bodyCodeNext)
  | _  -> 
      let (pushImportsCode, pushImportsNext) = 
		genPushImports insts startLoc 
      in
      let (bodyCode, bodyCodeNext, _, _, _) = 
		genGoal goal cl 1 false (List.hd chunks) (List.tl chunks) 
		  pushImportsCode pushImportsNext 
      in
      genPopImports bodyCode bodyCodeNext


(****************************************************************************)
(*          GENERATING INSTRUCTIONS FOR ONE CLAUSE                          *)
(****************************************************************************)
let genClauseCode cl insts startLoc =
  let isFact = Absyn.isClauseFact cl in
  let chunks = 
    if isFact then [[]] else Registers.chunkify (Absyn.getClauseGoal cl) 
  in

  (* generate code for clause head *)
  let genClauseHead insts startLoc'  =
    let (newInsts, newStartLoc) = 
      genClauseHeadCode cl (List.hd chunks) insts startLoc' isFact
    in
    if isFact 
    then (newInsts @ [Instr.Ins_proceed], newStartLoc + Instr.getSize_proceed)
    else (newInsts, newStartLoc)
  in

  (* generate code if has cut var*)
  let genClauseCut insts startLoc   =
    if isFact then (insts, startLoc)
    else 
      let cutVar = Absyn.getClauseCutVarOption cl in
      match cutVar with
		None -> (insts, startLoc)
      | Some(vData) -> 
		  (insts @ [Instr.Ins_get_level(Absyn.getVariableDataOffset(vData))],
		   startLoc + Instr.getSize_get_level)
  in
  
  (* generate code for body *)
  let genClauseBody insts startLoc =
	  if isFact then (insts, startLoc)
	  else genClauseBodyCode cl chunks insts startLoc
    in
    let (instsHead, instsHeadNext) = genClauseHead insts startLoc          in
    let (instsCut,  instsCutNext)  = genClauseCut  instsHead instsHeadNext in
    let (instsBody, instsBodyNext) = genClauseBody instsCut instsCutNext   in
    (instsBody, instsBodyNext)
