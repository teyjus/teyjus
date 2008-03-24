(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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
(*                    LOAD HEADER INFORMATION                              *)
(***************************************************************************)
let loadHeaderInfo () =
  (* check bytecode version number *)
  (if (Bytecode.readWord () = Bytecode.linkedByteCodeVersionNumber) then ()
   else Errormsg.error Errormsg.none "Loader: inconsistent bytecode version");
  let modName = Bytecode.readString () in
  Bytecode.skipNWords 1;
  modName


(****************************************************************************)
(*                 LOAD KIND SYMBOL TABLE                                   *)
(****************************************************************************)
(* global kinds *)
let loadGlobalKinds kindSymTab index numberGlobalKinds =
  let globalKinds = Array.make numberGlobalKinds None in

  let rec loadGlobalKindsAux kindSymTab tabInd arrayInd =
	if (arrayInd = numberGlobalKinds) then kindSymTab 
	else
	  (* load one kind and add into kind symbol table *)
	  let loadKind () =
		let kind  = Bytecode.readGlobalKind tabInd in
		Array.set globalKinds arrayInd (Some kind);
		Table.add (Absyn.getKindSymbol kind) kind kindSymTab
	  in
	  loadGlobalKindsAux (loadKind ()) (tabInd + 1) (arrayInd + 1)
  in
  (loadGlobalKindsAux kindSymTab index 0, globalKinds)

(* local kinds *)
let skipLocalKinds () =
  let numLocalKinds = Bytecode.readTwoBytes () in
  Bytecode.skipNBytes numLocalKinds

let loadKindSymTab () =
  (* skip total number of kinds *)
  let _ = Bytecode.readTwoBytes () in
  (* load global kinds *)
  let numberGlobalKinds = Bytecode.readTwoBytes () in
  let (kindSymbolTab, globalKinds) =
	loadGlobalKinds (Pervasive.pervasiveKinds) (Pervasive.numberPervasiveKinds)
	  numberGlobalKinds 
  in
  (* skip local kind information *)
  skipLocalKinds();
  (kindSymbolTab, globalKinds)

(****************************************************************************)
(*                 LOAD TYPE SKELETON TABLE                                 *)
(****************************************************************************)
(* load type skeletons and enter them into type skeleton table *)
let loadTypeSkeletons globalKinds =

  let getKind cat index =
	if cat = Bytecode.global then
	  Array.get globalKinds index 
	else if cat = Bytecode.pervasive then
	  Pervasiveutils.findKindIndexMapping index
	else None
  in

  let numberTypeSkels = Bytecode.readTwoBytes () in
  let typeSkeletonTab = Array.make numberTypeSkels Absyn.ErrorType in

  let rec loadTypeSkeletonsAux index =
	if (index = numberTypeSkels) then ()
	else
	  let typeSkel = Bytecode.readTypeSkeleton getKind in
	  Array.set typeSkeletonTab index typeSkel;
	  loadTypeSkeletonsAux (index + 1)
  in
  loadTypeSkeletonsAux 0;
  typeSkeletonTab
	

(****************************************************************************)
(*                   LOAD (GLOBAL) CONSTANTS                                *)
(****************************************************************************)
let rec loadGlobalConsts constSymTab index numConsts tySkelTab =
  (* decide how to fill in type skeleton of the constant *)
  let loadConstTypeSkel ind =
	Absyn.Skeleton(Array.get tySkelTab ind, ref (Some ind), ref false)	
  in

  (* load one constant and add it into const symbol table *)
  let loadConst constSymTab index =
	let const = Bytecode.readGlobalConstant loadConstTypeSkel index in
	Table.add (Absyn.getConstantSymbol const) const constSymTab
  in
  if (numConsts = 0) then constSymTab 
  else 
	loadGlobalConsts (loadConst constSymTab index) (index + 1) (numConsts - 1)
	  tySkelTab

let loadConstSymTab tySkeletonTab = 
  let _ = Bytecode.readTwoBytes () in 
  let numberGConsts = Bytecode.readTwoBytes () in
  loadGlobalConsts (Pervasive.pervasiveConstants)
	(Pervasive.numberPervasiveConstants) numberGConsts tySkeletonTab 
  
(****************************************************************************)
(*                       INTERFACE FUNCTION                                 *)
(****************************************************************************)
let loadModuleTable modName =
  Bytecode.openInChannel (Bytecode.makeLinkedByteCodeName modName);
  let modName = loadHeaderInfo () in
  let (kindSymTab, globalKinds) = loadKindSymTab () in
  let tySkeletonTab = loadTypeSkeletons globalKinds in
  let constSymTab = loadConstSymTab tySkeletonTab in 
  Bytecode.closeInChannel ();
  Absyn.Module(modName, [], [], ref constSymTab, ref kindSymTab, 
			   Pervasive.pervasiveTypeAbbrevs, [], [], [], [], [], 
			   ref [], [], ref [], ref (Absyn.ClauseBlocks []))


