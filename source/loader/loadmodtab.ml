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
let rec loadGlobalConsts constSymTab index globalConsts numConsts tySkelTab =
  (* decide how to fill in type skeleton of the constant *)
  let loadConstTypeSkel ind =
	Absyn.Skeleton(Array.get tySkelTab ind, ref (Some ind), ref false)	
  in

  (* load one constant and add it into const symbol table *)
  let loadConst constSymTab index =
	let const = Bytecode.readGlobalConstant loadConstTypeSkel index in
    Array.set globalConsts (index - Pervasive.numberPervasiveConstants) (Some const);
	Table.add (Absyn.getConstantSymbol const) const constSymTab
  in
  if (numConsts = 0) then constSymTab
  else 
	loadGlobalConsts (loadConst constSymTab index) (index + 1)
      globalConsts (numConsts - 1) tySkelTab

let skipLocalConsts () =
  let numLConsts = Bytecode.readTwoBytes() in
  Bytecode.skipNBytes (5 * numLConsts)

let skipHiddenConsts () =
  let numHConsts = Bytecode.readTwoBytes() in
  Bytecode.skipNBytes (2 * numHConsts)
  
let loadConstSymTab tySkeletonTab = 
  let _ = Bytecode.readTwoBytes () in 
  let numberGConsts = Bytecode.readTwoBytes () in
  let globalConsts = (Array.make numberGConsts None) in
  let constSymTab = loadGlobalConsts (Pervasive.pervasiveConstants)
	                  (Pervasive.numberPervasiveConstants)
                      globalConsts numberGConsts tySkeletonTab in
  let _ = skipLocalConsts () in
  let _ = skipHiddenConsts () in
  (constSymTab,globalConsts)
  

(****************************************************************************)
(*                       LOAD IMPORT TABLES                                 *)
(****************************************************************************)

(* TODO: This is wrong, need to look at linkcode.txt
 * but link bytecode does not keep track of exportdefs!
 *)
  
(* let loadImportTab globalConsts =
 *   prerr_endline "Loading import tables...";
 *   let skipPredTab () =
 *     let numPreds = Bytecode.readTwoBytes () in
 *     prerr_endline(Format.sprintf "Skipping predicates: %d" numPreds);
 *     Bytecode.skipNBytes (3 * numPreds)
 *   in
 *   let skipSearchTab () =
 *     let numPreds = Bytecode.readTwoBytes () in
 *     prerr_endline(Format.sprintf "Skipping predicates: %d" numPreds);
 *     Bytecode.skipNBytes (numPreds * (Bytecode.getWordSize()))
 *   in
 *   let loadExpDefsTab () =
 *     let numPreds = Bytecode.readTwoBytes () in
 *     prerr_endline(Format.sprintf "Loading ExpDefs: %d" numPreds);
 *     List.iter (fun _ ->
 *         let readConstFn = (fun _ ind ->
 *             Array.get globalConsts (ind - Pervasive.numberPervasiveConstants)) in
 *         let const = Option.get @@ Bytecode.readConstantIndex readConstFn in
 *         prerr_endline (Format.sprintf "Loading exportdef constant: %s"
 *                          (Absyn.getConstantName const));
 *         let expdef = Absyn.getConstantExportDefRef const in
 *         expdef := true)
 *       (List.init numPreds (fun i -> ()))
 *   in
 *   (\* skip nonExpDefs *\)
 *   let _ = skipPredTab () in
 *   let _ = loadExpDefsTab () in
 *   (\* skip localDefs *\)
 *   let _ = skipPredTab () in
 *   let _ = Bytecode.skipNBytes 1 in
 *   skipSearchTab ()
 *   
 * let loadImportTabs globalConsts =
 *   let numImportTabs = Bytecode.readTwoBytes() in *)
  
  
(****************************************************************************)
(*                       SKIP TABLES                                        *)
(****************************************************************************)
let skipStringTab () =
  let numStrings = Bytecode.readTwoBytes () in
  (* prerr_endline(Printf.sprintf "Skipping strings: %d" numStrings); *)
  List.iter (fun _ -> ignore @@ Bytecode.readString())
    (List.init numStrings (fun i -> ()))

let skipImpGoalTabs () =
  let skipImpGoalTab () =
    let numExtPreds = Bytecode.readTwoBytes() in
    let _ = Bytecode.skipNBytes (3 * numExtPreds) in
    let _ = Bytecode.readOneByte() in
    let numDefs = Bytecode.readTwoBytes() in
    Bytecode.skipNBytes ((3 + (Bytecode.getWordSize())) * numDefs)
  in
  let numImpGoalTabs = Bytecode.readTwoBytes() in
  (* prerr_endline(Printf.sprintf "Skipping implication goal tables: %d" numImpGoalTabs); *)
  List.iter skipImpGoalTab (List.init numImpGoalTabs (fun i -> ()))

let skipHashTabs () =
  let skipHashTab () =
    let numEntries = Bytecode.readTwoBytes() in
    Bytecode.skipNBytes ((3 + (Bytecode.getWordSize())) * numEntries)
  in
  let numHashTabs = Bytecode.readTwoBytes() in
  (* prerr_endline(Printf.sprintf "Skipping Hash tables: %d" numHashTabs); *)
  List.iter skipHashTab (List.init numHashTabs (fun i -> ()))

let skipBVTabs () =
  Bytecode.skipNBytes 2


(****************************************************************************)
(*                       INTERFACE FUNCTION                                 *)
(****************************************************************************)
let loadModuleTable modName =
  Bytecode.openInChannel (Bytecode.makeLinkedByteCodeName modName);
  let modName = loadHeaderInfo () in
  let (kindSymTab, globalKinds) = loadKindSymTab () in
  let tySkeletonTab = loadTypeSkeletons globalKinds in
  let (constSymTab,globalConsts) = loadConstSymTab tySkeletonTab in
  (* strings *)
  let _ = skipStringTab() in
  (* imp goal tables *)
  let _ = skipImpGoalTabs() in
  (* hash tables *)
  let _ = skipHashTabs() in
  (* bv tables *)
  let _ = skipBVTabs() in
  (* import tables *)
  (* TODO: loading exportdefs would require changing the linker bytecode! *)
  (* let _ = loadImportTabs globalConsts in *)
  Bytecode.closeInChannel ();
  Absyn.Module(modName, [], [], ref constSymTab, ref kindSymTab, 
			   Pervasive.pervasiveTypeAbbrevs, [], [], [], [], [], 
			   ref [], [], ref [], ref (Absyn.ClauseBlocks []))


