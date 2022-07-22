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
(* The code in this module is in charge of putting out all the information *)
(* generated by the procedures in codegen after a successful processing    *)
(* of a module into in the required bytecode form into a specified byteode *)
(* file.                                                                   *)
(***************************************************************************)
(***********************************************************************)
(* auxiliary functions used in the module                              *)
(***********************************************************************)
let map func mylist =
  let _ = List.map func mylist in
  ()

let writeKindIndex kind =
  let cat =
    match (Absyn.getKindType kind) with
      Absyn.LocalKind -> Bytecode.local
    | Absyn.GlobalKind -> Bytecode.global
    | Absyn.PervasiveKind -> Bytecode.pervasive
  in
  Bytecode.writeint1 cat;
  Bytecode.writeint2 (Absyn.getKindIndex kind)

let getConstantMark constCat =
  match constCat with 
	Absyn.GlobalConstant -> Bytecode.global
  | Absyn.LocalConstant  -> Bytecode.local
  | Absyn.PervasiveConstant (_) -> Bytecode.pervasive
  | _ -> Bytecode.hidden (* assumed to be hidden constant *)		
  
let writeConstIndex const =
  let cty = (getConstantMark (Absyn.getConstantType const)) in
  let ci = (Absyn.getConstantIndex const) in
  prerr_endline(Printf.sprintf "Writing const %d~%d" cty ci);

  Bytecode.writeint1 (getConstantMark (Absyn.getConstantType const));
  Bytecode.writeint2 (Absyn.getConstantIndex const)

(*****************************************************************************)
(*                  WRITING OUT MODULE HEADER INFORMATION                    *)
(*****************************************************************************)
let writeHeader modName codeSize =
  (* <bytecode version number> *)
  Bytecode.writeWord Bytecode.byteCodeVersionNumber;
  (* [module name] *)
  Bytecode.writeString modName;
  (* <code size in bytes> *)
  Bytecode.writeWord codeSize

let writeCodeSize codeSize =
  Bytecode.writeWord codeSize

(****************************************************************************)
(*                     WRITING OUT KIND INFORMATION                         *)
(****************************************************************************)
let writeKindInfo gkinds lkinds = 
  (* global kind: arity, name *)
  let writeGlobalKind kind =
	Bytecode.writeint1 (Absyn.getKindArity kind);
	Bytecode.writeString (Absyn.getKindName kind)
  in
  (* local kind : arity *)
  let writeLocalKind kind =
	Bytecode.writeint1 (Absyn.getKindArity kind)
  in
  
  let writeKindsAux kinds writeKindFunc =
	let Codegen.KindList(kindList, numKinds) = kinds in
	Bytecode.writeint2 numKinds;
	map writeKindFunc kindList 
  in

  writeKindsAux gkinds writeGlobalKind;
  writeKindsAux lkinds writeLocalKind

(*****************************************************************************)
(*                  WRITING OUT TYPE SKELETON INFORMATION                    *)
(*****************************************************************************)
let writeTypeSkels tyskels =

  (* writing out the prefix representation of a type skeleton *) 
  let rec writeType tySkel =
    match tySkel with
      Absyn.ApplicationType(kind, args) ->
		Bytecode.writeint1 Bytecode.typeMarkKind; 
        writeKindIndex kind;
		Bytecode.writeint1 (List.length args);
        map writeType args
    | Absyn.ArrowType(lop, rop) ->
		Bytecode.writeint1 Bytecode.typeMarkArrow;
		map writeType [lop ; rop] 
    | _ -> (* type skeleton variable *)
		Bytecode.writeint1 Bytecode.typeMarkSkeletonVar;
		Bytecode.writeint1 (Absyn.getSkeletonVariableIndex tySkel)
  in

  let writeOneTypeSkel tySkel =
    if (Absyn.getSkeletonNew tySkel) then 
		writeType (Absyn.getSkeletonType tySkel)
    else ()
  in

  let Codegen.TypeSkeletonList(tySkelList, numTySkels) = tyskels in
  Bytecode.writeint2 numTySkels; 
  map writeOneTypeSkel tySkelList 

(*****************************************************************************)
(*                  WRITING OUT LOCAL AND GLOBAL CONSTANTS                   *)
(*****************************************************************************)
let writeFixity fixity =
  match fixity with
    Absyn.Infix      -> Bytecode.writeint1 Bytecode.fixityMarkInfix
  | Absyn.Infixl     -> Bytecode.writeint1 Bytecode.fixityMarkInfixl
  | Absyn.Infixr     -> Bytecode.writeint1 Bytecode.fixityMarkInfixr
  | Absyn.Prefix     -> Bytecode.writeint1 Bytecode.fixityMarkPrefix
  | Absyn.Prefixr    -> Bytecode.writeint1 Bytecode.fixityMarkPrefixr
  | Absyn.Postfix    -> Bytecode.writeint1 Bytecode.fixityMarkPostfix
  | Absyn.Postfixl   -> Bytecode.writeint1 Bytecode.fixityMarkPostfixl
  | Absyn.NoFixity   -> Bytecode.writeint1 Bytecode.fixityMarkNoFixity 

let writeConstInfo gconsts lconsts hconsts =
  (* global constant: fixity, precedence, type env size, name, tyskelind *)
  (* local constant: fixity, precedence, type env size, tyskelind *)
  let writeConst global const =
    writeFixity (Absyn.getConstantFixity const);
	let prec = Absyn.getConstantPrec const in
	(if prec < 0 then Bytecode.writeint1 0
	 else Bytecode.writeint1 prec);
    (* Bytecode.writeint1 (Absyn.getConstantPrec const); *)
    Bytecode.writeint1 (Absyn.getConstantTypeEnvSize false const);
    (if (global) then Bytecode.writeString (Absyn.getConstantName const)
     else ());
    Bytecode.writeint2 (Absyn.getSkeletonIndex 
			   (Absyn.getConstantSkeletonValue const))
  in

  (* hidden constant: tyskelind *)
  let writeHConst const =
    Bytecode.writeint2 (Absyn.getSkeletonIndex 
			   (Absyn.getConstantSkeletonValue const))
  in
  
  let writeConstInfoAux consts writeConstFunc =
    let Codegen.ConstantList(constList, numConsts) = consts in
    Bytecode.writeint2 numConsts;
    map writeConstFunc constList 
  in

  writeConstInfoAux gconsts (writeConst true);
  writeConstInfoAux lconsts (writeConst false);
  writeConstInfoAux hconsts writeHConst

(****************************************************************************)
(*                   WRITING OUT STRING INFORMATION                         *)
(****************************************************************************)
let writeStrings strs =
  let writeOneString str =
    if (Absyn.getStringInfoNew str) then
      Bytecode.writeString (Absyn.getStringInfoString str)
    else ()
  in

  let Codegen.StringList(strInfo, numStrs) = strs in
  Bytecode.writeint2 numStrs;
  map writeOneString strInfo 


(*****************************************************************************)
(*                WRITING OUT IMPLICATION GOAL DEFINITIONS                   *)
(*****************************************************************************)
let writeImpGoalInfo implGoals =
  let writeDef def =
	let Codegen.ImpPredInfo(pred, offset) = def in
    prerr_endline(Printf.sprintf "Def: %s<%d>"
                    (Absyn.getConstantName pred) offset);
	writeConstIndex pred;
	Bytecode.writeWord offset
  in

  let writeImpGoal impGoal = 
	let Codegen.ImpGoalCode(Codegen.PredList(extPreds, numExtPreds), 
							impPredList, numDefs) =  impGoal
	in
    prerr_endline(Printf.sprintf "NumExtPred: %d\nNumDefs: %d"
                    numExtPreds numDefs);
    
	(* [next clause table] *)
	Bytecode.writeint2 numExtPreds;
	map writeConstIndex extPreds; 
	(* <find code function> *)
	Bytecode.writeint1 Bytecode.findCodeFuncMarkHash;
	(* [search table] *)
	Bytecode.writeint2 numDefs;
	map writeDef impPredList
  in 

  let Codegen.ImpGoalList(impGoalList, numImpGoals) = implGoals in
  (* <number of implication goals> *)
  Bytecode.writeint2 numImpGoals;
  (* [implication goal info] *)
  map writeImpGoal impGoalList

(*****************************************************************************)
(*                     WRITING OUT HASH TABLE INFORMATION                    *)
(*****************************************************************************)
let writeHashTabInfo hashTabs =
  let writeHashEntry hashEntry =
	let Codegen.ConstHashTabEntry(constCat, index, codeLoc) = hashEntry in
	Bytecode.writeint1 (getConstantMark constCat);
	Bytecode.writeint2 index;
	Bytecode.writeWord codeLoc
  in

  (* [hash table] *)
  let writeHashTab hashTab =
	let Codegen.ConstHashTab(numEntries, hashChains) = hashTab in
        (* <number of entries>*)
	Bytecode.writeint2 numEntries;
        (* [hash table entries] *)
	map writeHashEntry hashChains
  in
	
  let Codegen.ConstHashTabs(hashTabList, numHashTabs) = hashTabs in
  (* <number of hash tables> *)
  Bytecode.writeint2 numHashTabs;
  (* [hash tables] *)
  map writeHashTab hashTabList	

(****************************************************************************)
(*    WRITING OUT INFORMATION ABOUT PREDICATE DEFINITIONS IN THIS MODULE    *)
(****************************************************************************)
let writeModDefsInfo nonExpDefs expDefs localDefs defs = 
  let writePredTabs predlist =
	let Codegen.PredList(predNames, numPreds) = predlist in
        (* <number of predicates> *)
	Bytecode.writeint2 numPreds;
        (* [constant indexes]*)
	map writeConstIndex predNames
  in

  let writeSearchTabEntry pred =
	writeConstIndex pred;
	Bytecode.writeWord (Absyn.getConstantCodeInfoClausesIndex pred)
  in

  let writeSearchTab defs =
    let Codegen.PredList(predNames, numPreds) = defs in
	Bytecode.writeint2 numPreds;
	map writeSearchTabEntry predNames
  in

  (* [next clause table] *)
  writePredTabs nonExpDefs;
  (* [exportdef predicate table] *)
  writePredTabs expDefs;
  (* [local predicate table] *)
  writePredTabs localDefs;
  (* <find code function> *)
  Bytecode.writeint1 Bytecode.findCodeFuncMarkHash;
  (* [search table] *)
  writeSearchTab defs

(*****************************************************************************)
(*    WRITING OUT RENAMING INFOR FOR IMPORTED OR ACCUMULATED MODULES         *)
(*****************************************************************************)
let writeRenamingInfo renamingList = 

  let writeRenamingInfoOneMod renaming =
	let writeRenamingKinds kinds =
          let writeRenamingKind kind =
	    Bytecode.writeString (Absyn.getKindName kind);
	    writeKindIndex kind
	  in

	  let Codegen.KindList(kindList, numKinds) = kinds in
	  Bytecode.writeint2 numKinds;
	  map writeRenamingKind kindList
	in
	
	let writeRenamingConsts consts =
	  let writeRenamingConst const =
	    Bytecode.writeString (Absyn.getConstantName const);
	    writeConstIndex const;
	  in

	  let Codegen.ConstantList(constList, numConsts) = consts in
	  Bytecode.writeint2 numConsts;
	  map writeRenamingConst constList
	in
	
	let Codegen.RenamingInfo(modName, kinds, consts) = renaming in
        (* <module name>*)
	Bytecode.writeString modName;
        (* [kind renaming functions] *)
	writeRenamingKinds kinds;
        (* [constant renaming functions] *)
	writeRenamingConsts consts
  in

  let modNumber = List.length renamingList in
  (* <number of acc/imp modules>*)
  Bytecode.writeint1 modNumber;
  (* [renaming functions] *)
  map writeRenamingInfoOneMod renamingList 


(***************************************************************************)
(*                    WRITING OUT INSTRUCTIONS                             *)
(***************************************************************************)
let writeInstructions code =
  map Instr.writeInstruction code

(************************************************************************)
(*                        INTERFACE FUNCTION                            *)
(************************************************************************)
let writeByteCode cgModule =
  match cgModule with
    Codegen.Module(modName, gkinds, lkinds, gconsts, lconsts, hconsts, defs, 
				   nonExpDefs, expDefs, localDefs, tySkels, strs, impRenaming,
				   accRenaming, instrs, hashTabs, implGoals) ->
	  let Codegen.Instructions(code, codeSize) = instrs in
      (* [module header] *)
      writeHeader modName codeSize;
      (* [global kinds] *)
      (* [local kinds] *)
      writeKindInfo gkinds lkinds;
      (* [type skeletons] *)
      writeTypeSkels tySkels;
      (* [global constants] *)
      (* [local constants] *)
      (* [hidden constants] *)
      writeConstInfo gconsts lconsts hconsts;
      (* [strings] *)
      writeStrings strs;
      (* [implication goal tables] *)
      writeImpGoalInfo implGoals;
      (* [hash tables] *)
      writeHashTabInfo hashTabs;
      (* [bound variable tables] *)
	  Bytecode.writeint2 0;
      (* [import table] *)
	  writeModDefsInfo nonExpDefs expDefs localDefs defs;
      (* [accumulated modules] *)
	  writeRenamingInfo accRenaming;
      (* [imported modules] *)
	  writeRenamingInfo impRenaming;
      (* [instructions] *)
      writeInstructions code
	   
	   
let writeQueryByteCode cgModule =
  match cgModule with
    Codegen.Module(modName, gkinds, lkinds, gconsts, lconsts, hconsts, defs, 
				   nonExpDefs, expDefs, localDefs, tySkels, strs, impRenaming,
				   accRenaming, instrs, hashTabs, implGoals) ->
    let Codegen.Instructions(code,codeSize) = instrs in
    (* [code size] *)
    prerr_endline ("Writing Code size: "^(string_of_int codeSize));
    writeCodeSize codeSize;
    (* [type skeletons] *)
    let numtyskels = Codegen.getNumCGTypeSkeletons tySkels in
    prerr_endline(Printf.sprintf "Num Type Skels: %d" numtyskels);
    (* writeTypeSkels tySkels; *)
    (* [global constants] *)
    (* [local constants] *)
    (* [hidden constants] *)
    assert(Codegen.getNumCGConsts gconsts = 0
           && Codegen.getNumCGConsts lconsts = 0);
    (* writeConstInfo gconsts lconsts hconsts; *)
    (* [strings] *)
    writeStrings strs;
    (* [implication goal tables] *)
    writeImpGoalInfo implGoals;
    (* [hash tables] *)
    writeHashTabInfo hashTabs;
    (* [bound variable tables] *)
    Bytecode.writeint2 0;
    (* [instructions] *)
    writeInstructions code
