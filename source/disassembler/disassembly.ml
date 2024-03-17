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
(* The functions in this module are in charge of disassembling a bytecode  *)
(* file.                                                                   *)
(***************************************************************************)

(***************************************************************************)
(* get a kind/constant from its index and category information             *)
(***************************************************************************)
let getKind gkinds lkinds cat index =
	if cat = Bytecode.global then
	  Array.get gkinds index
	else if cat = Bytecode.local then
	  Array.get lkinds index
	else if cat = Bytecode.pervasive then
	  Pervasiveutils.findKindIndexMapping index
	else
	  (Errormsg.error Errormsg.none 
		 "disassembleTypeSkeleton: invalid type skeleton in bytecode";
	   None)
  
let getConstant gconsts lconsts hconsts cat index =
  if cat = Bytecode.global then
	Array.get gconsts index
  else if cat = Bytecode.local then
	Array.get lconsts index
  else if cat = Bytecode.hidden then
	Array.get hconsts index
  else (* pervasive *)
	Pervasiveutils.findConstantIndexMapping index

(***************************************************************************)
(* disassemble clause tables                                               *)
(***************************************************************************)
let disassembleClauseTable gconsts lconsts hconsts =
  let numberClauses = Bytecode.readTwoBytes () in  
  
  let rec disassembleClauseTableAux preds index =
	if (index = numberClauses) then List.rev preds
	else
	  let pred = 
		Option.get 
		  (Bytecode.readConstantIndex (getConstant gconsts lconsts hconsts))
	  in
   	  disassembleClauseTableAux (pred :: preds) (index + 1)
  in
  
  disassembleClauseTableAux [] 0

(***************************************************************************)
(* disassemble search tables                                               *)
(***************************************************************************)
let disassembleSearchTable gconsts lconsts hconsts =
  let numberEntries = Bytecode.readTwoBytes () in
  
  let rec disassembleSearchTableAux preds index =
	if (index = numberEntries) then List.rev preds
	else
	  let pred =
		Option.get
		  (Bytecode.readConstantIndex (getConstant gconsts lconsts hconsts))
	  in
	  let offset = Bytecode.readWord() in
	  Label.assignLabel offset (Bytecode.displayC pred);
	  disassembleSearchTableAux (pred :: preds) (index + 1)
  in
  
  disassembleSearchTableAux [] 0

(***************************************************************************)
(* disassemble hash tables                                                 *)
(***************************************************************************)
let disassembleHashTable gconsts lconsts hconsts =
  let numberEntries = Bytecode.readTwoBytes () in
 
  let rec disassembleHashTableAux hashTab index =
	if (index = numberEntries) then List.rev hashTab
	else
	  let pred =
		Option.get
		  (Bytecode.readConstantIndex (getConstant gconsts lconsts hconsts))
	  in
	  
	  let label = Label.label(Bytecode.readWord ()) in
	  disassembleHashTableAux ((pred, label)::hashTab) (index + 1)
  in

  let tab = disassembleHashTableAux [] 0 in
  tab
		
(***************************************************************************)
(*                          HEADER INFORMATION                             *)
(***************************************************************************)
let disassembleHeaderInfo () =
  (* Extract the bytecode version number, mod name, and code size *)
  let codeVer = Bytecode.readWord() in
  let modName = Bytecode.readString () in
  let codeSize = Bytecode.readWord () in
    (codeVer, modName, codeSize)

(***************************************************************************)
(*                     GLOBAL/LOCAL KIND INFORMATION                       *)
(***************************************************************************)
let disassembleKinds makeKindFn =
  let length = (Bytecode.readTwoBytes ()) in
  let kinds  = Array.make length None in

  let rec disassembleKindsAux ind =
	if (ind = length) then ()
	else
	  (Array.set kinds ind (Some (makeKindFn ind));
	   disassembleKindsAux (ind + 1))
  in
  disassembleKindsAux 0;
  kinds
	  
(***************************************************************************)
(*                    TYPE SKELETON INFORMATION                            *)
(***************************************************************************)
let disassembleTypeSkeletons gkinds lkinds  =

  let numTySkels = Bytecode.readTwoBytes () in
  
  let rec disassembleTypeSkeletonsAux ind tyskels =
	if (ind = numTySkels) then (List.rev tyskels)
	else
	  let tyskel = Bytecode.readTypeSkeleton (getKind gkinds lkinds) in
	  disassembleTypeSkeletonsAux (ind + 1) (tyskel :: tyskels)
  in
  disassembleTypeSkeletonsAux 0 []

(***************************************************************************)
(*         GLOBAL/LOCAL/HIDDEN CONSTANT INFORMATION                        *)
(***************************************************************************)
let disassembleConstants makeConstFn =

  let length = (Bytecode.readTwoBytes ()) in
  let consts = Array.make length None in
  
  (* decide how to fill in type skeleton of the constant *)
  let getConstTypeSkel ind =
	Absyn.Skeleton(Absyn.ErrorType, ref (Some ind), ref false)	
  in

  let rec disassembleConstantsAux ind =
	if (ind = length) then ()
	else
	  (Array.set consts ind (Some (makeConstFn getConstTypeSkel ind));
	   disassembleConstantsAux (ind + 1))
  in
  disassembleConstantsAux 0;
  consts

(***************************************************************************)
(*                      STRING INFORMATION                                 *)
(***************************************************************************)
let disassembleStrings () =
  let numberStrings = Bytecode.readTwoBytes () in

  let rec disassembleStringsAux number strs =
	if number = numberStrings then List.rev strs
	else
	  let str = Bytecode.readString () in
	  disassembleStringsAux (number + 1) (str :: strs)
  in
  disassembleStringsAux 0 []  

(***************************************************************************)
(*               IMPLICATION TABLE INFORMATION                             *)
(***************************************************************************)
let disassembleImplTabs gconsts lconsts hconsts =
  let numberTables = Bytecode.readTwoBytes () in

  let rec disassembleImplTabsAux implTabs index =
	if (index = numberTables) then List.rev implTabs
	else
	  let nextClauses = disassembleClauseTable gconsts lconsts hconsts in
	  let findCodefn  = Bytecode.readFindCodeFn  () in
	  let searchTab   = disassembleSearchTable gconsts lconsts hconsts in
	  disassembleImplTabsAux ((nextClauses, findCodefn, searchTab)::implTabs)
		(index + 1)
  in
  disassembleImplTabsAux [] 0

(***************************************************************************)
(*                      HASH TABLE INFORMATION                             *)
(***************************************************************************)
let disassembleHashTabs gconsts lconsts hconsts =
  let numberHashTabs = Bytecode.readTwoBytes () in
  let hashTabs       = Array.make numberHashTabs [] in
  
  let rec disassembleHashTabsAux index =
	if (index = numberHashTabs) then ()
	else
	  let hashTab = disassembleHashTable gconsts lconsts hconsts in
	  Array.set hashTabs index hashTab;
	  disassembleHashTabsAux (index + 1)
  in
  disassembleHashTabsAux 0;
  hashTabs
  
(***************************************************************************)
(*                      MODULE TABLE                                       *)
(***************************************************************************)
let disassembleModuleTable gconsts lconsts hconsts =
  let nextClauseTab = disassembleClauseTable gconsts lconsts hconsts in
  let exportdefTab  = disassembleClauseTable gconsts lconsts hconsts in
  let localpredTab  = disassembleClauseTable gconsts lconsts hconsts in
  let findCodefn    = Bytecode.readFindCodeFn () in
  let searchTab     = disassembleSearchTable gconsts lconsts hconsts in
  Context.ModuleTable(nextClauseTab, exportdefTab, localpredTab, findCodefn, searchTab)

(***************************************************************************)
(*                      IMPORT TABLES                                      *)
(***************************************************************************)
let disassembleImportTables gconsts lconsts hconsts =
  let numberImportTabs = Bytecode.readTwoBytes () in
  
  let rec disassembleImportTablesAux index importTabs =
	if (index = numberImportTabs) then List.rev importTabs
	else
	  let segNum        = Bytecode.readOneByte () in
	  let nextClauseTab = disassembleClauseTable gconsts lconsts hconsts in
	  let localConsts   = disassembleClauseTable gconsts lconsts hconsts in
	  let findCodefn    = Bytecode.readFindCodeFn () in
	  let searchTab     = disassembleSearchTable gconsts lconsts hconsts in
	  disassembleImportTablesAux (index + 1) 
		((segNum, nextClauseTab, localConsts, findCodefn, searchTab) :: importTabs)
  in

  let importTabs = disassembleImportTablesAux 0 [] in
  Context.ImportTables(importTabs)

(***************************************************************************)
(*                      RENAMING TABLES                                    *)
(***************************************************************************)
let disassembleRenamingTables gkinds lkinds gconsts lconsts hconsts =
  let numberRenamingTabs = Bytecode.readOneByte () in

  let rec disassembleRenamingsAux index renamingTabs =
	if (index = numberRenamingTabs) then List.rev renamingTabs
	else
	  let modname = Bytecode.readString () in
	  let numKindRenamings = Bytecode.readTwoBytes () in

	  let rec disassembleKindRenamings index renamings =
		if (index = numKindRenamings) then (List.rev renamings)
		else
		  let from = Bytecode.readString () in
		  let toInfo = 
			Option.get (Bytecode.readKindIndex (getKind gkinds lkinds))
		  in
		  disassembleKindRenamings (index + 1) ((from, toInfo) :: renamings)
	  in
	  
	  let kindRenamings = disassembleKindRenamings 0 [] in
	  let numConstRenamings = Bytecode.readTwoBytes () in

	  
	  let rec disassembleConstRenamings index renamings =
		if (index = numConstRenamings) then (List.rev renamings)
		else
		  let from = Bytecode.readString () in
		  let toInfo =
			Option.get (Bytecode.readConstantIndex 
						  (getConstant gconsts lconsts hconsts))
		  in
		  disassembleConstRenamings (index + 1) ((from, toInfo) :: renamings)
	  in

	  let constRenamings = disassembleConstRenamings 0 [] in
	  disassembleRenamingsAux (index + 1)
		((modname, kindRenamings, constRenamings) :: renamingTabs)
  in

  disassembleRenamingsAux 0 []

(****************************************************************************)
(*                        INSTRUCTIONS                                      *)
(****************************************************************************)
let disassembleInstructions gkinds lkinds gconsts lconsts hconsts codeSize =
  let rec disassembleInstructionsAux pos insts =
	if (pos = codeSize) then List.rev insts
	else
	  let (inst, size) = 
		Instr.readInstruction (getKind gkinds lkinds) 
		  (getConstant gconsts lconsts hconsts) 
	  in 
	  disassembleInstructionsAux (pos + size) (inst :: insts)
  in
  Bytecode.setGetLabelFn (Label.addLabel);
  disassembleInstructionsAux 0 []

(***************************************************************************)
(*                        INTERFACE FUNCTION                               *)
(***************************************************************************)
let disassemble filename tableOnly instrOnly =
  Bytecode.openInChannel filename;
  let (codeVer, modName, codeSize) = disassembleHeaderInfo () in
  let linkedCode =
    if codeVer = Bytecode.linkedByteCodeVersionNumber then true
    else if codeVer = Bytecode.byteCodeVersionNumber then false
    else (Errormsg.error Errormsg.none
		    ("Disassembler: unknown bytecode version <" ^
               (string_of_int codeVer) ^ ">"); true)
  in
  if !Errormsg.anyErrors then 1
  else
	begin
    let () = if linkedCode then ignore (Bytecode.readTwoBytes ()) in
	let gKinds = disassembleKinds Bytecode.readGlobalKind     in
	let lKinds = disassembleKinds Bytecode.readLocalKind      in
	let tySkels = disassembleTypeSkeletons gKinds lKinds      in
	if !Errormsg.anyErrors then 1
	else
      begin
      let () = if linkedCode then ignore (Bytecode.readTwoBytes ()) in
	  let gConsts = disassembleConstants Bytecode.readGlobalConstant in
	  let lConsts = disassembleConstants Bytecode.readLocalConstant  in
	  let hConsts = disassembleConstants Bytecode.readHiddenConstant in
	  let strings = disassembleStrings () in
	  let impltabs = disassembleImplTabs gConsts lConsts hConsts in
	  let hashtabs = disassembleHashTabs gConsts lConsts hConsts in
	  let _ = Bytecode.readTwoBytes () in (* skip bv table *)
	  let moduletab = 
		if linkedCode then disassembleImportTables gConsts lConsts hConsts 
		else disassembleModuleTable gConsts lConsts hConsts 
	  in
	  let accRenamings = 
		if linkedCode then []
		else disassembleRenamingTables gKinds lKinds gConsts lConsts hConsts 
	  in
	  let impRenamings =
		if linkedCode then []
		else disassembleRenamingTables gKinds lKinds gConsts lConsts hConsts 
	  in
	  let instructions =
		disassembleInstructions gKinds lKinds gConsts lConsts hConsts codeSize
	  in
	  let context =
		Context.ModContext(filename, codeVer, modName,
						   codeSize, gKinds, lKinds, tySkels, gConsts, lConsts,
						   hConsts, strings, impltabs, hashtabs, moduletab,
						   accRenamings, impRenamings, instructions)
	  in
	    Context.displayModContext context tableOnly instrOnly;
	    Bytecode.closeInChannel ();
	    0
      end
    end

