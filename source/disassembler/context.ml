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
(* implication tables                                                      *)
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
(* Display functions:                                                      *)
(***************************************************************************)
(* print line *)
let printLine str =
  print_string str;
  print_newline ()

(* print predicate table *)
let printPredicateTable preds header =
  let collectPreds preds pred = preds ^ "   " ^ pred ^ "\n" in

  let numPreds = List.length preds in
  printLine (header ^ (string_of_int numPreds));
  if (numPreds = 0) then ()
  else
	print_string 
	  (List.fold_left collectPreds "" 
		 (List.map Bytecode.displayC preds))

(**************************************************************)
(*  GLOBAL/LOCAL KINDS                                        *)
(**************************************************************)
let printGlobalKind kind =
  ((Absyn.getKindName kind) ^ "/" ^ (string_of_int (Absyn.getKindArity kind)))
									 
let printLocalKind kind =
  ("/" ^ (string_of_int (Absyn.getKindArity kind)))

let printKinds kinds header printKindFunc =
  let length = Array.length kinds in

  let rec printKindsAux index =
	if (index = length) then ()
	else
	  (printLine ((string_of_int index) ^ ": " ^ 
				  (printKindFunc (Option.get (Array.get kinds index))));
	   printKindsAux (index + 1))
  in
  
  print_newline ();
  printLine header;
  printKindsAux 0 
  
(***************************************************************)
(*           TYPE SKELETONS                                    *)
(***************************************************************)
let printTypeSkels tyskels =

  let rec printTypeSkelsAux tyskels index =
	match tyskels with
	  [] -> ()
	| (tyskel :: rest) -> 
		(printLine ((string_of_int index) ^ ": " ^ 
					(Absyn.string_of_type tyskel));
		 printTypeSkelsAux rest (index + 1))
  in
  
  print_newline ();
  printLine "Type skeleton table:";
  printTypeSkelsAux tyskels 0

(***************************************************************)
(*  GLOBAL/LOCAL/HIDDEN CONSTANTS                              *)
(***************************************************************)
let printGlobalConst c = 
  ((Absyn.getConstantName c) ^ " (" ^ 
   (Absyn.string_of_fixity (Absyn.getConstantFixity c)) ^ 
   ", precedence " ^ (string_of_int (Absyn.getConstantPrec c)) ^
   ")\n    Env Size: " ^ (string_of_int (Absyn.getConstantTypeEnvSize false c)) 
  ^ ", Type Skeleton: #" ^ 
   (string_of_int (Absyn.getSkeletonIndex (Absyn.getConstantSkeletonValue c))))

let printLocalConst c =	
  (" (" ^ (Absyn.string_of_fixity (Absyn.getConstantFixity c)) ^ 
   ", precedence " ^ (string_of_int (Absyn.getConstantPrec c)) ^
   ")\n    Env Size: " ^ 
   (string_of_int (Absyn.getConstantTypeEnvSize false c)) ^
   ", Type Skeleton: #" ^ 
   (string_of_int (Absyn.getSkeletonIndex (Absyn.getConstantSkeletonValue c))))

let printHiddenConst c =
  (" Type Skeleton: #" ^ 
   (string_of_int (Absyn.getSkeletonIndex (Absyn.getConstantSkeletonValue c))))

	
let printConsts consts header printConstFunc =
  let length = Array.length consts in

  let rec printConstsAux index =
	if (index = length) then ()
	else
	  (printLine ((string_of_int index) ^ ": " ^ 
				  (printConstFunc (Option.get (Array.get consts index))));
	   printConstsAux (index + 1))
  in
  
  print_newline ();
  printLine header;
  printConstsAux 0 
  
(***************************************************************)
(*                     STRINGS 		                           *)
(***************************************************************)
let printStrings strList =
  let rec printStringAux strList index =
	match strList with
	  [] -> ()
	| (str :: rest)	->
		printLine ((string_of_int index) ^ ": " ^ str);
		printStringAux rest (index + 1)
  in
  
  print_newline ();
  printLine "String table:";
  printStringAux strList 0

(***************************************************************)
(*               IMPLICATION TABLES 		                   *)
(***************************************************************)
let printImplTables implTabs =

  let printImplTable = function
	  (nextClauses, findCodefn, searchTab) ->
		printPredicateTable nextClauses 
		  "  Predicate definitions possibly extending previous ones: ";
		printLine ("  Find function type: " ^ 
				   (Bytecode.displayFindCodeFn findCodefn));
		printPredicateTable searchTab "  In-core table size: "
  in
  
  let rec printImplTablesAux implTabs ind =
	match implTabs with
	  [] -> ()
	| (implTab :: rest) ->
		printLine ((string_of_int ind) ^ ":");
		printImplTable implTab;
		printImplTablesAux rest (ind + 1)

  in
  
  print_newline ();
  printLine "Implication Tables:";
  printImplTablesAux implTabs 0
  
(***************************************************************)
(*               HASH TABLES 		                           *)
(***************************************************************)
let printHashTables hashTabs =

  let printHashTable hashTab =
	let tabSize = List.length hashTab in
	let rec printHashTableEntries hashTab =
	  match hashTab with
		[] -> ()
	  | ((c, label)::rest) -> 
		  printLine ("    " ^ (Bytecode.displayC c) ^ " -> " ^ label);
		  printHashTableEntries rest
	in

	printLine ("    Table size: "^ (string_of_int tabSize));
	printLine "    Constants:";
	printHashTableEntries hashTab 
  in
  
  let numberHashTabs = Array.length hashTabs in
  let rec printHashTablesAux index =
	if (index = numberHashTabs) then ()
	else
	  (printLine ((string_of_int index) ^ ":");
	   printHashTable (Array.get hashTabs index);
	   printHashTablesAux (index + 1))
  in

  print_newline ();
  printLine "Hash tables:";
  printHashTablesAux 0

(***************************************************************)
(*               MODULE TABLE  		                           *)
(***************************************************************)
let rec printModuleTable table =
  match table with
	ModuleTable(nextClauses, exportDefs, localPreds, findCodefn, searchTab) ->
	  print_newline ();
	  printLine "Module table:";
	  printPredicateTable nextClauses 
		"  Predicate definitions possibly extending previous ones: ";
	  printPredicateTable exportDefs "  Exportdef predicates: ";
	  printPredicateTable localPreds "  Local predicates: ";
	  printLine ("  Find function type: " ^ 
				 (Bytecode.displayFindCodeFn findCodefn));
	  printPredicateTable searchTab "  In-core table size: "
	  
  | ImportTables(importTabs) ->
	  print_newline ();
	  printLine "Import tables:";
	  List.iter printImportTable importTabs 

and printImportTable = function 
	(segNum, nextClauses, localConsts, findCodefn, searchTab) ->
	  print_newline ();
	  printLine "  Import table:";
	  printLine ("    number of code segments: " ^ string_of_int segNum); 
	  printPredicateTable nextClauses "    Next clause table: ";
	  printPredicateTable localConsts "    Local constant table: ";
	  printLine ("    Find function type: " ^ 
				 (Bytecode.displayFindCodeFn findCodefn));
	  printPredicateTable searchTab "    Search table: "

(***************************************************************)
(*               RENAMING TABLES  		                       *)
(***************************************************************)
let printRenamingTables renamings header =

  let rec printRenamingList renamings printFn =
	match renamings with
	  [] -> ()
	| ((from, toInfo) :: rest) ->
		printLine ("    " ^ from ^  " -> " ^(printFn toInfo));
		printRenamingList rest printFn
  in

  let rec printRenamingTablesAux renamings index =
	match renamings with
	  [] -> ()
	| ((modname, kindRenamings, constRenamings) :: rest) ->
		printLine ((string_of_int index) ^ ": " ^ modname);
		printLine "    Kind renamings:";
		printRenamingList kindRenamings Bytecode.displayK;
		printLine "    Constant renamings:";
		printRenamingList constRenamings Bytecode.displayC;
		printRenamingTablesAux rest (index + 1)
  in

  print_newline ();
  printLine header;
  printRenamingTablesAux renamings 0

(***************************************************************)
(*               INSTRUCTIONS     		                       *)
(***************************************************************)
let printInstructions instructions =
  let printLabel label =
	let numberWS = 20 - String.length label in
	let rec mkWS number =
	  if number = numberWS then ""
	  else (" " ^ (mkWS (number + 1)))
	in
	if (numberWS > 0) then label ^ (mkWS 0)
	else label
  in
  
  let rec printInstructionsAux insts pos =
	match insts with
	  [] -> ()
	| (inst :: rest) -> 
		let (inst, size) = Instr.displayInstruction inst in
		let label = Label.findLabel pos in
		printLine ((printLabel label) ^ inst);
		printInstructionsAux rest (pos + size)
  in
  
  print_newline ();
  printLine "LABEL               INSTRUCTION              OPERANDS";
  print_newline ();
  Bytecode.setFindLabelFn (Label.findLabel);
  printInstructionsAux instructions 0
		  
(***************************************************************************)
(* Displaying module context                                               *)
(***************************************************************************)
let displayModContext modContext tableOnly instrOnly =
  match modContext with
	ModContext(filename, bcversion, modname, codesize, gkinds, lkinds, tyskels,
			   gconsts, lconsts, hconsts, strings, impltabs, hashtabs, 
			   moduletable, renamingacc, renamingimp, instructions) 
	->
	  printLine ("Disassembling from bytecode file: " ^ filename);
	  printLine ("Bytecode version: " ^ (string_of_int bcversion));
	  printLine ("Module name: " ^ modname);
	  (*printLine ("Bytes needed for code: " ^ (string_of_int codesize));*)
	  (if (not tableOnly) then
		printInstructions instructions
	  else ());
	  (if (not instrOnly) then 
		(printKinds gkinds "Global kind table:" printGlobalKind;
		 printKinds lkinds "Local kind table:" printLocalKind;
		 printTypeSkels tyskels;
		 printConsts gconsts "Global constant table: " printGlobalConst;
		 printConsts lconsts "Local constant table: " printLocalConst;
		 printConsts hconsts "Hidden constant table: " printHiddenConst;
		 printStrings strings;
		 printImplTables impltabs;
		 printHashTables hashtabs;
		 printModuleTable moduletable;
		 printRenamingTables renamingacc "Accumulated tables:";
		 printRenamingTables renamingimp "Imported tables:"
		) 
	  else ())
		 

