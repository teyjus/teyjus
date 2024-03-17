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
(*                           CODE GENERATION                               *)
(* Functions and data structures are defined here for preparing all        *)
(* information gathered from a module for dumping out into a bytecode file.*)
(* 1). Indexes are assigned to different categories of kind and constant   *)
(*     symbols, type skeletons and strings.                                *)
(* 2). Imported and accumulated module lists are mapped into lists and all *)
(*     information needed for dumping out the renaming functions is stored *)
(*     in these lists.                                                     *)
(* 3). Predicate definitions are processed and transformed into instructions*)
(*     together with the size of code space. (The functions in this module *)
(*     are in charge of generating switching and indexing code and in this *)
(*     process, hash tables are formed and stored, ready for dumping.      *)
(*     Clause code is gethered by invocation of functions from             *)
(*     the module Clausegen. Back patching is performed over call and      *)
(*     execute instructions after offsets of all clause defintions of this *)
(*     module become available.)                                           *)
(* 4). Antecedents of implication goals are processed, definitions are     *)
(*     translated and information for generating the dump for such tables  *)
(*     is left in a list.                                                  *)
(***************************************************************************)


(****************************************************************************)
(*   DATA STRUCTURES FOR RECORDING INFORMATION NEEDED FOR SPITTING CODE     *)
(****************************************************************************)

(**************************************************************************)
(* global kinds and local kinds                                           *)
(*========================================================================*)
(* kinds lists: (kinds list, number of kinds)                             *)
(**************************************************************************)
type cgkinds =  KindList of Absyn.akind list * int


(**************************************************************************)
(* global, local and hidden constants                                     *)
(*========================================================================*)
(* constants lists: (constants list, number of constants)                 *)
(**************************************************************************)
type cgconsts = ConstantList of Absyn.aconstant list * int


(**************************************************************************)
(* type skeletons                                                         *)
(*========================================================================*)
(* type skeletons list: (type skeletons, number of type skeletons)        *)
(**************************************************************************)
type cgtypeskeletons = TypeSkeletonList of Absyn.askeleton list * int


(**************************************************************************)
(* strings                                                                *)
(*========================================================================*)
(* string list: (strings list, number of strings)                         *)
(**************************************************************************)
type cgstrings = StringList of Absyn.astringinfo list * int


(**************************************************************************)
(* predicates defined in this module;                                     *)
(* global, non-exportdef predicates in this module (predicates whose      *)
(* previous defintions could be extended by this module);                 *)
(* (global) exportdef predicates in this module;                          *)
(* local predicates in this module;                                       *)
(*========================================================================*)
(* predicates list: (predicates (names) list, number of predicates)       *)
(**************************************************************************)
type cgpreds = PredList of Absyn.aconstant list * int


(**************************************************************************)
(* renaming information of imported and accumulated modules               *)
(*========================================================================*)
(* renaming lists: (modname, kinds renaming, constant renaming            *)
(**************************************************************************)
type cgrenaming = RenamingInfo of string * cgkinds * cgconsts


(**************************************************************************)
(* predicate instructions                                                 *)
(*========================================================================*)
(* instruction list: (instructions, total number of bytes)                *)
(**************************************************************************)
type cginstructions = Instructions of Instr.instruction list * int     


(**************************************************************************)
(* hash tables:                                                           *)
(*========================================================================*)
(* hash tables list: (hash table list, number hash tables)                *)
(**************************************************************************)
type cghashtabs = ConstHashTabs of (cghashtab list * int)
(* hash table structure for indexing on constants: (size, tab entries)    *)
and  cghashtab = ConstHashTab of int * (cghashtabentry list)
(* hash table entry: (const category, index, code location )              *)
and  cghashtabentry = ConstHashTabEntry of Absyn.aconstanttype * int * int


(**************************************************************************)
(* information for implication tables:                                    *)
(*========================================================================*)
(* implication goals list : (implication goal list , number of imp goals) *)
(**************************************************************************)
type cgimpgoallist = ImpGoalList of (cgimpgoalcode list * int)
(* information for each implication goal:                                 *) 
(*  (extending preds, preds in antecedent, number of preds in antecendent *)
and  cgimpgoalcode = ImpGoalCode of (cgpreds * cgimppredinfo list * int)
(* predicates defined in an implication goal: (predicate, offset)         *)
and  cgimppredinfo = ImpPredInfo of (Absyn.aconstant * int )


(**************************************************************************)
(* module:                                                                *)
(*========================================================================*)
(* (module name,                                                          *)
(*  global kinds,                                                         *)
(*  local kinds,                                                          *)
(*  global constants,                                                     *)
(*  local constants,                                                      *)
(*  hidden constants,                                                     *)
(*  predicates defined in this module,                                    *)
(*  global, non-exportdef predicates in this module (predicate whose      *)
(*  previous definitions could be extended by this module),               *)
(*  (global) exportdef predicates in this module,                         *)
(*  local predicates in this module,                                      *)
(*  type skeletons,                                                       *)
(*  strings,                                                              *)
(*  imported modules renaming info,                                       *)
(*  accumulated modules renaming info,                                    *)
(*  instructions,                                                         *)
(*  hash tables for constant indexing,                                    *)
(*  implication goal list                                                 *)
(* )                                                                      *)
(**************************************************************************)   
type cgmodule =
	Module of string * cgkinds * cgkinds * cgconsts * cgconsts * 
	    cgconsts * cgpreds * cgpreds * cgpreds * cgpreds * 
		cgtypeskeletons * cgstrings * cgrenaming list * cgrenaming list * 
		cginstructions * cghashtabs * cgimpgoallist 

val getCGModuleName : cgmodule -> string
(* val getCGModuleInstructions : cgmodule -> cginstructions
 * val getCGModuleLocalConstants : cgmodule -> cgconsts
 * val getCGModuleGlobalConstants : cgmodule -> cgconsts
 * val getCGModuleHiddenConstants : cgmodule -> cgconsts
 * val getCGModuleStrings : cgmodule -> cgstrings
 * val getCGModuleImpGoalList : cgmodule -> cgimpgoallist
 * val getCGModuleHashTables : cgmodule -> cghashtabs *)

val getNumCGConsts : cgconsts -> int
val getNumCGTypeSkeletons : cgtypeskeletons -> int
val getNumCGPreds : cgpreds -> int
  
val set_main_pred : string -> unit
val get_main_pred_loc : unit -> int


(*****************************************************************************)
(*                CODE GENERATION FOR A MODULE                               *)
(*****************************************************************************)
val generateModuleCode : Absyn.amodule -> cgmodule 




