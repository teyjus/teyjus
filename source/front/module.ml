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
(************************************************************************)
(*                     global module table                              *)
(************************************************************************)
let path = ref "./"
let setPath p = 
  (path := p;
   Simerrors.handleSimExceptions (Ccode_stubs.setPath p))
   

(** **************************************************************** **)
(**               module table entry                                 **)
(** **************************************************************** **)
(* entry structure  *)
type moduletableentry = ModuleTableEntry of (string * Absyn.amodule) 

(* access functions *)
let getModuleTableModuleName = function ModuleTableEntry(name, _) -> name
let getModuleTableModule =     function ModuleTableEntry(_, amod) -> amod
let makeModuleTableEntry amod =
  let name = Absyn.getModuleName amod in
  ModuleTableEntry(name, amod)

(** **************************************************************** **)
(**                     special modules                              **)
(** **************************************************************** **)
let topModuleName = ""

(* top module  *)
let topModule = 
  ModuleTableEntry(topModuleName, 
		   Absyn.Module(topModuleName, [], [],
				ref Pervasive.pervasiveConstants,
				ref Pervasive.pervasiveKinds, 
				Pervasive.pervasiveTypeAbbrevs,
				[], [], [], [], [], ref [], [], ref [], 
				ref (Absyn.ClauseBlocks []))) 
(* error module *)
let errorModule = ModuleTableEntry("%empty", Absyn.ErrorModule)


(** **************************************************************** **)
(**               system module table                                **)
(** **************************************************************** **)
(* the size of the module table                                        *)
let numberofModules = 256 (* agree with that in system/memory.h *)

(* the module table: initialized to all empty                          *)
let moduleTables = Array.make numberofModules errorModule 

(* try to find the smallest index of a free entry in the module table; *)
(* error index -1 is returned if the module table is full.             *)
let getFirstFreeEntry () =
  let rec firstFreeEntryAux ind =
	if (ind = numberofModules) then -1 
	else
	  if (Array.get moduleTables ind) = errorModule then ind
	  else firstFreeEntryAux (ind + 1)
  in
  firstFreeEntryAux 0
  
(* enter a module into the ith entry of the global module table *)
let enterModuleTable amod index =
  Array.set moduleTables index (makeModuleTableEntry amod)

(* look for a module of given name in the current module table and     *)
(* return its abstract syntax if found; otherwise Absyn.ErrorModule    *)
(* is returned.                                                        *)
let findModule modName =
  let rec findModuleAux ind =
	if (ind = numberofModules) then (-1, Absyn.ErrorModule)
	else
	  let ModuleTableEntry(name, amod) = Array.get moduleTables ind in
	  if name = modName then (ind, amod)
	  else findModuleAux (ind + 1)
  in
  findModuleAux 0


(* asking for a slot in the global module table:                       *)
(* if a module of the same name is in the table, the index of this     *)
(* module will be returned; otherwise, the index of the first free     *)
(* entry will be returned; otherwise the module table is full and an   *)
(* error index -1 will be returned.                                    *)
let getModuleTableIndex modName =
  let (index, _) = findModule modName in
  if (index = -1) then getFirstFreeEntry ()
  else index 


(***************************************************************************)
(*                       current module                                    *)
(***************************************************************************)
let currentModule = ref Absyn.ErrorModule

let setCurrentModule modtab = currentModule := modtab
let getCurrentModule ()     = !currentModule


(***************************************************************************)
(*                       load a module                                     *)
(* 1. find an index for the module in the global module tables (OCaml and  *)
(*    C);                                                                  *)
(* 2. invoke the C function to link and load the module into the simulator *)
(*    memory;                                                              *)
(* 3. load the ocaml module.                                               *)
(***************************************************************************)
(* top module: nothing needs to be loaded *)
let loadTopModule ()   = ()

(* specified module *)
let loadModule modName = 
  (* asking for a slot from the global module table *)
  let index = getModuleTableIndex modName in
  if (index = -1) then
	(print_endline "Module table has no space for more modules.";
	 raise Simerrors.TopLevel)
  else
    (* loading  *)
    let _ = Simerrors.handleSimExceptions (Ccode_stubs.load modName index) in
    (* load ocaml (compiler) symbol tables.                              *)
    (* impossible to fail: any failure should be caught by the c loader  *)
    (* already                                                           *)
    let amod = Loadmodtab.loadModuleTable ((!path) ^ modName) in
      enterModuleTable amod index
		 
		 
(* load module *)
let moduleLoad modName =
  if modName = topModuleName then loadTopModule ()
  else loadModule modName	

(***************************************************************************)
(*                 install and open a module context                       *)
(* 1. search the ocaml module table to find the module and its index;      *)
(* 2. register the module as current module;                               *)
(* 3. invoke C functions to install the module and open its context for the*)
(*    simulator.                                                           *)
(***************************************************************************)
(* top module       *)
let openTopModule () =
  setCurrentModule (getModuleTableModule topModule);
  let _ = Simerrors.handleSimExceptions (Ccode_stubs.topModuleInstall ()) in
  ()

(* specified module *)
let openModule modName =
  let (index, amod) = findModule modName in
  if (amod = Absyn.ErrorModule) then
	(print_endline ("Module " ^ modName ^ " is not currently loaded.");
	 raise Simerrors.TopLevel)
  else
	(setCurrentModule amod;
	 let _ = Simerrors.handleSimExceptions (Ccode_stubs.moduleInstall(index)) 
	 in
	 ())

(* install module *)
let moduleInstall modName =
  if modName = topModuleName then openTopModule ()
  else openModule modName
	

(* init module context *)
let initModuleContext () =
  let amod = getCurrentModule () in
  if (Absyn.getModuleName amod) = topModuleName then ()
  else 
    let _ = Simerrors.handleSimExceptions (Ccode_stubs.initModuleContext()) in
    ()
      

let cleanModule () =
  Ccode_stubs.cleanModule()
  
