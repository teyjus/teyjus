(************************************************************************)
(*                     global module table                              *)
(************************************************************************)

(**               module table entry                                   **)

(* entry structure  *)
type moduletableentry = ModuleTableEntry of (string * Absyn.amodule) 

(* access functions *)
let getModuleTableModuleName = function ModuleTableEntry(name, _) -> name
let getModuleTableModule =     function ModuleTableEntry(_, amod) -> amod
let makeModuleTableEntry amod =
  let name = Absyn.getModuleName amod in
  ModuleTableEntry(name, amod)

(* special modules  *)
(* top module  *)
let topModule = 
  ModuleTableEntry("", 
				   Absyn.Module("", [], [], ref Pervasive.pervasiveConstants,
								ref Pervasive.pervasiveKinds, 
								Pervasive.pervasiveTypeAbbrevs,
								[], [], [], [], [], ref [], [], ref [], 
								ref (Absyn.ClauseBlocks []))) 
(* error module *)
let errorModule = ModuleTableEntry("%empty", Absyn.ErrorModule)


(**               system module table                                  **)
(* the size of the module table                                        *)
let numberofModules = 256 (* agree with that in system/memory.h *)

(* the module table: initialized to all empty                          *)
let moduleTables = Array.make numberofModules errorModule 

(* try to find the smallest index of a free entry in the module table; *)
(* numberofModules is returned if the module table is full.            *)
let firstFreeEntry () =
  let rec firstFreeEntryAux ind =
	if (ind = numberofModules) then ind
	else
	  if (Array.get moduleTables ind) = errorModule then ind
	  else firstFreeEntryAux (ind + 1)
  in
  firstFreeEntryAux 0
  

(* enter a given module into the module table and return its index;    *)
(* -1 is returned if the module table is full.                         *)
let enterModuleTable amod =
  let index = firstFreeEntry () in
  if (index = numberofModules) then -1 
  else
	(Array.set moduleTables index (makeModuleTableEntry amod);
	 index)

(* look for a module of given name in the current module table and     *)
(* return its abstract syntax if founded; otherwise Absyn.ErrorModule  *)
(* is returned.                                                        *)
let findModule modName =
  let rec findModuleAux ind =
	if (ind = numberofModules) then Absyn.ErrorModule
	else
	  let ModuleTableEntry(name, amod) = Array.get moduleTables ind in
	  if name = modName then amod
	  else findModuleAux (ind + 1)
  in
  findModuleAux 0

(* remove a module of given name from the module table                 *)
let removeModule modName =
  let rec removeModuleAux ind =
	if (ind = numberofModules) then ()
	else
	  let ModuleTableEntry(name, amod) = Array.get moduleTables ind in
	  if name = modName then Array.set moduleTables ind errorModule 
	  else
		removeModuleAux (ind + 1)
  in
  removeModuleAux 0
  

(***************************************************************************)
(*                       current module                                    *)
(***************************************************************************)
let currentModule = ref Absyn.ErrorModule

let setCurrentModule modtab = currentModule := modtab
let getCurrentModule ()     = !currentModule
let initCurrentModule () = setCurrentModule Absyn.ErrorModule


(***************************************************************************)
(*                       load a module                                     *)
(***************************************************************************)
let loadModule modName =
  if modName = "" then true
  else 
	let amod = Loadmodtab.loadModuleTable modName in
    (* first remove the old copy of this module if it is in module table *)
	let amod' = findModule modName in
	(if (amod' = Absyn.ErrorModule) then removeModule modName 
	else ());
	let index = enterModuleTable amod in
	if (index = -1) then 
	  (print_endline ("Error: module table full.\n");
	   false)
	else true


(***************************************************************************)
(*                       open a module context                             *)
(***************************************************************************)
let openTopModule () =
  setCurrentModule (getModuleTableModule topModule) 

let openModule modName =
  let amod = findModule modName in
  if amod = Absyn.ErrorModule then 
	(print_endline ("Error: module " ^ modName ^ " is not loaded.");
	 false)
  else
	(setCurrentModule amod;
	 true)


(* load a module:                                                       *)
(* 1. invoke the C load function to set up module run-time on simulator *)
(*    memory;                                                           *)
(* 2. invoke the OCaml load function to set up compiler symbol table    *)

