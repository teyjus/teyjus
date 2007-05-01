(***************************************************************************)
(* This module specifies the global module table used for the compiler.    *)
(***************************************************************************)

type moduletable = ModuleTable of (string * Absyn.amodule) 

let getModuleTableModuleName = function
	ModuleTable(name, _) -> name
let getModuleTableModule = function
	ModuleTable(_, amod) -> amod
let makeModuleTable amod =
  let name = Absyn.getModuleName amod in
  ModuleTable(name, amod)

let moduleTables : (moduletable list) ref = ref []

let getModuleTable  ()    = !moduleTables
let setModuleTable modTab = moduleTables := modTab

let initModuleTable ()    = setModuleTable []
let enterModuleTable amod = 
  moduleTables := (makeModuleTable amod) :: !moduleTables

let findModule name  =
  let rec findModuleAux table =
	match table with
	  [] -> Absyn.ErrorModule (* module not found *)
	| (ModuleTable(name', amod) :: rest) ->
		if (name = name') then amod
		else findModuleAux rest
  in
  findModuleAux (getModuleTable ())

let removeModule name =
  let rec removeModuleAux table =
	match table with
	  [] -> []
	| (entry :: rest) ->
		let ModuleTable(name', amod) = entry in
		if (name = name') then rest (* assume module names are unique *)
		else (entry :: (removeModuleAux rest))
  in
  setModuleTable (removeModuleAux (getModuleTable ()))


		  
