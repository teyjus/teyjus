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
(*     Clause code is gathered by invocation of functions from             *)
(*     the module Clausegen. Back patching is performed over call and      *)
(*     execute instructions after offsets of all clause definitions of this*)
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
		

let main_pred = ref None

let set_main_pred name : unit =
  main_pred := Some (name, 0)

let get_main_pred_loc () : int =
  snd (Option.get !main_pred)
              
let getCGModuleName = function
    Module(cgname,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> cgname
                                                    
let getNumCGConsts = function
    ConstantList(_,num) -> num

let getNumCGTypeSkeletons = function
    TypeSkeletonList(_,num) -> num

let getNumCGPreds = function
    PredList(_,num) -> num

(*****************************************************************************)
(*  HASH TABLE LIST:                                                         *)
(*****************************************************************************)
(* entries in one hash table  *)
let hashTabEntries : (cghashtabentry list) ref = ref []

let initHashTabEntries () = hashTabEntries := [] 
let getHashTabEntries  () = !hashTabEntries
let addHashTabEntry entry = hashTabEntries := (!hashTabEntries) @ [entry]

(* module hash tables         *)
let hashTabs : cghashtabs ref = ref (ConstHashTabs([], 0))

let initHashTabs     ()  = hashTabs := (ConstHashTabs([], 0))
let getHashTabs      ()  = !hashTabs
let getHashTabNumber ()  = let ConstHashTabs(_, number) = !hashTabs in number
let addHashTabs newTab   = 
  let ConstHashTabs(tabs, number) = !hashTabs in
  hashTabs := (ConstHashTabs(tabs @ [newTab], number+1))


(*****************************************************************************)
(* GLOBAL DATA STRUCTURE INITIALIZATION                                      *)
(*****************************************************************************)
let genCodeInitialization () =
  Registers.initRegsArray ();       
  Clausegen.initBackPatchLists (); 
  Clausegen.initImpPointList ();
  initHashTabs ()

(*****************************************************************************)
(*                          ASSIGN KIND INDEXES                              *)
(* assign indexes (offsets) to global and local kinds of the module          *)
(*****************************************************************************)
let assignKindIndex gkinds lkinds = 
  let rec assignKindIndexAux kinds index =
	match kinds with 
	  [] -> index
	| (kind :: rest) ->
		Absyn.setKindIndex kind index;
		assignKindIndexAux rest (index + 1)
  in
  let numGKinds = assignKindIndexAux gkinds 0 in
  let numLKinds = assignKindIndexAux lkinds 0 in
  (KindList(gkinds, numGKinds), KindList(lkinds, numLKinds))

(******************************************************************************)
(*                         ASSIGN CONSTANT INDEXES                            *)
(* 1. assign indexes (offsets) to global, local and hidden constants of the   *)
(*    module.                                                                 *)
(* 2. gather predicates having definitions in this module                     *)
(*    gather non-exportdef global predicates having definitions in this module*)
(*    gather exportdef (global) predicates having definitions in this module  *)
(*    gather local predicates having definitions in this module               *)
(* 3. gather predicates whose previous definitions may be extended by code    *)
(*    in this module                                                          *)
(******************************************************************************)
let assignConstIndex gconsts lconsts hconsts =

  (* a). assignIndex for local consts;                                    *)
  (* b). gather predicates having definitions;                            *)
  (* c). close the defintions for such predicate.                         *)
  let rec assignLocalConstIndex consts index defs numDefs =
	match consts with
	  [] -> (index, defs, numDefs)  (*note defs are in reversed order *)
	| (const :: rest) ->
	   Absyn.setConstantIndex const index; (*set index*)
	   let (newDefs, newNumDefs) =
		 match (!(Absyn.getConstantCodeInfo const)) with
		   None -> (defs, numDefs)         (* not have def in this module *)
		 | Some(Absyn.Clauses(clauseBlock)) ->
			Absyn.setClauseBlockClose clauseBlock true;
			(const :: defs, numDefs + 1)
		 | _ ->
		    Errormsg.impossible Errormsg.none 
			  ("Codegen.assignConstIndex: invalid constant codeInfo on constant '" ^
			     (Absyn.getConstantName const) ^ "'")
	   in
	   assignLocalConstIndex rest (index + 1) newDefs newNumDefs
  in
  
  (* assign indexes for hidden constants *)
  let rec assignHiddenConstIndex consts index = 
	match consts with
	  [] -> index
	| (const :: rest) ->
		Absyn.setConstantIndex const index;
		assignHiddenConstIndex rest (index + 1)
  in

  (* a). assign indexes for global constants;                             *)
  (* b). increment predicates having definitions;                         *)
  (* c). gather predicates whose previous definitions could be extended   *)
  (*     by this module (non-exportDef ones);                             *)
  (* d). gather exportDef predicates and close off their definitions.     *)
  let rec assignGlobalConstIndex consts index defs numDefs nonExpDefs 
      numNonExpDefs expDefs numExpDefs =
    match consts with
      [] -> (index, defs, numDefs, nonExpDefs, numNonExpDefs, expDefs, 
	     numExpDefs) (* reverse?! *)
    | (const :: rest) ->
	Absyn.setConstantIndex const index;
	let codeInfo = Absyn.getConstantCodeInfo const in
	let (newDefs, newNumDefs, newNonExpDefs, newNumNonExpDefs, newExpDefs, 
	     newNumExpDefs) =
	  match (!codeInfo) with
	   (* None -> (defs, numDefs, nonExpDefs, numNonExpDefs, expDefs, 
		     numExpDefs) *)
	    None ->
	      if (Absyn.getConstantExportDef const) then
		(defs, numDefs, nonExpDefs, numNonExpDefs, const::expDefs,
		 numExpDefs+1)
	      else
		(defs, numDefs, nonExpDefs, numNonExpDefs, expDefs, 
		 numExpDefs)
	  | Some(Absyn.Clauses(clauseBlock)) -> 
	      if (Absyn.getConstantExportDef const) then
		(Absyn.setClauseBlockClose clauseBlock true;
		 (const :: defs, numDefs + 1, nonExpDefs, numNonExpDefs, 
		  const :: expDefs, numExpDefs + 1))
	      else
		(Absyn.setClauseBlockNextClause clauseBlock 
		   (numNonExpDefs + 1);
		 (const :: defs, numDefs + 1, const :: nonExpDefs, 
		  numNonExpDefs + 1, expDefs, numExpDefs))
	  | _ -> Errormsg.impossible Errormsg.none 
		"assignConstIndex: invalid constant codeInfo"
	in
	assignGlobalConstIndex rest (index+1) newDefs newNumDefs newNonExpDefs
	  newNumNonExpDefs newExpDefs newNumExpDefs
  in
  (* function body of assignConstIndex *)
  let (numLConsts, defs, numDefs) = 
	assignLocalConstIndex lconsts 0 [] 0 
  in
  let numHConsts = assignHiddenConstIndex  hconsts 0 in
  let (numGConsts, newDefs, newNumDefs, nonExpDefs, numNonExpDefs, expDefs,
	   numExpDefs) =
	assignGlobalConstIndex gconsts 0 defs numDefs [] 0 [] 0 
  in
  (if (numExpDefs > Instr.i1Size) then 
	Errormsg.error Errormsg.none  "Too many open predicates in the module"
  else ());
  (ConstantList (gconsts, numGConsts), ConstantList (lconsts, numLConsts), 
   ConstantList (hconsts, numHConsts), 
   PredList (List.rev newDefs, newNumDefs), 
   PredList(List.rev nonExpDefs, numNonExpDefs), 
   PredList(List.rev expDefs, numExpDefs),
   PredList(List.rev defs, numDefs))


(****************************************************************************)
(*                      ASSIGN TYPE SKELETON INDEXES                        *)
(* merge type skeletons and those of hidden constants; assign indexes       *)
(****************************************************************************)
let assignSkelIndex skels hskels =

  let rec assignSkelIndexAux skels assigned index =
    match skels with
      [] -> (List.rev assigned, index)
    | (skel :: rest) ->
		(* assign an index to skel: if appeared in skels, then use the index *)
        (* recorded there, otherwise, use the index as the given argument and*)
        (* add skel to the "assigned" list.                                  *)
		let rec mergeOrAssign skels =
		  match skels with
			[] -> 
			  Absyn.setSkeletonNew skel true;
			  Absyn.setSkeletonIndex skel index;
			  (skel :: assigned, index + 1)
		  | (skel' :: rest') ->
			  if (skel' == skel) then (* already dealt with: remove *)		
				(assigned, index)
			  else
				if (Types.equalMappedTypeSkels skel skel') then
				  (Absyn.setSkeletonNew skel false;
				   Absyn.setSkeletonIndex skel (Absyn.getSkeletonIndex skel');
				   (assigned, index))
				else mergeOrAssign rest' 
		in
		let (newAssigned, newIndex) = mergeOrAssign assigned in
		assignSkelIndexAux rest newAssigned newIndex 
  in

  (* function body of assignSkelIndex *)
  let mySkels = skels @ hskels in
  let (newSkels, numTySkels) = assignSkelIndexAux mySkels [] 0 in
  TypeSkeletonList(newSkels, numTySkels)

(****************************************************************************)
(*                      ASSIGN STRING INDEXES                               *)
(* assign indexes to strings in this module                                 *)
(****************************************************************************)
let assignStringIndex strs =
  let rec assignStringIndexAux strs assigned index =
    match strs with
      [] -> (List.rev assigned, index)
    | (str :: rest) ->
		(* assign an index to str: if appeared in strs, then use the index   *)
        (* recorded there, otherwise, use the index as the given argument and*)
        (* add str to the "assigned" list.                                   *)
		let rec mergeOrAssign strs =
		  match strs with
			[] -> 
			  Absyn.setStringInfoNew str true;
			  Absyn.setStringInfoIndex str index;
			  (str :: assigned, index + 1)
		  | (str' :: rest') ->
			  if (str' == str) then (assigned, index)
			  else
				if (Absyn.getStringInfoString str) =
				  (Absyn.getStringInfoString str') then
				  (Absyn.setStringInfoNew str false;
				   Absyn.setStringInfoIndex str (Absyn.getStringInfoIndex str');
				   (assigned, index))
				else mergeOrAssign rest' 
		in
		let (newAssigned, newIndex) = mergeOrAssign assigned in
		assignStringIndexAux rest newAssigned newIndex
  in
  (* function body of assignStringIndex *)
  let (newStrs, numStrings) = assignStringIndexAux strs [] 0 in
  StringList(newStrs, numStrings)

(****************************************************************************)
(*                      COLLECT RENAMING INFORMATION                        *) 
(* collect renaming information for imported and accumulated modules        *)
(****************************************************************************)
let collectRenamingInfo amod =
  (* function body of collectRenamingInfo *)
  let gkinds = Absyn.getSignatureGlobalKindsList amod in
  let gconsts = Absyn.getSignatureGlobalConstantsList amod in
  RenamingInfo(Absyn.getSignatureName amod, 
	       KindList(List.rev gkinds, List.length gkinds),
	       ConstantList(List.rev gconsts, List.length gconsts))


let collectImports imps = 
  let rec collectImportsAux imps renamingInfo =
    match imps with
      [] -> (List.rev renamingInfo)
    | (Absyn.ImportedModule(_, amod) :: rest) ->
	collectImportsAux rest ((collectRenamingInfo amod) :: renamingInfo)
  in 
  collectImportsAux imps []

let collectAccs accs = 
  let rec collectAccsAux accs renamingInfo =
    match accs with
      [] -> (List.rev renamingInfo)
    | (Absyn.AccumulatedModule(_, amod) :: rest) ->
	collectAccsAux rest ((collectRenamingInfo amod) :: renamingInfo)
  in 
  collectAccsAux accs []

(**************************************************************************)
(*                    GENERATE INSTRUCTIONS                               *)
(**************************************************************************)
(****************************************************************)
(* location of the fail instruction: always as the first one    *)
(****************************************************************)
let failInstrLoc = 0  

(****************************************************************)
(* data representations for switching information               *)
(****************************************************************)
(* predicate index information: predindexinfo                   *)
(* (leading clauses, partitions)                                *)
(* 1.Leading clauses are assumed to be those with free variables*)
(*   as the head of the first argument.                         *)
type predindexinfo = PredIndex of Absyn.aclause list * switchinfo list
(* 2.Partitions of the clauses list: switchinfo                 *)
(*   (allclauses, trailclauses, constclauses, intclauses,       *)
(*    realclauses, strclauses, nilclauses, consclauses)         *)
(*    a. allclauses: clauses with first arguments that have     *)
(*                   non-free variable heads;                   *)
(*    b. trailclause: clauses with first arguments that have    *)
(*                    free variable heads.                      *)
and  switchinfo   = 
	SwitchInfo of Absyn.aclause list * Absyn.aclause list * 
        constswitchInfo list * Absyn.aclause list * Absyn.aclause list
		* Absyn.aclause list * Absyn.aclause list * Absyn.aclause list
(*    c. constclauses: constant choices plus the clauses for them*)
and  constswitchInfo =  ConstSwInfo of Absyn.aconstant * Absyn.aclause list

(***************************************************************************)
(* Calculate predicate indexing information                                *)
(*                 Absyn.aclause list -> predindexinfo                     *)
(* Break up the list of clauses into a leading part, where the head of the *)
(* first argument of every clause is a free variable, followed by a list   *)
(* of partitions.                                                          *)
(* Note it is assumed that the given clauses list is non-empty.            *)
(***************************************************************************)
let calculateIndexing clauses =
  
  (* obtain the head of a (beta-normal form of) term *)
  let rec termHead term =
	match term with
	  Absyn.AbstractionTerm(_) -> termHead (Absyn.getTermAbstractionBody term)
	| Absyn.ApplicationTerm(_) -> termHead (Absyn.getTermApplicationHead term)
	| _ -> term
  in

  (* collect (contiguous) leading clauses with free-variable-head first *) 
  (* argument in a given clause list *)
  (* It is known that the arguments of the given clauses cannot be nil  *)
  let rec getVarClauses cls varCls =
	match cls with 
	  [] -> (List.rev varCls, [])
	| (cl :: rest) ->
		if (Absyn.isTermFreeVariable
			  (termHead (List.hd (Absyn.getClauseTermArgs cl)))) then
		  getVarClauses rest (cl :: varCls)
		else (List.rev varCls, cls)
  in
  
  (* collect (contiguous) leading clauses with non-free-variable-head first *)
  (* argument in a given clause list *)
  (* It is known that the arguments of the given clauses cannot be nil  *)
  let rec getNonVarClauses cls nvarCls =
	match cls with
	  [] -> (List.rev nvarCls, [])
	| (cl :: rest) -> 
		if not(Absyn.isTermFreeVariable 
				 (termHead (List.hd (Absyn.getClauseTermArgs cl))))
		then getNonVarClauses rest (cl :: nvarCls)
		else (List.rev nvarCls, cls)
  in

  (* partition a given clauses list into blocks with starting with *)
  (* non-fv-head first argument and ending with fv-head first args *)
  let rec partition cls = 
	match cls with
	  [] -> []
	| _ ->
		(* get leading clauses with non-var-head first args *)
		let (allclauses, rest) = getNonVarClauses cls [] in
		(* get clauses with var-head first args in the remaining clauses *)
		let (trailclauses, nextblock) = getVarClauses rest [] in
		(* partition the clauses remained from the previous step *)
		((allclauses, trailclauses) :: (partition nextblock))  
  in

  (* generate partition information from one given clauses block *)
  let genPartition (allclauses, trailclauses) =
	(* collect constant switch information *)
	let rec insertConstSwitch c cl constSwitch inserted =
	  match constSwitch with
		[] -> 
		  if inserted then []
		  else [ConstSwInfo(c, [cl])]
	  | (ConstSwInfo(c', cls) :: rest) ->
		  if (not inserted) && (c == c') then 
			(ConstSwInfo(c', cls @ [cl]) :: insertConstSwitch c cl rest true)
		  else
			(ConstSwInfo(c', cls) :: insertConstSwitch c cl rest inserted)
	in

	(* partition rigid clauses *)
	let rec genPartitionAux rigidCls constSwitch intCls realCls strCls nilCls 
		consCls =
	  match rigidCls with
		[] -> (List.rev constSwitch, List.rev intCls, List.rev realCls, 
			   List.rev strCls, List.rev nilCls,  List.rev consCls)
	  | (cl :: rest) ->
		  let firstArgHead = termHead (List.hd (Absyn.getClauseTermArgs cl)) in
		  let (constSwitch', intCls', realCls', strCls', nilCls', consCls') =
			match firstArgHead with
			  Absyn.ConstantTerm(c, _,  _) ->
				if (Pervasive.isnilConstant c) then
				  (constSwitch, intCls, realCls, strCls, cl :: nilCls, consCls)
				else if (Pervasive.isconsConstant c) then
				  (constSwitch, intCls, realCls, strCls, nilCls, cl::consCls)
				else
				  (insertConstSwitch c cl constSwitch false, intCls, realCls, 
				   strCls, nilCls, consCls)
			| Absyn.IntTerm(_) ->
				(constSwitch, cl :: intCls, realCls, strCls, nilCls, consCls)
			| Absyn.RealTerm(_) ->
				(constSwitch, intCls, cl :: realCls, strCls, nilCls, consCls)
			| Absyn.StringTerm(_) -> 
				(constSwitch, intCls, realCls, cl :: strCls, nilCls, consCls) 
			| _ -> (* must be bound variables *)
				(constSwitch, intCls, realCls, strCls, nilCls, consCls)
		  in
		  genPartitionAux rest constSwitch' intCls' realCls' strCls' nilCls'
			consCls'
	in  

	(* function body of genPartition *)
	let (constSwitch, intCls, realCls, strCls, nilCls, consCls) = 
	  genPartitionAux allclauses [] [] [] [] [] []
	in
	SwitchInfo(allclauses, trailclauses, constSwitch, intCls, realCls,
			   strCls, nilCls, consCls)
  in
	
  (* function body of calculateIndexing *)
  if (Absyn.getClauseTermArgs (List.hd clauses)) = [] then (* propositions *)
	PredIndex(clauses, []) (* everything belongs to leading clauses *)
  else
	let (leadingClauses, rest) = getVarClauses clauses [] in
	let partitions = List.map genPartition (partition rest) in
	PredIndex (leadingClauses, partitions)
  

(*****************************************************************************)
(* Breaking a clauses list into a leading clause, the middle clauses segment *)
(* and the last clause. It is assumed that the given clause list contains at *)
(* least two clauses.                                                        *)
(*****************************************************************************)
let breakClauses cls =
  let rec breakClausesAux cls =
	let (cl, rest) = (List.hd cls, List.tl cls) in
	if (rest = []) then ([], cl)
	else 
	  let (mid, last) = breakClausesAux rest in
	  (cl :: mid, last)
  in
  let (start, rest)  = (List.hd cls, List.tl cls) in
  let (middle, last) = breakClausesAux rest in
  (start, middle, last)

(************************************************************************)
(* generating an indexed sequence of code for a list of variable clauses*)
(************************************************************************)
let genVarClausesCode cls isbeginning isend insts startLoc =
  let numArgs = Absyn.getClauseNumberOfArgs (List.hd cls) in
  (* generate instruction for a single clause clauses list*)
  let genSingleVarClause cl insts startLoc =
	match isbeginning, isend with 
	  true,  true  ->
	       Clausegen.genClauseCode cl insts startLoc
	| true,  false ->
		(*   try_me_else numArgs L *)
		let nextCodeLocRef = ref 0 in
		let try_me_else = Instr.Ins_try_me_else(numArgs, nextCodeLocRef) in
		(*   clause instructions   *)
		(* L:                      *)
		let (newInstrs, nextCodeLoc)= 
		  Clausegen.genClauseCode cl (insts @ [try_me_else]) 
			                      (startLoc + Instr.getSize_try_me_else)
		in
		nextCodeLocRef := nextCodeLoc;
		(newInstrs, nextCodeLoc)
	| false, true  ->
		(*    trust_me numArgs     *)
		let trust_me = Instr.Ins_trust_me(numArgs) in
		(*    clause instructions  *)
		Clausegen.genClauseCode cl (insts @ [trust_me]) 
		                        (startLoc+Instr.getSize_trust_me)
	| false, false ->
		(*    retry_me_else numArgs L *) 
		let nextCodeLocRef = ref 0 in
		let retry_me_else = Instr.Ins_retry_me_else(numArgs, nextCodeLocRef) in
		(*    clause instructions     *)
		(* L:                         *)
		let (newInstrs, nextCodeLoc) =
		  Clausegen.genClauseCode cl (insts @ [retry_me_else])
			                      (startLoc + Instr.getSize_retry_me_else)
		in
		nextCodeLocRef := nextCodeLoc;
		(newInstrs, nextCodeLoc)
  in 
  
  (* generate code for the leading clause in a var clauses list *)
  let genVarClausesStart cl insts startLoc =
	(*    try_me_else numArgs L/ retry_me_else numArgs L *)
	let nextCodeLocRef = ref 0 in
	let (instr, clCodeLoc) = 
	  if (isbeginning) 
	  then (Instr.Ins_try_me_else(numArgs, nextCodeLocRef), 
			startLoc + Instr.getSize_try_me_else)
	  else (Instr.Ins_retry_me_else(numArgs, nextCodeLocRef), 
			startLoc + Instr.getSize_retry_me_else)
	in
	(*    clause instructions                            *)
	(* L:                                                *) 
	let (newInsts, nextCodeLoc) =
	  Clausegen.genClauseCode cl (insts @ [instr]) clCodeLoc in
	nextCodeLocRef := nextCodeLoc;
	(newInsts, nextCodeLoc)
  in

  (* generate code for the clauses in the middle of a var clauses list *)
  let rec genVarClausesMid cls insts startLoc =
	match cls with
	  [] -> (insts, startLoc)
	| (cl :: rest) ->
		(*    retry_me_else numArgs L *)
		let nextCodeLocRef = ref 0 in
		let retry_me_else  = Instr.Ins_retry_me_else(numArgs, nextCodeLocRef) 
		in
		(*    clause instructions     *)
		(* L:                         *)
		let (newInstrs, nextCodeLoc) = 
		  Clausegen.genClauseCode cl (insts @ [retry_me_else]) 
			                      (startLoc + Instr.getSize_retry_me_else)
		in
		nextCodeLocRef := nextCodeLoc;
		genVarClausesMid rest newInstrs nextCodeLoc
  in

  (* generate code for the last clause in a var clauses list *)
  let genVarClausesLast cl insts startLoc = 
	(*     trust_me numArgs / retry_me_else numArgs L *)
	let nextCodeLocRef = ref 0 in
	let (instr, clCodeLoc) =
	  if (isend) then 
		(Instr.Ins_trust_me(numArgs), startLoc + Instr.getSize_trust_me)
	  else 
		(Instr.Ins_retry_me_else(numArgs, nextCodeLocRef),
		 startLoc + Instr.getSize_try_me_else)
	in
	(*     clause instructions                         *)
	(*  L:                                             *)  
	let (newInstrs, nextCodeLoc) = 
	  Clausegen.genClauseCode cl (insts @ [instr]) clCodeLoc 
	in
	nextCodeLocRef := nextCodeLoc;
	(newInstrs, nextCodeLoc)
  in

  (* function body of genVarClausesCode *)
  match cls with (* assume: cls cannot be an empty list *)
	[cl] -> genSingleVarClause cl insts startLoc 
  | _    -> 
	 let (start, middle, last)  = breakClauses cls in
	 let (startIns, midCodeLoc) = genVarClausesStart start insts startLoc in
	 let (midIns,lastCodeLoc) = genVarClausesMid middle startIns midCodeLoc in
	 genVarClausesLast last midIns lastCodeLoc 
  

(***********************************************************************)
(* generating a try/retry/trust sequence; it is assumed that the given *)
(* clauses list contains at least two clauses                          *)
(***********************************************************************)
let genSeqCode cls insts startLoc =
  (* it is assumed that the clauses list contains at least two clauses *)
  let (first, middle, last) = breakClauses cls in
  let numArgs = Absyn.getClauseNumberOfArgs first in

  (* generate try code for the first clause *)
  let genTryCode cl =
	let offset = Absyn.getClauseOffset cl in
	let try_code = Instr.Ins_try(numArgs, ref offset) in 
	(try_code, Instr.getSize_try)
  in
  (* generate retry code for the middle clauses *)
  let rec genRetryCode cls =
	match cls with
	  [] -> ([], 0)
	| (cl :: rest) ->
		let (code, size) = genRetryCode rest in
		let offset = Absyn.getClauseOffset cl in
		let retry = Instr.Ins_retry(numArgs, ref offset) in
		(retry :: code, size + Instr.getSize_retry)
  in
  (* generate trust code for the last clause *)
  let genTrustCode cl =
	let offset = Absyn.getClauseOffset cl in
	let trust = Instr.Ins_trust(numArgs, ref offset) in 
	(trust, Instr.getSize_trust)
  in
  
  let (firstCode, firstS) = genTryCode first in
  let (midCode, midS) = genRetryCode middle in
  let (lastCode, lastS) = genTrustCode last in
  (insts @ (firstCode :: midCode) @ [lastCode], startLoc+firstS + midS + lastS)

(*****************************************************************************)
(* determining a code location given a nonempty sequence of clauses to try;  *)
(* if the sequence is of length > 1, a try/retry/trust sequence will be      *)
(* generated.                                                                *)
(*****************************************************************************)
let genCodeTableEntry cls insts startLoc =
  (* assume cls is not empty *)
  match cls with
	[cl] -> (insts, startLoc, Absyn.getClauseOffset cl)
  | _    -> 
	  let (newInsts, newStartLoc) = genSeqCode cls insts startLoc in
	  (newInsts, newStartLoc, startLoc)

(*************************************************************************)
(* generating a sequence for cons if there are clauses; otherwise return *)
(* the location of the fail instruction.                                 *)
(*************************************************************************)
let genSwConsCode conscls insts startLoc = 
  if (conscls = []) then (insts, startLoc, failInstrLoc)
  else (genCodeTableEntry conscls insts startLoc)

(*************************************************************************)
(* generating the hash table, indexing and sequencing code for constant  *)
(* headed clauses in a partition; location of a fail instruction is      *)
(* returned if there are no clauses.                                     *)
(*************************************************************************)
let genSwConstCode partition insts startLoc = 
  (* add one hash table entry into the global hash table list if the given *)
  (* clauses list is not empty                                             *)
  let genHashTabEntry cls constCat constInd size insts startLoc =
	if (cls = []) then (insts, startLoc, size)
	else 
	  let (newInsts, newStartLoc, codeLoc) = 
		genCodeTableEntry cls insts startLoc
	  in 
	  addHashTabEntry (ConstHashTabEntry(constCat, constInd, codeLoc));
	  (newInsts, newStartLoc, size + 1)
  in	
  (* add hash table entries for constants *)
  let rec genHashTabEntryConsts constSws size insts startLoc =  
    match constSws with
      [] -> (insts, startLoc, size)
	| (ConstSwInfo(pred, cls) :: rest) -> 
		let (newInsts, newStartLoc, newSize) =
		  genHashTabEntry cls (Absyn.getConstantType pred) 
			(Absyn.getConstantIndex pred) size insts startLoc
		in
		genHashTabEntryConsts rest newSize newInsts newStartLoc
  in

  (* function body of genSwConstCode *)
  initHashTabEntries(); 
  match partition with 
	 SwitchInfo(_, _, constSws, intcls, realcls, strcls, nilcls, _) ->
	   let (intCode, intCodeLoc, tabSizeInt) = 
		 genHashTabEntry intcls (Absyn.PervasiveConstant(false)) 
		   (Absyn.getConstantIndex(Pervasive.intcConstant)) 0 insts startLoc 
	   in
	   let (realCode, realCodeLoc, tabSizeReal) = 
		 genHashTabEntry realcls (Absyn.PervasiveConstant(false)) 
		   (Absyn.getConstantIndex(Pervasive.realcConstant)) tabSizeInt 
		   intCode intCodeLoc
	   in
	   let (strCode, strCodeLoc, tabSizeStr) =
		 genHashTabEntry strcls (Absyn.PervasiveConstant(false))
		   (Absyn.getConstantIndex(Pervasive.strcConstant)) tabSizeReal
		   realCode realCodeLoc
	   in
	   let (nilCode, nilCodeLoc, tabSizeNil) =
		 genHashTabEntry nilcls (Absyn.PervasiveConstant(false))
		   (Absyn.getConstantIndex(Pervasive.nilConstant)) tabSizeStr
		   strCode strCodeLoc
	   in
	   let (constCode, constCodeLoc, tabSize) =
		 genHashTabEntryConsts constSws tabSizeNil nilCode nilCodeLoc
	   in
	   if (tabSize = 0) then (constCode, constCodeLoc, failInstrLoc)
	   else 
		 let newHashTab = ConstHashTab(tabSize, getHashTabEntries()) in
		 let switch_on_constant = 
		   Instr.Ins_switch_on_constant(tabSize, getHashTabNumber())
		 in
		 addHashTabs newHashTab;
		 (constCode @ [switch_on_constant], 
		  constCodeLoc + Instr.getSize_switch_on_constant,
		  constCodeLoc)

(***************************************************************************)
(* generating code for a partition, i.e. a segment of clauses with constant*)
(* heads for the first argument followed by a list of 'variable head'      *)
(* clauses.                                                                *)
(***************************************************************************)
let genPartitionCode partition isbeginning isend insts startLoc =
  
  let genSwitchOnTerm allcls conscls partition insts startLoc =
	(*    switch_on_term V C L V *)
	let varCodeLoc = startLoc + Instr.getSize_switch_on_term in
	let constCodeLocRef = ref 0 in
	let consCodeLocRef = ref 0 in
	let switch_on_term = 
	  Instr.Ins_switch_on_term(ref varCodeLoc, constCodeLocRef, consCodeLocRef,
							   ref varCodeLoc)
	in
	(* V: variable code          *)
	let (varCode, varCodeNextLoc) =
	  genVarClausesCode allcls true true (insts @ [switch_on_term]) varCodeLoc
	in
	(* C: const code             *)
	let (constCode, constCodeNextLoc, constCodeLoc) = 
	  genSwConstCode partition varCode varCodeNextLoc
	in
	(* L: cons code              *) 
	let (consCode, consCodeNextLoc, consCodeLoc) =
	  genSwConsCode conscls constCode constCodeNextLoc
	in
	constCodeLocRef := constCodeLoc;
	consCodeLocRef := consCodeLoc;
	(consCode, consCodeNextLoc)
  in

  (* function body of genPartitionCode *)
  match partition with
	SwitchInfo(allclauses, trailclauses, _, _, _, _, _, conscls) ->
	  let emptyTrailCls = trailclauses = [] in
	  let (newInsts, newStartLoc) =
		let myisend = isend && emptyTrailCls in
		(* assume allclauses contains at least one clause *)
		match allclauses with
		  [cl] -> (* only one clause *)
			genVarClausesCode allclauses isbeginning myisend insts startLoc 
		| _    -> 
			let numArgs  = Absyn.getClauseNumberOfArgs (List.hd allclauses) in
			match isbeginning, myisend with
			  true,  true   -> 
				genSwitchOnTerm allclauses conscls partition insts startLoc
			| true,  false  ->
				(*    try_me_else numArgs L *)
				let nextCodeLocRef = ref 0 in
				let try_me_else = 
				  Instr.Ins_try_me_else(numArgs, nextCodeLocRef)
				in
				(*    partition code        *)
				(* L:                       *)
				let (switchIns, nextCodeLoc) =
				  genSwitchOnTerm allclauses conscls partition
					(insts @[try_me_else]) (startLoc+Instr.getSize_try_me_else)
				in
				nextCodeLocRef := nextCodeLoc;
				(switchIns, nextCodeLoc)
			| false, true  ->
				(*    trust_me numArgs *)
				let trust_me = Instr.Ins_trust_me(numArgs) in
				(*    partition code   *)
				genSwitchOnTerm allclauses conscls partition 
				  (insts @ [trust_me]) (startLoc+Instr.getSize_trust_me) 
			| false, false   ->
				(*    retry_me_else numArgs L *)
				let nextCodeLocRef = ref 0 in
				let retry_me_else =
				  Instr.Ins_retry_me_else(numArgs, nextCodeLocRef) 
				in
				(*    partition code        *)
				(* L:                       *)
				let (switchIns, nextCodeLoc) =
				  genSwitchOnTerm allclauses conscls partition
					(insts @ [retry_me_else]) 
					(startLoc+Instr.getSize_retry_me_else)
				in
				nextCodeLocRef := nextCodeLoc;
				(switchIns, nextCodeLoc)
	  in
	  if emptyTrailCls then (newInsts, newStartLoc)
	  else 
	    genVarClausesCode trailclauses false isend newInsts newStartLoc
			  
(*****************************************************************)
(* generating switch (indexing) code for first the leading       *)
(* clauses and then for the partitions.                          *)
(*****************************************************************)	
let genSwitchCode predIndexInfo insts startLoc = 

  let rec genSwitchCodePartitions partitions isbeginning insts startLoc  =
	match partitions with
	  [] -> (insts, startLoc)
	| (partition :: rest) ->
		let (newInsts, newStartLoc) =
		  genPartitionCode partition isbeginning (rest = []) insts startLoc
		in
		genSwitchCodePartitions rest false newInsts newStartLoc
  in
  match predIndexInfo with
    PredIndex([], [])                     -> (insts, startLoc) (* possible? *)
  | PredIndex([], partitions)             ->
      genSwitchCodePartitions partitions true insts startLoc 
  | PredIndex(leadingClauses, [])         ->
      genVarClausesCode leadingClauses true true insts startLoc 
  | PredIndex(leadingClauses, partitions) ->
      let (leadingCode, leadingCodeNext) =
	genVarClausesCode leadingClauses true false insts startLoc 
      in
      genSwitchCodePartitions partitions false leadingCode leadingCodeNext

(********************************************************************)
(* generate dummy try_me_else instruction (for linking if necessary)*)
(********************************************************************)
let genDummyTryMeElse cls partitions =
  if (List.length cls) = 1 then (* single clause    *)
    ([Instr.Ins_try_me_else(0, ref failInstrLoc)], Instr.getSize_try_me_else)
  else                          (* multiple clauses *)
    match partitions with
      PredIndex([], [SwitchInfo(_, trailclauses, _, _, _, _, _, _)]) -> 
	if (trailclauses = []) then
	  ([Instr.Ins_try_me_else(0, ref failInstrLoc)], Instr.getSize_try_me_else)
	else  ([], 0)
    | _ -> ([], 0)

(****************************************************************)
(* generating instructions for one definition                   *)
(****************************************************************)
let processDef clauseBlock insts startLoc =
  (* generate instructions *)
  let clauses       = Absyn.getClauseBlockClauses clauseBlock in
  let partitions    = calculateIndexing clauses               in
  let (dummyTryMeElse, dummyTryMeElseSize)
      = genDummyTryMeElse clauses partitions
  in
  (* Get location of main_pred in code *)
  let pred = Absyn.getConstantName(Absyn.getClausePred (List.hd clauses)) in
  if ((Option.isSome !main_pred)
      && (pred = fst (Option.get !main_pred))) then
    main_pred := Some (pred, startLoc);
  if (Absyn.getClauseBlockClose clauseBlock) then (* closed definition *)
    (Absyn.setClauseBlockOffset clauseBlock (startLoc + dummyTryMeElseSize);
     genSwitchCode partitions (insts @ dummyTryMeElse) (startLoc + dummyTryMeElseSize))
  else
    (Absyn.setClauseBlockOffset clauseBlock startLoc;
     let numArgs       = Absyn.getClauseNumberOfArgs (List.hd clauses) in
     let nextClause    = Absyn.getClauseBlockNextClause clauseBlock in
     let tryCodeLoc    = startLoc + Instr.getSize_switch_on_reg in
     let trustCodeLoc  = tryCodeLoc + Instr.getSize_try         in
     let swCodeLoc     = 
       trustCodeLoc + Instr.getSize_trust_ext + dummyTryMeElseSize 
     in

     (*      switch_on_reg nextClause L1 L2 *)
     let switch_on_reg =
       Instr.Ins_switch_on_reg(nextClause, ref tryCodeLoc, ref swCodeLoc)
     in
     (*  L1: try numArgs L2 *)
     let tryCode = Instr.Ins_try(numArgs, ref swCodeLoc) in
     (*      trust_ext numArgs nextClause *)
     let trust_ext = Instr.Ins_trust_ext(numArgs, nextClause) in
     (*  L2: switch code    *)
     let (newInsts, newStartLoc) =
       genSwitchCode partitions 
	 (insts @ [switch_on_reg; tryCode; trust_ext] @ dummyTryMeElse) swCodeLoc
     in
     (newInsts, newStartLoc))


(****************************************************************)
(* generating instructions for top-level definitions            *)
(****************************************************************)
let processTopLevelDefs defs = 

  (* generate instruction for each top-level definition block *)
  let rec processTopLevelDefsAux clauseBlocks insts startLoc =
    match clauseBlocks with
      [] -> (insts, startLoc)
    | (clauseBlock :: rest) ->
       let (newInsts, newStartLoc) = processDef clauseBlock insts startLoc in
       processTopLevelDefsAux rest newInsts newStartLoc
  in
  
  let clauseBlocks = Absyn.getClauseInfoClauseBlocks defs in
  match clauseBlocks with
	[] -> ([], 0) (* no definition in the module*)
  | _  -> 
	  (* fail *)
	  let failIns = Instr.Ins_fail in
	  (* instructions for defs *)
	  let (insts, codeBytes) = 
		processTopLevelDefsAux clauseBlocks [failIns] (Instr.getSize_fail)
	  in
	  (insts, codeBytes)

(****************************************************************************)
(*  GENERATING INSTRUCTIONS AND COLLECT IMPLICATION POINT INFORMATION FOR   *)
(*  EMBEDDED CLAUSES.                                                       *)
(****************************************************************************)

(****************************************************************)
(* generate code for definitions in an implication              *)
(****************************************************************)
let genImpDefs defs insts startLoc =

  let rec genImpDefsAux defs insts startLoc extNum extPreds predNum predInfo =
	(* generate code for one definition *)
	let genImpDef pred clauseBlock insts startLoc extNum extPreds =
      let (newExtPreds, newExtNum) =
		if (Absyn.getClauseBlockClose clauseBlock) then (extPreds, extNum)
		else
		  (Absyn.setClauseBlockNextClause clauseBlock (extNum + 1);
		   (pred :: extPreds, extNum + 1))
	  in
	  let (newInsts, newStartLoc) = processDef clauseBlock insts startLoc in
      (newInsts, newStartLoc, newExtNum, newExtPreds, 
       ImpPredInfo(pred, Absyn.getClauseBlockOffset clauseBlock))
	in

	match defs with
	  [] ->
		(if (extNum > Instr.i1Size) then 
		  Errormsg.error Errormsg.none 
			"Too many open predicates in an implication goal"
		else ());
		(insts, startLoc, 
		 ImpGoalCode(PredList(List.rev extPreds, extNum), 
					 List.rev predInfo, predNum))
	| ((pred, clauseBlock) :: rest) ->
		let (newInsts, newStartLoc, newExtNum, newExtPreds, onePredInfo) =
		  genImpDef pred clauseBlock insts startLoc extNum extPreds 
		in
		genImpDefsAux rest newInsts newStartLoc newExtNum newExtPreds 
		  (predNum + 1) (onePredInfo :: predInfo)
  in
  genImpDefsAux defs insts startLoc 0 [] 0 []


(****************************************************************)
(* generating instructions for embedded definitions             *)
(* The embedded definitions are assumed to be recored in        *)
(* Clausegen.impPointList; the processing of these definitions  *)
(* could result new implication points being added.             *)
(****************************************************************)
let rec genImpPointCode impPoints insts startLoc impGoals numImpPoints = 

  let rec genImpPointCodeOneSeg impPoints insts startLoc impGoals =
	match impPoints with
	        (* note: no need to reverse impGoals--impPointList is collected *)
	        (* in reversed appearence order *)
	  [] -> (insts, startLoc, impGoals) 
	| (Absyn.Definitions (defs) :: rest) ->
		let (newInsts, newStartLoc, impGoal) =
		  genImpDefs defs insts startLoc 
		in
		genImpPointCodeOneSeg rest newInsts newStartLoc (impGoal :: impGoals)
  in

  match impPoints with 
	[] -> (insts, startLoc, ImpGoalList(impGoals, numImpPoints))
  | _  ->
	  let (newInsts, newStartLoc, newImpGoals) =
		Clausegen.initImpPointList();
		genImpPointCodeOneSeg impPoints insts startLoc [] 
	  in
	  genImpPointCode (Clausegen.getImpPointList ()) newInsts newStartLoc
		(impGoals @ newImpGoals) (numImpPoints +(Clausegen.getNumImpPoints ()))

(****************************************************************************)
(*  Collect local constants appearing in acc modules for instruction        *)
(*  generation.                                                             *)
(****************************************************************************)
let collectClosedConstsInAccs accs =
  let rec collectClosedConsts accConsts consts =
    match accConsts with
      [] -> consts
    | (c::rest) ->
	if (Absyn.isLocalConstant c) && not(List.memq c consts) then
	  collectClosedConsts rest (c::consts)
	else (* global constants *)
	  if (Absyn.getConstantExportDef c) then
	    collectClosedConsts rest (c::consts)
	  else
	    collectClosedConsts rest consts
  in	  

  let rec collectClosedConstsInAccsAux accs consts =
    match accs with
      [] -> List.rev consts
    | (Absyn.AccumulatedModule(_, asig) :: rest) ->
	let consts' = 
	  collectClosedConsts (Absyn.getSignatureGlobalConstantsList asig) consts
	in
	collectClosedConstsInAccsAux rest consts'
  in

  Clausegen.initAccConsts ();
  let consts = collectClosedConstsInAccsAux accs [] in
  Clausegen.setAccConsts consts

  


  


(*****************************************************************************)
(*                CODE GENERATION FOR A MODULE                               *)
(*****************************************************************************)
let generateModuleCode amod =
  genCodeInitialization ();
  match amod with
	Absyn.Module(modname, modimps, modaccs, _, _, _, modstr, gkinds, lkinds, 
				 gconsts, lconsts, hconsts, skels, hskels, clauses) ->
      let () = Errormsg.log Errormsg.none
        "Codegen.generateModuleCode: generating module code..." in
      (* collect local constants appearing in acc modules for the use   *)
      (* generating instructions.                                       *)
      collectClosedConstsInAccs modaccs;
      (* assign indexes to global and local kinds *)				   
      let (cgGKinds, cgLKinds) = assignKindIndex gkinds lkinds in
      (* 1) assign indexes to global, local and hidden constants;   *)
      (* 2) gather predicates have definitions in this module;      *)
      (* 3) gather predicates whose previous defintions may be      *)
      (*    extended by code in this module (global non-exportDef)  *)
      (* 4) gather global export-def predicates                     *)
      (* 5) gather local predicates                                 *)
      let (cgGConsts, cgLConsts, cgHConsts, cgDefs, cgGNonExpDefs, 
        cgGExpDefs, cgLDefs) = 
        assignConstIndex (List.rev gconsts) (List.rev lconsts)
          (List.rev (!hconsts))
      in
      (* merge type skeletons and those of hidden constants; assign indexes *)
      let cgTySkels = assignSkelIndex skels !hskels in 
      (* assign indexes to strings *)
      let cgStrings = assignStringIndex modstr in
      (* generate code for top-level predicate defintions *)
      let (cgTopCode, cgTopCodeSize) = processTopLevelDefs (!clauses) in
      (* generate code for definitions in implication goals, and gather     *)
      (* information for implication points                                 *)
      let (cgCode, cgCodeSize, cgImpGoals) =
        genImpPointCode (Clausegen.getImpPointList ()) cgTopCode cgTopCodeSize
		  [] (Clausegen.getNumImpPoints ())
      in
      (* form instructions list for this module *)
      let cgInstructions = Instructions(cgCode, cgCodeSize) in
      (* gather renaming information for imported modules *)
      let cgImports = collectImports modimps in
      (* gather renaming information for accumulated modules *)
      let cgAccumulates = collectAccs modaccs in
      (* back patching "call" and "execute" instructions *)
      Clausegen.backPatch ();
      let () = Errormsg.log Errormsg.none 
                 "Codegen.generateModuleCode: generated module code" in
        Module(modname, cgGKinds, cgLKinds, cgGConsts, cgLConsts, cgHConsts,
               cgDefs, cgGNonExpDefs, cgGExpDefs, cgLDefs, 
               cgTySkels, cgStrings, cgImports, cgAccumulates, cgInstructions,
               getHashTabs (), cgImpGoals)
  | _ -> Errormsg.impossible Errormsg.none 
	"genModuleCode: invalid input module"
