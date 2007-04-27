(***************************************************************************)
(*                    LOAD HEADER INFORMATION                              *)
(***************************************************************************)
let loadHeaderInfo () =
  (* check bytecode version number *)
  (if (Readutil.readWord () = Writeutil.byteCodeVersionNumber) then ()
   else Errormsg.error Errormsg.none "Loader: inconsistent bytecode version");
  let modName = Readutil.readString () in
  Readutil.skipNWords 1;
  modName


(****************************************************************************)
(*                 LOAD KIND SYMBOL TABLE                                   *)
(****************************************************************************)
let rec loadGlobalKinds kindSymTab index numberGlobalKinds =
  (* load one kind and add into kind symbol table *)
  let loadKind kindSymTab index =
	let arity  = Readutil.readOneByte () in
	let symbol = Symbol.symbol (Readutil.readString ()) in
	let kind   = Absyn.makeGlobalKind symbol arity index in
	Table.add symbol kind kindSymTab
  in
  if (numberGlobalKinds = 0) then kindSymTab
  else
	loadGlobalKinds (loadKind kindSymTab index) (index+1) (numberGlobalKinds-1)

let collectKindIndexMap kindSymbolTab numberKinds =
  let kindIndexMap = Array.make (numberKinds - 1) None in
  let collectKindsIndexMapAux sym kind =
	Array.set kindIndexMap (Absyn.getKindIndex kind) (Some kind)
  in
  Table.iter collectKindsIndexMapAux kindSymbolTab;
  kindIndexMap

let skipLocalKinds () =
  let numLocalKinds = Readutil.readTwoBytes () in
  Readutil.skipNBytes numLocalKinds

let loadKindSymTab () =
  (* load global kinds *)
  let numberGlobalKinds = Readutil.readTwoBytes () in
  let kindSymbolTab =
	loadGlobalKinds (Pervasive.pervasiveKinds) (Pervasive.numberPervasiveKinds)
	  numberGlobalKinds
  in
  (* collect kind-index mapping for global and pervasive kinds *)
  let kindIndexMap  = 
	collectKindIndexMap kindSymbolTab 
	  (Pervasive.numberPervasiveKinds + numberGlobalKinds)
  in
  (* skip local kind information *)
  skipLocalKinds();
  (kindSymbolTab, kindIndexMap)

(****************************************************************************)
(*                 LOAD TYPE SKELETON TABLE                                 *)
(****************************************************************************)
let rec loadTypeSkeleton kindIndexMap = 
  let cat = Readutil.readOneByte () in
  if cat = Writeutil.typeMarkArrow then
	let l = loadTypeSkeleton kindIndexMap in
	let r = loadTypeSkeleton kindIndexMap in
	Absyn.ArrowType(l, r)
  else if cat = Writeutil.typeMarkSkeletonVar then
	let offset = Readutil.readOneByte () in
	Absyn.SkeletonVarType (ref offset)
  else (* typeMarkKind *)
	loadApplicationType kindIndexMap 

and loadApplicationType kindIndexMap =
  (* get kind information *)
  let cat = Readutil.readOneByte () in
  let kindInd = Readutil.readTwoBytes () in
  (* get arity and arguments *)
  let arity = Readutil.readOneByte () in
  let args  = loadApplicationTypeArgs arity kindIndexMap in
  if (cat = Writeutil.local) then Absyn.ErrorType
  else if (cat = Writeutil.global) then
	let kind = 
	  Option.get (Array.get kindIndexMap 
					(Pervasive.numberPervasiveKinds + kindInd))
	in
	Absyn.ApplicationType(kind, args)
  else (* pervasive *)
	let kind = Option.get (Array.get kindIndexMap kindInd) in
	Absyn.ApplicationType(kind, args)
	  
and loadApplicationTypeArgs numArgs kindIndexMap = 
  let rec loadApplicationTypeArgsAux numArgs args =
	if (numArgs = 0) then List.rev args
	else 
	  loadApplicationTypeArgsAux (numArgs - 1) 
		((loadTypeSkeleton kindIndexMap) :: args)
  in
  loadApplicationTypeArgsAux numArgs []
  

(* load type skeletons and enter them into type skeleton table *)
let loadTypeSkeletons kindIndexMap =
  let numberTypeSkels = Readutil.readTwoBytes () in
  let typeSkeletonTab = Array.make (numberTypeSkels - 1) Absyn.ErrorType in

  let rec loadTypeSkeletonsAux index =
	if (index = numberTypeSkels) then ()
	else
	  let typeSkel = loadTypeSkeleton kindIndexMap in
	  Array.set typeSkeletonTab index typeSkel;
	  loadTypeSkeletonsAux (index + 1)
  in
  loadTypeSkeletonsAux 0;
  typeSkeletonTab
	

(****************************************************************************)
(*                   LOAD (GLOBAL) CONSTANTS                                *)
(****************************************************************************)
let loadFixity () =
  let number = Readutil.readOneByte () in
  if (number = Writeutil.fixityMarkInfix) then Absyn.Infix
  else if (number = Writeutil.fixityMarkInfixl) then Absyn.Infixl
  else if (number = Writeutil.fixityMarkInfixr) then Absyn.Infixr
  else if (number = Writeutil.fixityMarkNoFixity) then Absyn.NoFixity
  else if (number = Writeutil.fixityMarkPrefix) then Absyn.Prefix
  else if (number = Writeutil.fixityMarkPrefixr) then Absyn.Prefixr
  else if (number = Writeutil.fixityMarkPostfix) then Absyn.Postfix
  else Absyn.Postfixl

let loadConstSkel tySkelTab =
  let ind = Readutil.readTwoBytes () in
  Absyn.Skeleton(Array.get tySkelTab ind, ref (Some ind), ref false)

let rec loadGlobalConsts constSymTab index numConsts tySkelTab =
  (* load one constant and add it into const symbol table *)
  let loadConst constSymTab index =
	let fixity = loadFixity () in
	let prec  = Readutil.readOneByte () in
	let tyEnvSize = Readutil.readOneByte () in
	let symbol = Symbol.symbol (Readutil.readString ()) in
	let tySkel = loadConstSkel tySkelTab in
	let const = 
	  Absyn.makeGlobalConstant symbol fixity prec false false tyEnvSize 
		tySkel index 
	in
	Table.add symbol const constSymTab
  in
  if (numConsts = 0) then constSymTab 
  else 
	loadGlobalConsts (loadConst constSymTab index) (index + 1) (numConsts - 1)
	  tySkelTab

let loadConstSymTab tySkeletonTab = 
  let numberGConsts = Readutil.readTwoBytes () in
  loadGlobalConsts (Pervasive.pervasiveConstants)
	(Pervasive.numberPervasiveConstants) numberGConsts tySkeletonTab 
  
(****************************************************************************)
(*                       INTERFACE FUNCTION                                 *)
(****************************************************************************)
let loadModuleTable () =
  let modName = loadHeaderInfo () in 
  let (kindSymTab, kindIndexMap) = loadKindSymTab () in
  let tySkeletonTab = loadTypeSkeletons kindIndexMap in
  let constSymTab = loadConstSymTab tySkeletonTab in 
  Absyn.Module(modName, [], [], ref constSymTab, ref kindSymTab, 
			   Table.empty, [], [], [], [], [], ref [], [], [], 
			   ref (Absyn.ClauseBlocks [])) 

