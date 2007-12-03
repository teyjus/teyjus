type pos = Errormsg.pos

type typemolecule =
  Molecule of (Absyn.atype * Absyn.atype list * bool)

type typeandbindings =
  TypeAndBindings of (Absyn.atype * Absyn.atype Table.symboltable)

let get = function
  Some t -> t
| None -> invalid_arg "get: option contains no data"

(*  TypeAndBindings Accessors *)
let getTypeAndBindingsType =
  fun t ->
    (match t with
      TypeAndBindings(t, _) -> t)   

let getTypeAndBindingsBindings = function
  TypeAndBindings(_, bs) -> bs

type typeandenvironment =
  TypeAndEnvironment of (Absyn.atype * int)

(*  TypeAndEnvironment Accessors  *)
let getTypeAndEnvironmentType = function
  TypeAndEnvironment(t, _) -> t
    
let getTypeAndEnvironmentSize = function
  TypeAndEnvironment(_, i) -> i

type argstypes =
  ArgsTypes of (int * Absyn.atype list * Absyn.atype list)


(*  Unification results *)
type unifyresult =
    OccursCheckFailure
  | ClashFailure
  | Success

let typeSkeletonIndex = ref 0

(**********************************************************************
*rationalizeTypeAbbrevVar:
* Used when translating a type variable while translating a typeabbrev
* into abstract syntax.
**********************************************************************)
let rationalizeTypeAbbrevVar =
  fun sym symtable p ->
    (Errormsg.error p ("unbound variable " ^ (Symbol.name sym) ^ " in type abbreviation");
    TypeAndBindings(Absyn.ErrorType, symtable))

(**********************************************************************
*rationalizeSkeletonVar:
* Used when translating a type variable while translating a type into
* a type skeleton. 
**********************************************************************)
let rationalizeSkeletonVar =
  fun sym symtable p ->
    let t = Absyn.SkeletonVarType(ref !typeSkeletonIndex) in
    (typeSkeletonIndex := !typeSkeletonIndex + 1;
    TypeAndBindings(t, (Table.add sym t symtable)))

(**********************************************************************
*rationalizeVar:
* Used when translating a type variable while translating a type into
* an absyn type.
**********************************************************************)
let rationalizeVar =
  fun sym symtable p ->
    let t = Absyn.makeTypeVariable () in
    TypeAndBindings(t, (Table.add sym t symtable))

(********************************************************************
*rationalizeType:
* Rationalizes a type.
*********************************************************************)
let rec rationalizeType = fun
  ty vartable kindtable typeabbrevtable newsymfunc transvarfunc ->
    
    (******************************************************************
    *translateArrow:
    * Translate an arrow from preabsyn to absyn.
    ******************************************************************)
    let rec translateArrow = function Preabsyn.Arrow(l,r,p) ->
      let TypeAndBindings(l', ls) = rationalizeType l vartable kindtable typeabbrevtable newsymfunc transvarfunc in
      let TypeAndBindings(r', rs) = rationalizeType r ls kindtable typeabbrevtable newsymfunc transvarfunc in

      TypeAndBindings(Absyn.ArrowType(l', r'), rs)
    | t -> invalid_arg "Types.translateArrow: invalid type"
    in
  
    (******************************************************************
    *translateApp:
    * Translate an application from preabsyn to absyn.
    ******************************************************************)
    let rec translateApp = function Preabsyn.App(f,t,p) ->
      (**************************************************************
      *translateArgs:
      * Gets the arguments as a list instead of a tree.
      **************************************************************)
      let rec translate' = fun t ts ->
        match t with
          Preabsyn.App(f,arg,p') ->
            let TypeAndBindings(argtype, ts') = rationalizeType arg ts kindtable typeabbrevtable newsymfunc transvarfunc in
            let (head, ts'', argtypes) = (translate' f ts') in
            (head, ts'', argtype :: argtypes)
        | _ -> (t, ts, [])
      in

      let (head, ts, args) = translate' ty vartable in
      
      (match head with
        Preabsyn.Atom(sym,k,p) ->
            (match k with
              Preabsyn.VarID ->
                (Errormsg.error p "found type variable, expected a constructor";
                TypeAndBindings(Absyn.ErrorType, vartable))
            | Preabsyn.AVID ->
                (Errormsg.error p "found type variable, expected a constructor";
                TypeAndBindings(Absyn.ErrorType, vartable))
            | Preabsyn.CVID ->
                (Errormsg.error p "found type variable, expected a constructor";
                TypeAndBindings(Absyn.ErrorType, vartable))
            | _ ->
              (match (Table.find sym vartable) with
                Some t ->
                  (Errormsg.error p "found type variable, expected a constructor";
                  TypeAndBindings(Absyn.ErrorType, vartable))
              | None ->
                  (match (Table.find sym kindtable) with
                    Some k ->
                      if (Absyn.getKindArity k) <> (List.length args) then
                        (Errormsg.error p ("type constructor " ^ (Symbol.name sym) ^ " has arity " ^ (string_of_int (Absyn.getKindArity k)) ^ ", but given " ^ (string_of_int (List.length args)) ^ " arguments");
                        TypeAndBindings(Absyn.ErrorType, vartable))
                      else
                        TypeAndBindings(Absyn.ApplicationType(k, args), ts)
                  | None ->
                        (match (Table.find sym typeabbrevtable) with
                          Some t ->
                            (translateTypeAbbrevCall t args ts p)
                        | None ->
                          (Errormsg.error p ("undeclared constructor " ^ (Symbol.name sym));
                          TypeAndBindings(Absyn.ErrorType, vartable))))))
      | _ ->
        (Errormsg.error p "expected a constructor");
        TypeAndBindings(Absyn.ErrorType, vartable))
    | t -> invalid_arg "Types.translateApp: invalid type"
    in  
    match ty with
      Preabsyn.Atom(s, Preabsyn.AVID, pos) ->
        (transvarfunc s vartable pos)
    | Preabsyn.Atom(s, Preabsyn.VarID, pos) ->
        (**************************************************************
        * If the variable is in the variable table,
        * just return the type associated with it.  Otherwise,
        * create a new one.
        **************************************************************)
        (match (Table.find s vartable) with
          Some t -> TypeAndBindings(t, vartable)
        | None -> transvarfunc s vartable pos)
    | Preabsyn.Atom(s, Preabsyn.CVID, pos) ->
        (match (Table.find s vartable) with
          Some t -> TypeAndBindings(t, vartable)
        | None -> transvarfunc s vartable pos)
    | Preabsyn.Atom(s, _, pos) ->
        (match (Table.find s kindtable) with
          Some k ->
            if (Absyn.getKindArity k) <> 0 then
              (Errormsg.error pos ("type constructor has arity " ^
                (string_of_int (Absyn.getKindArity k)));
              TypeAndBindings(Absyn.ErrorType, vartable))
            else
              TypeAndBindings(Absyn.ApplicationType(k, []), vartable)
        | None ->
          (Errormsg.error pos ("undeclared constant '" ^ (Symbol.name s) ^ "'");
          TypeAndBindings(Absyn.ErrorType, vartable)))
        
    | Preabsyn.App(f, t, p) ->
        translateApp ty

    | Preabsyn.Arrow(l, r, p) ->
        translateArrow ty

    | Preabsyn.ErrorType -> TypeAndBindings(Absyn.ErrorType, vartable)

(**********************************************************************
*translateType:
* Translate a preabsyn representation of a type into an absyn type.
**********************************************************************)
and translateType = fun ty amodule ->
  let newSymFunc = fun () -> () in
  let TypeAndBindings(t, _) = (rationalizeType ty Table.empty
    (Absyn.getModuleKindTable amodule) (Absyn.getModuleTypeAbbrevTable amodule)
    newSymFunc rationalizeVar) in
  t

and translateType' = fun ty kindtable typeabbrevtable ->
  let newSymFunc = fun () -> () in
  let TypeAndBindings(t, _) = (rationalizeType ty Table.empty
    kindtable typeabbrevtable newSymFunc rationalizeVar) in
  t

(**********************************************************************
*translateTypeSkeleton:
* Translate a preabsyn representation of a type into an absyn type
* skeleton.
**********************************************************************)
and translateTypeSkeleton = fun ty kindtable typeabbrevtable newsymfunc ->
  (*********************************************************************
  *translateArrow:
  * Translate an arrow, with a check to ensure 
  *********************************************************************)
  let translateArrow = function Preabsyn.Arrow(l,r,pos) ->
    (*******************************************************************
    *getTarget:
    * Get the target of an arrow type.
    *******************************************************************)
    let rec getTarget = fun t ->
      match t with
        Preabsyn.Arrow(l,r,p) -> getTarget r
      | _ -> t
    in
    
    (*******************************************************************
    *getArgs:
    * Get the arguments of an arrow type in list form.
    *******************************************************************) 
    let rec getArgs = fun t ->
      match t with
        Preabsyn.Arrow(l, r, p) -> l::(getArgs r)
      | _ -> []
    in
    
    (*******************************************************************
    *translateArgs:
    * Takes a list of arguments, and translates them.  Passes along the
    * environment.
    *******************************************************************)
    let rec translateArgs = fun args ts ->
      match args with
        arg::rest ->
          let TypeAndBindings(t, ts') = rationalizeType arg ts kindtable typeabbrevtable newsymfunc rationalizeSkeletonVar in
          t::(translateArgs rest ts')
      | [] -> []
    in
    
    (******************************************************************
    *buildArrow:
    * Takes a list of arguments and a target type and constructs a
    * tree of arrow types.  Pretty sure this function exists somewhere
    * else...
    ******************************************************************)
    let rec buildArrow target args =
      match args with
        arg::rest -> Absyn.ArrowType(arg, buildArrow target rest)
      | [] -> target
    in

    (*  Get the argument and target parts *)
    let target = getTarget r in
    let args = getArgs ty in
    
    (*  First translate the target.  *)
    let TypeAndBindings(target', ts) = rationalizeType target Table.empty kindtable typeabbrevtable newsymfunc rationalizeSkeletonVar in

    (*  Translate all of the arguments  *)
    let args' = translateArgs args ts in
    
    (*  Rebuild the arrow type as absyn *)
    TypeAndEnvironment(buildArrow target' args', !typeSkeletonIndex)
    
  | t -> invalid_arg "Types.translateArrow: invalid type"
  in
  
  let _ = typeSkeletonIndex := 0 in
  
  match ty with
    Preabsyn.Arrow(l,r,pos) ->
      translateArrow ty
  | _ ->
    let TypeAndBindings(t, ts) = rationalizeType ty Table.empty kindtable typeabbrevtable newsymfunc rationalizeSkeletonVar in
    TypeAndEnvironment(t, !typeSkeletonIndex) 

(**********************************************************************
*translateFixities:
* Translate a list of fixity declarations and a constant list into an
* updated constant list.
**********************************************************************)
and translateFixities fixities constants =
  (********************************************************************
  *translate':
  * Translate an individual fixity declaration.  Update the constant
  * table.
  ********************************************************************)
  let translate' f ctable =
    let rec addFixities syms k prec pos ctable =
      (****************************************************************
      *getFixity:
      * Convert preabsyn fixity to absyn fixity.
      ****************************************************************)
      let getFixity k =
        match k with
          Preabsyn.Infix(p) -> Absyn.Infix
        | Preabsyn.Infixl(p) -> Absyn.Infixl
        | Preabsyn.Infixr(p) -> Absyn.Infixr
        | Preabsyn.Prefix(p) -> Absyn.Prefix
        | Preabsyn.Prefixr(p) -> Absyn.Prefixr
        | Preabsyn.Postfix(p) -> Absyn.Postfix
        | Preabsyn.Postfixl(p) -> Absyn.Postfixl
      in
      
      let getFixityArity k =
        match k with
          Preabsyn.Infix(p)
        | Preabsyn.Infixl(p)
        | Preabsyn.Infixr(p) -> 2
        | Preabsyn.Prefix(p)
        | Preabsyn.Prefixr(p)
        | Preabsyn.Postfix(p)
        | Preabsyn.Postfixl(p) -> 1
      in
      
      let rec getTypeArity = function
          Absyn.ArrowType(_,r) -> 1 + (getTypeArity r)
        | _ -> 0
      in
      
      match syms with
        [] -> ctable
      | Preabsyn.Symbol(sym,_,pos)::ss ->
          (match Table.find sym ctable with
            Some c ->
              let skel = (Absyn.getConstantSkeleton c) in
              let fix = Absyn.getConstantFixityRef c in
              let prec' = Absyn.getConstantPrecRef c in
              let pos' = Absyn.getConstantPos c in
              
              if Option.isSome skel then
                let t' = Absyn.getSkeletonType (Option.get skel) in
                if (getFixityArity k) <> (getTypeArity t') then
                  (Errormsg.error pos ("declared fixity is incompatible with declared type arity" ^
                    (Errormsg.see pos' "constant declaration"));
                  ctable)
                else if not (checkPrec !prec' prec) then
                  (Errormsg.error pos
                    ("constant " ^ (Symbol.name sym) ^ " already declared with precedence " ^
                    (string_of_int !prec') ^
                    (Errormsg.see pos' "constant declaration"));
                  ctable)
                else if not (checkFixity !fix (getFixity k)) then
                  (Errormsg.error pos
                    ("constant " ^ (Symbol.name sym) ^ " already declared with fixity " ^
                    (Absyn.string_of_fixity !fix) ^
                    (Errormsg.see pos' "constant declaration"));
                  ctable)
                else
                  (fix := (getFixity k);
                  prec' := prec;
                  ctable)
              else
                if not (checkPrec !prec' prec) then
                  (Errormsg.error pos
                    ("constant " ^ (Symbol.name sym) ^ " already declared with precedence " ^
                    (string_of_int !prec') ^
                    (Errormsg.see pos' "constant declaration"));
                  ctable)
                else if not (checkFixity !fix (getFixity k)) then
                  (Errormsg.error pos
                    ("constant " ^ (Symbol.name sym) ^ " already declared with fixity " ^
                    (Absyn.string_of_fixity !fix) ^
                    (Errormsg.see pos' "constant declaration"));
                  ctable)
                else
                  (fix := (getFixity k);
                  prec' := prec;
                  ctable)
          | None ->
              (Errormsg.error pos ("fixity declaration: undeclared constant " ^
                (Symbol.name sym));
              ctable))
    in
    match f with
      Preabsyn.Fixity(syms, k, prec, pos) -> addFixities syms k prec pos ctable
  in
  
  match fixities with
    [] -> constants
  | f::fs ->
      let constants' = translateFixities fs constants in
      (translate' f constants')

(**********************************************************************
*translateLocalKinds:
**********************************************************************)
and buildLocalKind sym arity pos =
  Absyn.Kind(sym, arity, ref 0, Absyn.LocalKind, pos)
  
and translateLocalKinds kinds =
  translateKinds kinds buildLocalKind

(**********************************************************************
*translateGlobalKinds:
**********************************************************************)
and buildGlobalKind sym arity pos =
  Absyn.Kind(sym, arity, ref 0, Absyn.GlobalKind, pos)

and translateGlobalKinds kinds =
  translateKinds kinds buildGlobalKind

(**********************************************************************
*translateKinds:
* Translate a list of kinds in preabstract syntax to a list of kinds
* in abstract syntax.
**********************************************************************)
and translateKinds = fun klist buildkind -> 
  let rec translate' = fun klist result ->
    match klist with
      [] -> result
    | k::ks ->
        let result' = (translateKind k buildkind result) in
        (translate' ks result')
  in
  (translate' klist [])

(**********************************************************************
*translateKind:
**********************************************************************)
and translateKind = fun kind buildkind klist ->
  (********************************************************************
  *addKind:
  * Given appropriate kind information, uses the kind building function
  * passed to translateKind to construct a kind, and then adds it to the
  * list.
  ********************************************************************)
  let rec addKind = fun syms a pos result ->
    (match syms with
      [] -> result
    | Preabsyn.Symbol(sym,_,_)::ss -> 
        let result' = (buildkind sym a pos) :: result in
        (addKind ss a pos result'))
  in
  
  match kind with
    Preabsyn.Kind(syms, a, pos) ->
      (addKind syms a pos klist)


(**********************************************************************
*translateGlobalConstants:
* Constructs a function to build a constant of Global kind, and
* translates all constants using it.
**********************************************************************)
and buildGlobalConstant = fun sym ty tyskel esize pos ->
  Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref false, ref false,
    ref false, ref true, ref false, ref false, tyskel,
    ref esize, ref (Some(Array.make esize true)), ref (Some(Array.make esize true)),
    ref None, ref Absyn.GlobalConstant, ref 0, pos)

and translateGlobalConstants clist kindtable typeabbrevtable =
  translateConstants clist kindtable typeabbrevtable buildGlobalConstant

(**********************************************************************
*translateLocalConstants:
**********************************************************************)
and buildLocalConstant = fun sym ty tyskel esize pos ->
  Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref false, ref false,
    ref false, ref true, ref false, ref false, tyskel,
    ref esize, ref (Some(Array.make esize true)), ref (Some(Array.make esize true)),
    ref None, ref Absyn.LocalConstant, ref 0, pos)
    
and translateLocalConstants clist kindtable typeabbrevtable =
  translateConstants clist kindtable typeabbrevtable buildLocalConstant

(**********************************************************************
*translateUseOnlyConstants:
**********************************************************************)
and translateUseOnlyConstants owner clist kindtable typeabbrevtable =
  let constantType = if owner then Absyn.GlobalConstant else Absyn.LocalConstant in
  let buildConstant = fun sym ty tyskel esize pos ->
    Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref false, ref true,
      ref true, ref true, ref false, ref false, tyskel,
      ref esize, ref (Some(Array.make esize true)), ref (Some(Array.make esize true)),
      ref None, ref constantType, ref 0, pos)
  in
  translateConstants clist kindtable typeabbrevtable buildConstant

(**********************************************************************
*translateExportdefConstants:
**********************************************************************)
and translateExportdefConstants owner clist kindtable typeabbrevtable =
  let constantType = if owner then Absyn.GlobalConstant else Absyn.LocalConstant in
  let buildConstant = fun sym ty tyskel esize pos ->
    Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref true, ref false,
      ref (not owner), ref true, ref false, ref false, tyskel,
      ref esize, ref (Some(Array.make esize true)), ref (Some(Array.make esize true)),
      ref None, ref constantType, ref 0, pos)
  in
  translateConstants clist kindtable typeabbrevtable buildConstant

(**********************************************************************
*translateClosedConstants:
**********************************************************************)
and translateClosedConstants clist kindtable typeabbrevtable =
  let buildConstant = fun sym ty tyskel esize pos ->
    Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref false, ref false,
      ref false, ref true, ref false, ref false, tyskel,
      ref esize, ref (Some(Array.make esize true)), ref (Some(Array.make esize true)),
      ref None, ref Absyn.GlobalConstant, ref 0, pos)
  in
  translateConstants clist kindtable typeabbrevtable buildConstant

(********************************************************************
*translateConstants:
* Translates a list of constant declarations in preabsyn
* representation into a constant table.
********************************************************************)
and translateConstants clist kindtable typeabbrevtable buildconstant =
  let rec translate' clist result =
    match clist with
      c::cs ->
        let result' = (translateConstant c result kindtable typeabbrevtable buildconstant) in
        translate' cs result'
    | [] -> result
  in
  translate' clist []

(**********************************************************************
*translateConstant:
* Translate a preabsyn constant into an absyn constant and enter it
* into a table.
**********************************************************************)
and translateConstant = fun c clist kindtable typeabbrevtable buildconstant ->
  (********************************************************************
  *translate':
  * Enter all names into table.
  ********************************************************************)
  let rec enter = fun names ty tyskel esize clist ->
    match names with
      Preabsyn.Symbol(name,_,p)::ns ->
        let clist' = (buildconstant name ty tyskel esize p) :: clist in
        (enter ns ty tyskel esize clist')
    | [] -> clist
  in
  
  let rec newSymFunc = fun () -> ()
  in

  match c with
    Preabsyn.Constant(names, Some t, pos) ->
      let TypeAndEnvironment(tyskel, size) = translateTypeSkeleton t kindtable typeabbrevtable newSymFunc in
      let ty = translateType' t kindtable typeabbrevtable in
      (enter names [ty] (ref(Some(Absyn.Skeleton(tyskel, ref None, ref false)))) size clist)
  | Preabsyn.Constant(names, None, pos) ->
      (enter names [] (ref None) 0 clist)

(********************************************************************
*translateTypeAbbrevs:
* Translates a list of type abbreviations in preabsyn representation
* into a type abbreviation table.
********************************************************************)
and translateTypeAbbrevs = fun tabbrevs kindtable -> 
  let rec translate' = fun tlist abbrevtable ->
    match tlist with
      [] -> abbrevtable
    | t::ts ->
        let abbrevtable' = translateTypeAbbrev t abbrevtable kindtable in
        translate' ts abbrevtable'
  in
  translate' tabbrevs Table.empty

(********************************************************************
*translateTypeAbbrev:
* Translate a type abbreviation from preabsyn to absyn.
********************************************************************)
and translateTypeAbbrev =
  fun abbrev abbrevtable kindtable ->
    let Preabsyn.TypeAbbrev(name, arglist, ty, pos) = abbrev in
    
    (****************************************************************
    *getName:
    ****************************************************************)
    let getName = function
      Preabsyn.Symbol(n,Preabsyn.ConstID,p) -> n
    | Preabsyn.Symbol(n,k,p) -> (Errormsg.error p "type abbreviation: expected abbreviation name";
                                n)
    in
    
    (****************************************************************
    *checkArgs:
    ****************************************************************)
    let rec checkArgs = function
      [] -> []
    | Preabsyn.Symbol(n,Preabsyn.CVID,p)::ss -> n::(checkArgs ss)
    | Preabsyn.Symbol(n,k,p)::ss -> (Errormsg.error p "type abbreviation: expected argument name";
                                    [])
    in
    
    (****************************************************************
    *buildTable:
    ****************************************************************)
    let rec buildTable = fun syms i ->
      match syms with
        [] -> Table.empty
      | sym::ss -> (Table.add sym (Absyn.SkeletonVarType(ref i)) (buildTable ss (i + 1)))
    in
    
    (******************************************************************
    *newSymFunc:
    * If a new symbol is encountered when rationalizing a type while
    * parsing type abbreviations, it is an error.
    ******************************************************************)
    let newSymFunc = fun () -> () 
    in

    
    (*  Get the name and arguments  *)
    let abbrevname = getName name in
    let args = checkArgs arglist in
    
    (*  Build a symbol table of the args  *)
    let symtable = buildTable args 0 in
    
    (*  Translate the type body *)
    let TypeAndBindings(bodytype,bindings) = (rationalizeType ty symtable kindtable abbrevtable newSymFunc rationalizeTypeAbbrevVar) in
    
    (Table.add abbrevname (Absyn.TypeAbbrev(abbrevname, args, bodytype, pos)) abbrevtable)

(********************************************************************
*translateTypeAbbrevCall:
* Given a variable table and arguments, instantiates a type abbrev.
********************************************************************)
and translateTypeAbbrevCall = fun abbrev args vartable pos ->
  let Absyn.TypeAbbrev(name, syms, target, pos') = abbrev in
  
  let rec replaceArg = fun argnum a t ->
    match t with
      Absyn.ArrowType(l,r) -> Absyn.ArrowType(replaceArg argnum a l, replaceArg argnum a r)
    | Absyn.ApplicationType(k,tlist) ->
        let tlist' = List.map (replaceArg argnum a) tlist in
        Absyn.ApplicationType(k, tlist')
    | Absyn.TypeVarType(_) -> t
    | Absyn.TypeSetType(_) -> t
    | Absyn.SkeletonVarType(i) ->
        if !i = argnum then
          a
        else
          t
    | Absyn.ErrorType -> Absyn.ErrorType
  in
  
  (*  Replaces each argument placeholder with a real type *)
  let rec replaceAll = fun argnum args target ->
    match args with
      arg::aa ->
        let target' = replaceArg argnum arg target in
        (replaceAll (argnum + 1) aa target')
    | [] ->
        target
  in
  
  if (List.length syms) <> (List.length args) then
    (Errormsg.error pos ("typeabbrev expected " ^ (string_of_int (List.length syms)) ^ " arguments" ^
      (Errormsg.see pos' "typeabbrev declaration"));
    TypeAndBindings(Absyn.ErrorType, vartable))
  else
    TypeAndBindings((replaceAll 0 args target), vartable)

(******************************************************************
*mergeTypeAbbrevs:
******************************************************************)
and mergeTypeAbbrevs t1 t2 =
  let merge = fun sym tabbrev table ->
    let Absyn.TypeAbbrev(s, args, ty, p) = tabbrev in
    
    match (Table.find sym table) with
      Some Absyn.TypeAbbrev(s', args', ty', p') ->
        if args <> args' then
          (Errormsg.error p "typeabbrev already declared with different arguments";
          table)
        else if ty <> ty' then
          (Errormsg.error p "typeabbrev already declared with different type";
          table)
        else
          table
    | None ->
      (Table.add sym tabbrev table)
  in
  (Table.fold merge t1 t2)

(**********************************************************************
*compareConstants:
* Determines whether two constants are defined in compatible ways.
**********************************************************************)
and compareConstants c1 c2 =
  let name = Absyn.getConstantName c1 in
  
  let fix = Absyn.getConstantFixity c1 in
  let fix' = Absyn.getConstantFixity c2 in
  
  let prec = Absyn.getConstantPrec c1 in
  let prec' = Absyn.getConstantPrec c2 in
  
  let skel = Absyn.getConstantSkeleton c1 in
  let skel' = Absyn.getConstantSkeleton c2 in
  
  let p = Absyn.getConstantPos c1 in
  let p' = Absyn.getConstantPos c2 in
  
  
  if not (checkFixity fix fix') then
    (Errormsg.error p ("constant already declared with fixity " ^
      (Absyn.string_of_fixity fix') ^
      (Errormsg.see p' "constant declaration"));
    false)
  else if not (checkPrec prec prec') then
    (Errormsg.error p ("constant '" ^ name ^ "' already declared with precedence" ^
      (string_of_int prec') ^ (Errormsg.see p' "constant declaration"));
    false)
  else if (Option.isSome skel) && (Option.isSome skel') &&
    (skel <> skel') then
    (Errormsg.error p ("constant '" ^ name ^ "' declared with incompatible type" ^
      (Errormsg.see p' "constant declaration"));
    false)
  else
    true

(**********************************************************************
*compareAccumConstants:
* Determines whether two constants are defined in compatible ways.
**********************************************************************)
and compareAccumConstants c1 c2 =
  let name = Absyn.getConstantName c1 in

  let fix = Absyn.getConstantFixity c1 in
  let fix' = Absyn.getConstantFixity c2 in
  
  let prec = Absyn.getConstantPrec c1 in
  let prec' = Absyn.getConstantPrec c2 in
  
  let skel = Absyn.getConstantSkeleton c1 in
  let skel' = Absyn.getConstantSkeleton c2 in
  
  let p = Absyn.getConstantPos c1 in
  let p' = Absyn.getConstantPos c2 in
    
  (*
  let ed = Absyn.getConstantExportDef c1 in
  let ed' = Absyn.getConstantExportDef c2 in
  *)
  if not (checkFixity fix fix') then
    (Errormsg.error p ("constant already declared with fixity " ^
      (Absyn.string_of_fixity fix') ^
      (Errormsg.see p' "constant declaration"));
    false)
  else if not (checkPrec prec prec') then
    (Errormsg.error p ("constant already declared with precedence" ^
    (string_of_int prec'));
    false)
  else if (Option.isSome skel) && (Option.isSome skel') &&
    (skel <> skel') then
    (Errormsg.error p' ("constant '" ^ name ^ "' declared with incompatible type" ^
      (Errormsg.see p "constant declaration"));
    false)
  else
    true
(**********************************************************************
*checkKindArities:
* Ensures that all constants have arities.
**********************************************************************)
and checkKindArities ktable =
  let result = ref true in
  let check s k =
    let a = Absyn.getKindArityOption k in
    if Option.isSome a then
      ()
    else
      result := false
  in
  let _ = Table.iter check ktable in
  !result

(**********************************************************************
*checkConstantBodies:
* Ensures that all constants have skeletons.
**********************************************************************)
and checkConstantBodies ctable =
  let result = ref true in
  let check s c =
    let p = (Absyn.getConstantPos c) in
    let name = (Absyn.getConstantName c) in
    match (Absyn.getConstantSkeleton c) with
      Some skel ->
        (*  Check that the type declared here is correct for
        useonly and exportdef constants.  *)
        let ty = Absyn.getSkeletonType skel in
        if (Absyn.getConstantUseOnly c) && (Absyn.getConstantExportDef c) then
          (Errormsg.error p
            ("constant '" ^ name ^ "' declared as both exportdef and useonly");
          result := false)
        else if (Absyn.getConstantUseOnly c) || (Absyn.getConstantExportDef c) then
          let sort = if (Absyn.getConstantUseOnly c) then "useonly" else "exportdef" in
          if not (Absyn.isArrowType ty) then
            (Errormsg.error p
              ("constant '" ^ name ^ "' declared as " ^ sort ^ " without target type 'o'");
            result := false)
          else
            let targ = Absyn.getArrowTypeTarget ty in
            if (Absyn.isConstantType targ) &&
              (Pervasive.iskbool (Absyn.getTypeKind targ)) then
              ()
            else
              (Errormsg.error p
                ("constant '" ^ name ^ "' declared as " ^ sort ^ " without target type 'o'");
              result := false)              
        else
          ()
    | None ->
        (Errormsg.error p
          ("constant '" ^ name ^ "' declared without type");
        result := false)
  in
  
  let _ = Table.iter check ctable in
  !result

(**********************************************************************
*checkFixity:
* Checks whether two fixities are compatible.  If the fixities are
* equal, the check is true, and if either is not yet defined, it is
* true.
**********************************************************************)
and checkFixity f1 f2 =
  (f1 = f2 || (f1 = Absyn.NoFixity || f2 = Absyn.NoFixity))

(**********************************************************************
*checkPrec:
* Checks whether two precedences are compatible.
**********************************************************************)
and checkPrec = fun f1 f2 ->
  (f1 = f2 || (f1 = -1 || f2 = -1))

(**********************************************************************
*mergeConstants:
* Merges a list of constants into a table of constants, based on the
* options specified.
**********************************************************************)
and mergeConstants clist ctable f =
  let merge table constant =
    let sym = Absyn.getConstantSymbol constant in
    let constant' = Table.find sym ctable in
    (f constant' constant table)
  in
  List.fold_left merge ctable clist

(**********************************************************************
*mergeKinds:
* Merges a list of kinds into a table of kinds, based on the
* options specified.
**********************************************************************)
and mergeKinds klist ktable f =
  let merge table kind =
    let sym = Absyn.getKindSymbol kind in
    let kind' = Table.find sym ktable in
    (f kind' kind ktable)
  in
  List.fold_left merge ktable klist

(**********************************************************************
*previouslyExists:
* Checks that the constant already existed.
**********************************************************************)
and previouslyExists =
  fun currentConstant newConstant ->
    if Option.isSome currentConstant then
      let c = Option.get currentConstant in
      not(Absyn.getConstantRedefinable c)
    else
      false

(**********************************************************************
*hasSkeleton:
* Ensures that the new constant has a skeleton.
**********************************************************************)
and hasSkeleton =
  fun currentConstant newConstant ->
    Option.isSome (Absyn.getConstantSkeleton newConstant)

(**********************************************************************
*mustCompare:
* Requires that the two constants are compatible.
**********************************************************************)
and mustCompare =
  fun currentConstant newConstant ->
    if (Option.isSome currentConstant) then
      compareConstants (Option.get currentConstant) newConstant
    else
      Errormsg.impossible Errormsg.none "Translate.mustCompare: invalid constant"

(**********************************************************************
*ifThenElse:
**********************************************************************)
and ifThenElse test ifBranch elseBranch =
  fun currentConstant newConstant ctable ->
    if (test currentConstant newConstant) then
      ifBranch currentConstant newConstant ctable
    else
      elseBranch currentConstant newConstant ctable

and success = fun _ _ ctable -> ctable
and failure = fun _ _ ctable -> ctable

and error s = fun currentConstant newConstant ctable ->
  let p = Absyn.getConstantPos newConstant in
  let s' = "constant '" ^ (Absyn.getConstantName newConstant) ^ "' " ^ s in
  if Option.isSome currentConstant then
    let p' = Absyn.getConstantPos (Option.get currentConstant) in
    (Errormsg.error p
      (s' ^ (Errormsg.see p' "constant declaration"));
    ctable)
  else
    (Errormsg.error p s';
    ctable)

and log s = fun currentConstant newConstant ctable ->
  let p = Absyn.getConstantPos newConstant in
  let s' = "constant '" ^ (Absyn.getConstantName newConstant) ^ "' " ^ s in
  if Option.isSome currentConstant then
    let p' = Absyn.getConstantPos (Option.get currentConstant) in
    (Errormsg.log p
      (s' ^ (Errormsg.see p' "constant declaration"));
    ctable)
  else
    (Errormsg.log p s';
    ctable)

and orElse left right =
  fun currentConstant newConstant ->
    (left currentConstant newConstant) || (right currentConstant newConstant)

and andAlso args =
  fun currentConstant newConstant ->
    let forall f = (f currentConstant newConstant) in
    List.for_all forall args
    
and seq functions =
  fun currentConstant newConstant ctable ->
    let folder ctable f =
      (f currentConstant newConstant ctable)
    in
    List.fold_left folder ctable functions

(**********************************************************************
*doesNot:
**********************************************************************)
and doesNot test =
  fun currentConstant newConstant ->
    not (test currentConstant newConstant)

(**********************************************************************
*copySkeleton:
* Copies the skeleton from the new constant to the old.
**********************************************************************)
and copySkeleton =
  fun currentConstant newConstant ctable ->
    let skelref = Absyn.getConstantSkeletonRef currentConstant in
    let newskel = Absyn.getConstantSkeleton newConstant in
    (skelref := newskel;
    ctable)
  
(**********************************************************************
*enterConstant:
**********************************************************************)
and enterConstant f =
  fun currentConstant newConstant ctable ->
    let sym = Absyn.getConstantSymbol newConstant in
    let ctable' = Table.add sym newConstant ctable in
    f (Some newConstant) newConstant ctable'

and hasCurrentConstantType t =
  fun currentConstant newConstant ->
    if (Option.isSome currentConstant) then
      (Absyn.getConstantType (Option.get currentConstant)) = t
    else
      Errormsg.impossible Errormsg.none
        "Translate.hasCurrentConstantType: invalid current constant"

and hasCurrentConstantUseonly v =
  fun currentConstant newConstant ->
    if (Option.isSome currentConstant) then
      (Absyn.getConstantUseOnly (Option.get currentConstant)) = v
    else
      Errormsg.impossible Errormsg.none
        "Translate.hasCurrentConstantUseonly: invalid current constant"

and hasCurrentConstantExportdef v =
  fun currentConstant newConstant ->
    if (Option.isSome currentConstant) then
      (Absyn.getConstantExportDef (Option.get currentConstant)) = v
    else
      Errormsg.impossible Errormsg.none
        "Translate.hasCurrentConstantExportdef: invalid current constant"

and setCurrentConstantClosed v =
  fun currentConstant newConstant ctable ->
    if (Option.isSome currentConstant) then
      let cref = Absyn.getConstantClosedRef (Option.get currentConstant) in
      (cref := v;
      ctable)
    else
      Errormsg.impossible Errormsg.none
        "Translate.setCurrentConstantClosed: invalid current constant"

and setCurrentConstantUseonly v =
  fun currentConstant newConstant ctable ->
    if (Option.isSome currentConstant) then
      let cref = Absyn.getConstantUseOnlyRef (Option.get currentConstant) in
      (Errormsg.log Errormsg.none
        ("setting useonly '" ^ (Absyn.getConstantName newConstant) ^
        "' to " ^ (string_of_bool v));
      cref := v;
      ctable)
    else
      Errormsg.impossible Errormsg.none
        "Translate.setCurrentConstantUseonly: invalid current constant"

and setCurrentConstantExportdef v =
  fun currentConstant newConstant ctable ->
    if (Option.isSome currentConstant) then
      let cref = Absyn.getConstantExportDefRef (Option.get currentConstant) in
      (Errormsg.log Errormsg.none
        ("setting exportdef '" ^ (Absyn.getConstantName newConstant) ^
        "' to " ^ (string_of_bool v));
      cref := v;
      ctable)
    else
      Errormsg.impossible Errormsg.none
        "Translate.setCurrentConstantExportDef: invalid current constant"

(**********************************************************************
*setCurrentConstantType:
**********************************************************************)
and setCurrentConstantType t =
  fun currentConstant newConstant ctable ->
    if Option.isSome currentConstant then
      let tyref = Absyn.getConstantTypeRef (Option.get currentConstant) in
      (tyref := t;
      ctable)
    else
      Errormsg.impossible Errormsg.none
        "Translate.setCurrentConstantType: invalid current constant"

and copyConstant =
  fun currentConstant newConstant ctable ->
    if (Option.isSome currentConstant) then
      let currentConstant = Option.get currentConstant in
      let tyref = Absyn.getConstantTypeRef currentConstant in
      let skelref = Absyn.getConstantSkeletonRef currentConstant in
      let uoref = Absyn.getConstantUseOnlyRef currentConstant in
      let edref = Absyn.getConstantExportDefRef currentConstant in
      let closedref = Absyn.getConstantClosedRef currentConstant in
      (Errormsg.log Errormsg.none
        ("set exportdef '" ^ (Absyn.getConstantName newConstant) ^ "' to " ^ (string_of_bool !edref));
      tyref := Absyn.getConstantType newConstant;
      skelref := Absyn.getConstantSkeleton newConstant;
      uoref := Absyn.getConstantUseOnly newConstant;
      edref := Absyn.getConstantExportDef newConstant;
      closedref := Absyn.getConstantClosed newConstant;
      ctable)
    else
      Errormsg.impossible Errormsg.none
        "Translate.copyConstant: invalid current constant"
        
(*  Copiers: they need an explanation.   *)
(*  Really merging should be handled as it is in translateModule, duh.  *)
let copy currentConstant newConstant =
  let () = Errormsg.log Errormsg.none
    ("copying constant '" ^ (Absyn.getConstantName newConstant) ^ "'") in
  let skelref = Absyn.getConstantSkeletonRef currentConstant in
  let sizeref = Absyn.getConstantTypeEnvSizeRef currentConstant in
  let tref = Absyn.getConstantTypeRef currentConstant in
  let () = tref := Absyn.getConstantType newConstant in
  if (Option.isNone !skelref) then
    (sizeref := Absyn.getConstantTypeEnvSize false newConstant;
    skelref := Absyn.getConstantSkeleton newConstant)
  else
    ()

let copyAccum currentConstant newConstant =
  let () = Errormsg.log Errormsg.none
    ("copying accum '" ^ (Absyn.getConstantName newConstant) ^ "'") in

  let currentType = Absyn.getConstantType currentConstant in
  let () = copy currentConstant newConstant in
  (Absyn.getConstantTypeRef currentConstant) := currentType

let copyExportdef generalCopier owner currentConstant newConstant =
  let () = Errormsg.log Errormsg.none
    ("copying exportdef '" ^ (Absyn.getConstantName newConstant) ^ "'") in
  let () = generalCopier currentConstant newConstant in
  
  let edref = Absyn.getConstantExportDefRef currentConstant in
  let nodefsref = Absyn.getConstantNoDefsRef currentConstant in
  (edref := true;
  nodefsref := not owner)

let copyUseonly generalCopier owner currentConstant newConstant =
  let () = Errormsg.log Errormsg.none
    ("copying useonly '" ^ (Absyn.getConstantName newConstant) ^ "'") in
  let () = generalCopier currentConstant newConstant in
  let uoref = Absyn.getConstantUseOnlyRef currentConstant in
  let nodefsref = Absyn.getConstantNoDefsRef currentConstant in
  (Errormsg.log Errormsg.none "setting useonly";
  uoref := true;
  nodefsref := true)


(**********************************************************************
*translate:
* Convert from a preabsyn module to an absyn module.
**********************************************************************)
let rec translate mod' sig' =
    let (asig, (ktable, ctable, atable)) =
      translateSignature sig' true true Pervasive.pervasiveKinds
        Pervasive.pervasiveConstants Pervasive.pervasiveTypeAbbrevs
        buildGlobalKind buildGlobalConstant copy in
    let amod = translateModule mod' ktable ctable atable in
    (amod, asig)

(**********************************************************************
*translateSignature:
* Translates a signature from preabsyn to a set of tables corresponding
* to constants, kinds, and type abbreviations.
* Arguments:
*   s: the signature to parse
*   owner: is this signature the current module's or is it an
*     an accumulated module's signature
*   ktable: the kind table
*   ctabls: the constant table
*   tabbrevtable: the type abbreviation table
*   kbuilder: the kind builder to use
*   cbuilder: the constant builder to use
**********************************************************************)
and translateSignature s owner accumOrUse ktable ctable tabbrevtable
  kbuilder cbuilder generalCopier =
  match s with
    Preabsyn.Module(_) ->
      (Errormsg.impossible
        Errormsg.none
        "Translate.translateSignature: expected Preabsyn.Signature.")
  | Preabsyn.Signature(name, gconsts, uconsts, econsts, gkinds, tabbrevs,
      fixities,accumsigs,usesigs) ->

  (******************************************************************
  *mergeKinds:
  * Adds the kinds from one signature into the kinds
  * of all signatures.
  ******************************************************************)
  let mergeKinds = fun klist kt ->
    let merge = fun (ktable, renamingList) kind ->
      match kind with
        Absyn.Kind(s, Some a, _, Absyn.GlobalKind, p) ->
          (*  If the kind is already in the table, match the arity.
              Otherwise, add it to the table. *)
	        let kindInTab = Table.find s ktable in
            (match kindInTab with
              Some Absyn.Kind(s', Some a', _, Absyn.GlobalKind, p') ->
                if a <> a' then
                  (Errormsg.error p ("kind already declared with arity " ^
				            (string_of_int a') ^ (Errormsg.see p' "kind declaration"));
                    (ktable, renamingList))
                else
                  (ktable, (Option.get kindInTab) :: renamingList)
            | Some Absyn.Kind(s', Some a', _, Absyn.PervasiveKind, p') ->
                ((Table.add s kind ktable), kind::renamingList)
            | Some k -> (Errormsg.impossible (Absyn.getKindPos k) ("invalid kind type " ^ (Absyn.string_of_kind k)))
            | None -> (Table.add s kind ktable, kind::renamingList))
      | Absyn.Kind(s, Some a, _, Absyn.LocalKind, p) ->
	        let kindInTab = Table.find s ktable in
	          (match kindInTab with
	            Some Absyn.Kind(s', Some a', _, Absyn.GlobalKind, p') ->
	              if a <> a' then
                  (Errormsg.error p ("kind already declared with arity " ^
				            (string_of_int a') ^ (Errormsg.see p' "kind declaration"));
                  (ktable, renamingList))
                else
                  (ktable, (Option.get kindInTab) :: renamingList)
            | Some Absyn.Kind(s', Some a', _, Absyn.LocalKind, p') ->
                if a <> a' then
                  (Errormsg.error p ("kind already declared with arity " ^
	                  (string_of_int a') ^ (Errormsg.see p' "kind declaration"));
                    (ktable, renamingList))
                else
                  (ktable, (Option.get kindInTab) :: renamingList)
            | Some Absyn.Kind(s', Some a', _, Absyn.PervasiveKind, p') ->
                (Table.add s kind ktable, kind :: renamingList)
            | Some k -> (Errormsg.impossible (Absyn.getKindPos k) ("invalid kind type " ^ (Absyn.string_of_kind k)))
            | None -> (Table.add s kind ktable, kind :: renamingList))
      | _ -> Errormsg.impossible Errormsg.none "Non-global kind encountered in mergeGlobalKinds"              
    in
    (List.fold_left merge (kt,[]) klist)
  in
  
  (******************************************************************
  *mergeConstants:
  * Adds the constants from one signature into the constants from
  * all accumulated signatures.
  ******************************************************************)
  let mergeConstants clist ctable rename copier =
    let merge (ctable, renamingList) c =
      let s = Absyn.getConstantSymbol c in
      let pos = Absyn.getConstantPos c in
      match (Table.find s ctable) with
        Some c2 ->
          if Absyn.getConstantRedefinable c2 then
            ((Table.add s c ctable), c::renamingList)
          else if owner && not (compareConstants c c2) then
            (ctable, renamingList)
          else if (not owner) && not (compareConstants c c2) then
            (ctable, renamingList)
          else
            let () = copier c2 c in
            let () = Errormsg.log pos
              ("copied constant: " ^ (Absyn.getConstantName c2) ^ ", " ^
                (if (Absyn.getConstantType c2) = Absyn.LocalConstant then
                  "local"
                else
                  "global")) in
            if Absyn.isGlobalConstant c then
	            if Absyn.isGlobalConstant c2 then
	              (ctable, c2::renamingList)
	            else
	              (ctable, c::renamingList) (* c2 must be perv then *)
	          else if Absyn.isPervasiveConstant c2 then
              (ctable, c::renamingList)
	          else (* c2 local or global *)
		          (ctable, c2::renamingList)
      | None ->
          let () = copier c c in
          let () = Errormsg.log pos
              ("self-copied constant: " ^ (Absyn.getConstantName c) ^ ", " ^
                (if (Absyn.getConstantType c) = Absyn.LocalConstant then
                  "local"
                else
                  "global")) in
          ((Table.add s c ctable), c::renamingList)
    in
    (List.fold_left merge (ctable, rename) clist)
  in    

  (******************************************************************)
  (*union lists:                                                    *)
  (******************************************************************)
  let union list1 list2 =
    let rec union_aux list2 result =
      match list2 with
	    [] -> List.rev result
      | (el :: rest) ->
	  if (List.mem el list1) then union_aux rest result
	  else union_aux rest (el::result)
    in
    list1 @ (union_aux list2 [])
  in

  (******************************************************************
  *processAccumSigs:
  * Convert a list of accumulated signature filenames into a list of
  * preabsyn signatures.
  ******************************************************************)
  let rec processSigs = function
    Preabsyn.Symbol(accum,_,_)::rest ->
      (Compile.compileSignature (Symbol.name accum))::(processSigs rest)
  | [] -> []
  in
  
  (******************************************************************
  *translateSigs:
  * Generalizes processing of accumulated and used signatures.
  ******************************************************************)
  let rec translateSigs accum sigs kindRenaming constRenaming ktable ctable atable =
    match sigs with
      s::rest ->	
        let (asig, (ktable', ctable', atable')) =
          translateSignature s true accum ktable ctable atable
            buildGlobalKind buildGlobalConstant copy in
        (translateSigs accum rest 
	        (union kindRenaming (Absyn.getSignatureGlobalKindsList asig))
	        (union constRenaming (Absyn.getSignatureGlobalConstantsList asig))
	        ktable' ctable' atable')
    | [] ->
        (kindRenaming, constRenaming, ktable, ctable, atable)
  in
  
  (******************************************************************
  *translateAccumSigs:
  * Translate all accumulated signatures.  Just reads the signature
  * and imports the appropriate symbols, etc., recursively.
  ******************************************************************)
  let translateAccumSigs = translateSigs accumOrUse in
  
  (******************************************************************
  *translateUseSigs:
  * Translate all used signatures.  This processing is the same as
  * for accumulated signatures, but all exportdef constants are
  * switched to useonly.
  ******************************************************************)
  let rec translateUseSigs = translateSigs false in
    
  (*  Process accumulated signatures: *)
  let sigs = processSigs accumsigs in
  let (accKindRenaming, accConstRenaming, ktable, ctable, tabbrevtable) =
    translateAccumSigs sigs [] [] ktable ctable tabbrevtable 
  in
  
  (*  Process used signatures:  *)
  let sigs = processSigs usesigs in
  let (useKindRenaming, useConstRenaming, ktable, ctable, tabbrevtable) =
    translateUseSigs sigs accKindRenaming accConstRenaming ktable ctable tabbrevtable
  in
  
  (*  Process kinds *)
  let kindlist = translateKinds gkinds kbuilder in
	
  let (ktable, kindRenaming) = mergeKinds kindlist ktable in
  
  (*  Process type abbreviations  *)
  let tabbrevtable = translateTypeAbbrevs tabbrevs ktable in
  let tabbrevtable = mergeTypeAbbrevs tabbrevtable tabbrevtable in
  
  (*  Translate constants *)
  let constantlist = translateConstants gconsts ktable tabbrevtable cbuilder in
  let uconstantlist = translateUseOnlyConstants owner uconsts ktable tabbrevtable in
  let econstantlist =
    if accumOrUse then
      let () = Errormsg.log Errormsg.none "Accuming" in
      translateExportdefConstants owner econsts ktable tabbrevtable
    else
      let () = Errormsg.log Errormsg.none "Using" in
      translateUseOnlyConstants owner econsts ktable tabbrevtable
  in
  
  let (ctable, constRenaming) =
    mergeConstants constantlist ctable [] generalCopier in
  let (ctable, constRenaming) =
    mergeConstants
      uconstantlist ctable constRenaming (copyUseonly generalCopier owner) in
      
  let ecopier = if accumOrUse then copyExportdef else copyUseonly in
  let (ctable, constRenaming) =
    mergeConstants
      econstantlist ctable constRenaming
        (ecopier generalCopier owner)  in

  (*  Translate fixities *)
  let ctable = translateFixities fixities ctable in
 
  (Absyn.Signature(name, union useKindRenaming kindRenaming,
    union useConstRenaming constRenaming),
   (ktable,ctable,tabbrevtable))

(**********************************************************************
*translateModule:
* Translates a module from preabsyn to absyn.
**********************************************************************)
and translateModule mod' ktable ctable atable =
  let _ = Errormsg.log Errormsg.none "Translate.translateModule: Translating module..." in
  (******************************************************************
  *getSkeleton:
  * Used when folding over the constant lists to get all constant
  * skeletons.  Assumes that the constant has a skeleton.
  ******************************************************************)
  let getSkeleton result c =
    let skel = Absyn.getConstantSkeletonValue c in
    skel :: result
  in
  (******************************************************************
  *getGlobalKind:
  * Used when folding over the kind table to collect all global
  * kinds.
  ******************************************************************)
  let getGlobalKind sym k result =
    if Absyn.isGlobalKind k then
      k::result
    else
      result
  in
  (******************************************************************
  *getLocalKind:
  * Used when folding over the kind table to collect all local
  * kinds.
  ******************************************************************)
  let getLocalKind sym k result =
    if Absyn.isLocalKind k then
      k::result
    else
      result
  in
  
  (******************************************************************
  *getConstant:
  * Used when folding over a table to collect all constants of a
  * particular kind (Global, Local, etc).
  ******************************************************************)
  let getConstant kind sym constant result =
    if (Absyn.getConstantType constant) = kind then
      constant :: result
    else
      result
  in
  
  (********************************************************************
  *mergeLocalKinds:
  * Merges the list of local kinds and the kind table.  Any local
  * kind without an associated arity must be declared already.
  ********************************************************************)
  let mergeLocalKinds = fun klist kt ->
    let merge = fun ktable kind ->
      match kind with
        Absyn.Kind(s, Some a, _, Absyn.LocalKind, p) ->
          (match (Table.find s ktable) with
            Some Absyn.Kind(s', Some a', _, Absyn.GlobalKind, p') ->
              if a <> a' then
                (Errormsg.error p ("kind already declared with arity " ^ (string_of_int a));
                ktable)
              else
                (Table.add s kind ktable)
          | Some Absyn.Kind(s', Some a', _, Absyn.LocalKind, p') ->
              if a <> a' then
                (Errormsg.error p ("kind already declared with arity " ^ (string_of_int a));
                ktable)
              else
                ktable
          | Some k ->
              (Errormsg.impossible Errormsg.none "Translate.translateModule: invalid kind")
          | None -> (Table.add s kind ktable))
      | Absyn.Kind(s, None, _, Absyn.LocalKind, p) ->
          (match (Table.find s ktable) with
            Some Absyn.Kind(s', Some a', m, Absyn.GlobalKind, p') ->
              (Table.add s (Absyn.Kind(s', Some a', m,Absyn.LocalKind, p')) ktable)
          | Some Absyn.Kind(s', Some a', m, Absyn.LocalKind, p') ->
              ktable
          | Some k ->
              (Errormsg.impossible (Absyn.getKindPos k) "invalid kind type")
          | None ->
              (Errormsg.error p ("undeclared kind " ^ (Symbol.name s));
              ktable))
      | _ -> (Errormsg.impossible (Absyn.getKindPos kind) "mergeLocalKinds(): invalid kind type")
    in
    (List.fold_left merge kt klist)
  in
  
  (********************************************************************
  *mergeGlobalKinds:
  * Merges the global kinds declared in the module with the kind table.
  * All global kinds are added as locals unless they appear as globals
  * in the kindtable already.
  ********************************************************************)
  let mergeGlobalKinds = fun klist kt ->
    let merge = fun ktable kind ->
      match kind with
        | Absyn.Kind(s,Some a,m, Absyn.GlobalKind,p) ->
            begin match (Table.find s ktable) with
                Some Absyn.Kind(s',Some a',_,Absyn.GlobalKind, p') ->
                  if a <> a' then
                    (Errormsg.error p ("kind already declared with arity " ^ (string_of_int a) ^
                                         (Errormsg.see p' "kind declaration"));
                     ktable)
                  else
                    ktable
              | Some k ->
                  (Errormsg.impossible (Absyn.getKindPos k) "invalid kind type")
              | None ->
                  (Table.add s (Absyn.Kind(s, Some a, m,Absyn.LocalKind, p)) ktable)
            end
        | _ -> Errormsg.impossible Errormsg.none "Non-global kind encountered in mergeGlobalKinds"
    in
    (List.fold_left merge kt klist)
  in

  (********************************************************************
  *mergeLocalConstants:
  * Merge the local constants in the module into the constant table.
  * If a constant is declared as local and has no declared type then
  * the constant must already exist as a global.  If the type is
  * declared then it must either match an existing global declaration
  * or there must not be any global declaration.
  ********************************************************************)
  let mergeLocalConstants clist ctable =
    let f = 
      ifThenElse (doesNot hasSkeleton)
        (ifThenElse (previouslyExists)
          (ifThenElse (hasCurrentConstantType Absyn.GlobalConstant)
            (error "local constant appears in signature")
            (setCurrentConstantType Absyn.LocalConstant))
          (enterConstant (setCurrentConstantType Absyn.LocalConstant)))
        (ifThenElse (doesNot previouslyExists)
          (enterConstant (setCurrentConstantType Absyn.LocalConstant))
          (ifThenElse (mustCompare)
            (ifThenElse (hasCurrentConstantType Absyn.GlobalConstant)
              (error "local constant appears in signature")
              (seq [copyConstant; setCurrentConstantType Absyn.LocalConstant]))
            (failure)))
    in    
    mergeConstants clist ctable f
  in
      
  (********************************************************************
  *mergeClosedConstants:
  * Merge the closed constants in the module into the constant table.
  ********************************************************************)
  let mergeClosedConstants clist ctable =
    let f = 
      ifThenElse (doesNot hasSkeleton)
        (ifThenElse (previouslyExists)
          (setCurrentConstantClosed true)
          (enterConstant (setCurrentConstantClosed true)))
        (ifThenElse (doesNot previouslyExists)
          (enterConstant (setCurrentConstantClosed true))
          (ifThenElse (mustCompare)
            (seq [copyConstant; setCurrentConstantClosed true])
            (failure)))
    in    
    mergeConstants clist ctable f
  in
  
  (********************************************************************
  *mergeUseOnlyConstants:
  * Merge the useonly constants in the module into the constant table.
  ********************************************************************)
  let mergeUseonlyConstants clist ctable =
    let f =
      ifThenElse (doesNot hasSkeleton)
        (ifThenElse
          (andAlso
            [previouslyExists; hasCurrentConstantType Absyn.GlobalConstant;
            hasCurrentConstantUseonly true; hasCurrentConstantExportdef false])
          (success)
          (error "declared as useonly without corresponding declaration in signature"))
        (ifThenElse
          (andAlso
            [previouslyExists; hasCurrentConstantType Absyn.GlobalConstant;
            hasCurrentConstantUseonly true; hasCurrentConstantExportdef false; mustCompare])
          (success)
          (error "declared as useonly without corresponding declaration in signature"))
    in    
    mergeConstants clist ctable f
  in

  (********************************************************************
  *mergeExportdefConstants:
  * Merge the exportdef constants in the module into the constant table.
  ********************************************************************)
  let mergeExportdefConstants clist ctable =
    let f = 
      ifThenElse (doesNot hasSkeleton)
        (ifThenElse
          (andAlso
            [previouslyExists; hasCurrentConstantType Absyn.GlobalConstant;
            hasCurrentConstantUseonly false; hasCurrentConstantExportdef true])
          (success)
          (error "declared as exportdef without corresponding declaration in signature"))
        (ifThenElse
          (andAlso
            [previouslyExists; hasCurrentConstantType Absyn.GlobalConstant;
            hasCurrentConstantUseonly false; hasCurrentConstantExportdef true;
            mustCompare])
          (success)
          (error "declared as exportdef without corresponding declaration in signature"))
    in    
    mergeConstants clist ctable f
  in
  
  (********************************************************************
  *mergeGlobalConstants:
  * Merge the global constants in the module into the constant table.
  ********************************************************************)
  let mergeGlobalConstants clist ctable =
    let f =
      ifThenElse previouslyExists
        (ifThenElse mustCompare
          (success)
          (failure))
        (enterConstant (setCurrentConstantType Absyn.LocalConstant))
    in
    mergeConstants clist ctable f
  in
  
  (******************************************************************
  *processAccumMods:
  * Convert a list of accumulated modules filenames into a list of
  * preabsyn signatures.
  ******************************************************************)
  let rec processAccumMods = function
    Preabsyn.Symbol(accum,_,_)::rest ->
      (Compile.compileSignature ((Symbol.name accum)))::(processAccumMods rest)
  | [] -> []
  in
  
  (******************************************************************
  *processImpMods:
  * Convert a list of imported modules filenames into a list of
  * preabsyn signatures.
  ******************************************************************)
  let rec processImpMods = function
    Preabsyn.Symbol(accum,_,_)::rest ->
      (Compile.compileSignature ((Symbol.name accum)))::(processImpMods rest)
  | [] -> []
  in
  
  (********************************************************************
  *translateAccumMods:
  * Goes through all of the accumulated modules, parses them, and
  * gets the appropriate tables and suchlike.
  ********************************************************************)
  let rec translateAccumMods = fun accums ktable ctable atable sigs ->
    match accums with
      s::rest ->
        let (asig, (ktable', ctable', atable')) =
          translateSignature s false true ktable ctable atable
          buildLocalKind buildLocalConstant copyAccum in
        let a = Absyn.AccumulatedModule(Absyn.getSignatureName asig, asig) in
        (translateAccumMods rest ktable' ctable' atable' (a::sigs))
    | [] ->
        (sigs, (ktable, ctable, atable))
  in
  
  (********************************************************************
  *translateImpMods:
  * Goes through all of the imported modules, parses them, and
  * gets the appropriate tables and suchlike.
  ********************************************************************)
  let rec translateImpMods = fun imps ktable ctable atable sigs ->
    match imps with
      s::rest ->
        let (asig, (ktable', ctable', atable')) =
          translateSignature s false true ktable ctable atable
          buildLocalKind buildLocalConstant copyAccum in
	      let i = Absyn.ImportedModule(Absyn.getSignatureName asig, asig) in
        (translateImpMods rest ktable' ctable' atable' (i::sigs))
    | [] ->
        (sigs, (ktable, ctable, atable))
  in
  (*  Get the pieces of the module  *)
  match mod' with
    Preabsyn.Module(name, gconsts, lconsts, cconsts, uconsts, econsts, fixities,
                      gkinds, lkinds, tabbrevs, clauses, accummods,
                      accumsigs, usesigs, impmods) ->

      (*  Translate the accumulated modules *)
      let accummods' = processAccumMods accummods in
      let (accums, (ktable, ctable, atable)) = translateAccumMods accummods' ktable ctable atable [] in
      
      (*  Translate the imported modules  *)
      let impmods' = processImpMods impmods in
      let (imps, (ktable, ctable, atable)) = translateImpMods impmods' ktable ctable atable [] in
      
      (*  Translate local and global kinds, and get associated tables *)
      let gkindlist = translateGlobalKinds gkinds in
      let lkindlist = translateLocalKinds lkinds in
      
      let ktable = mergeGlobalKinds gkindlist ktable in
      let ktable = mergeLocalKinds lkindlist ktable in
          
      (*  Translate type abbreviations and get the associated table *)
      let atable' = translateTypeAbbrevs tabbrevs ktable in
      let atable = mergeTypeAbbrevs atable' atable in
      
      (*  Translate local, global, closed, useonly, and exportdef constants
          and get the associated tables. *)
      let gconstlist = translateGlobalConstants gconsts ktable atable in
      let lconstlist = translateLocalConstants lconsts ktable atable in
      let cconstlist = translateClosedConstants cconsts ktable atable in
      let uconstlist = translateUseOnlyConstants true uconsts ktable atable in
      let econstlist = translateExportdefConstants true econsts ktable atable in
      
      let ctable = mergeGlobalConstants gconstlist ctable in
      let ctable = mergeLocalConstants lconstlist ctable in
      let ctable = mergeClosedConstants cconstlist ctable in
      let ctable = mergeUseonlyConstants uconstlist ctable in
      let ctable = mergeExportdefConstants econstlist ctable in
      
      (*  Apply fixity flags  *)
      let ctable = translateFixities fixities ctable in
      
      (*  Get local and global kind lists.  *)
      let globalkinds = Table.fold (getGlobalKind) ktable [] in
      let localkinds = Table.fold (getLocalKind) ktable [] in
      
      (*  Get local and global constant lists.  *)
      let globalconstants = Table.fold (getConstant Absyn.GlobalConstant) ctable [] in
      let localconstants = Table.fold (getConstant Absyn.LocalConstant) ctable [] in
      
      (*  Verify that all constants have a body, and
          all kinds have an arity.  *)
      if (checkConstantBodies ctable) && (checkKindArities ktable) then
        (*  Get constant skeletons. *)
        let skeletons = (List.fold_left (getSkeleton) [] globalconstants) in
        let skeletons = (List.fold_left (getSkeleton) skeletons localconstants) in

        let amod =
          Absyn.Module(name, imps, accums, ref ctable, ref ktable,
          atable, [], globalkinds, localkinds, globalconstants, localconstants,
          ref [], skeletons, ref [], ref(Absyn.ClauseBlocks([]))) in
          let _ = Errormsg.log Errormsg.none
            "Translate.translateModule: Translated module." in
        amod
      else
        Absyn.ErrorModule
  | Preabsyn.Signature _ ->
      invalid_arg "Types.translateModule: attempted to translate Preabsyn.Signature()"
