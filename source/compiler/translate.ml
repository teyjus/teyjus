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
type pos = Errormsg.pos


let getFromTable check t =
  let r = Table.fold 
            (fun _ el l -> if (check el) then el::l else l) t [] in
    List.rev r

type typeandenvironment =
  TypeAndEnvironment of (Absyn.atype * int)

let typeSkeletonIndex = ref 0


type typeandbindings =
  TypeAndBindings of (Absyn.atype * Absyn.atype Table.symboltable)

(********************************************************************
*rationalizeType:
* Rationalizes the type ty i.e. translates a preabsyn ptype to an absyn atype
*
* - vartable is the current table associating symbols to absyn types
*    * when rationalizing a type, vartable is originally empty
*    * for a type abbreviation, it is initialized with all the variables
*      declared as appearing in the abbreviated type
*      For instance, when rationalizing 
*      typeabbrev (bar A B) (list A) -> (list B)
*      vartable will be initialized to the table associating A and B to two
*      BindableVarType
*   While recursively rationalizing the type, the vartable will grow
*
*
*********************************************************************)
let rec rationalizeType ty vartable kindtable typeabbrevtable transvarfunc =
    
  (******************************************************************
  *translateArrow:
  * Translate an arrow from preabsyn to absyn.
  ******************************************************************)
  let translateArrow = function Preabsyn.Arrow(l, r, _) ->
    let TypeAndBindings(r', rs) = rationalizeType r vartable kindtable 
                                    typeabbrevtable transvarfunc in
    let TypeAndBindings(l', ls) = rationalizeType l rs kindtable 
                                    typeabbrevtable transvarfunc in
      TypeAndBindings(Absyn.ArrowType(l', r'), ls)
  | t -> invalid_arg "Types.translateArrow: invalid type"
  in

  (******************************************************************
  *translateApp:
  * Translate an application from preabsyn to absyn.
  ******************************************************************)
  let translateApp ty =

   (**************************************************************
    *getHeadAndArgsType:
    * Given a type application, gets the arguments as a list instead of a tree.
    * Returns a triple containing
    * (the head, the updated symbol table, the list of arguments)
    **************************************************************)
    let rec getHeadAndArgsType ty ts =
      match ty with
        | Preabsyn.App(f, arg, _) ->
            (* First take care of the leftmost argument to preserve the 
             * convention of writing type variables in alphabetic order
             * from left to right (eg. A -> B -> pair A B *)
            let (head, ts', argtypes) = (getHeadAndArgsType f ts) in
            let TypeAndBindings(argtype, ts'') = 
              rationalizeType arg ts' kindtable typeabbrevtable 
                transvarfunc 
            in
              (head, ts'', argtypes @ [argtype])
        | _ -> (ty, ts, [])
    in

    (* The main case for the application translation, when the head
     * is a constant *)
    let const_app sym ts args  pos =
      match (Table.find sym vartable) with
           Some t ->
             (Errormsg.error pos 
                "found type variable, expected a constructor";
              TypeAndBindings(Absyn.ErrorType, vartable))
         | None ->
             match (Table.find sym kindtable) with
                  Some k ->
                    if (Absyn.getKindArity k) <> (List.length args) then
                      (Errormsg.error pos 
                         ("type constructor " ^ (Symbol.name sym) ^ 
                          " has arity " ^ 
                          (string_of_int (Absyn.getKindArity k)));
                       TypeAndBindings(Absyn.ErrorType, vartable))
                    else
                      TypeAndBindings(Absyn.ApplicationType(k, args), ts)
                | None ->
                    match (Table.find sym typeabbrevtable) with
                         Some t ->
                           (translateTypeAbbrevCall t args ts pos)
                       | None ->
                           (Errormsg.error pos 
                              ("undeclared constructor " ^ (Symbol.name sym));
                            TypeAndBindings(Absyn.ErrorType, vartable))
    in

      (* Main for translateApp *)
      match ty with 
        | Preabsyn.App(_, _ , p) ->
           let (head, ts, args) = getHeadAndArgsType ty vartable in
             (match head with
                | Preabsyn.Atom(sym, Preabsyn.ConstID, p) ->
                    const_app sym ts args p
                | Preabsyn.Atom(_, _, p) ->
                    (Errormsg.error p 
                       "found type variable, expected a constructor";
                     TypeAndBindings(Absyn.ErrorType, vartable))
                | _ ->  
                    (Errormsg.error p "expected a constructor");
                    TypeAndBindings(Absyn.ErrorType, vartable))
       | _ -> invalid_arg "Types.translateApp: invalid type"
  in  

  (* Main for rationalizeType *)
  match ty with
    | Preabsyn.Atom(s, Preabsyn.AVID, pos) ->
        (transvarfunc s vartable pos)
    | Preabsyn.Atom(s, Preabsyn.VarID, pos) ->
        (* If the variable is in the variable table, just return the 
         * type associated with it.  Otherwise, create a new one. *)
        (match (Table.find s vartable) with
          | Some t -> TypeAndBindings(t, vartable)
          | None -> transvarfunc s vartable pos)
    | Preabsyn.Atom(s, Preabsyn.CVID, pos) ->
        (match (Table.find s vartable) with
             Some t -> TypeAndBindings(t, vartable)
           | None -> transvarfunc s vartable pos)
    | Preabsyn.Atom(s, Preabsyn.ConstID, pos) ->
        (match (Table.find s kindtable) with
             Some k ->
               if (Absyn.getKindArity k) <> 0 then
                 (Errormsg.error pos ("expected a sort but found type " ^ 
                                      "constructor of arity " ^
                                      (string_of_int (Absyn.getKindArity k)));
                  TypeAndBindings(Absyn.ErrorType, vartable))
               else
                 TypeAndBindings(Absyn.ApplicationType(k, []), vartable)
           | None ->
               (match (Table.find s typeabbrevtable) with
                    Some t ->
                      (translateTypeAbbrevCall t [] vartable pos)
                  | None ->
                      (Errormsg.error pos 
                         ("undeclared constant '" ^ (Symbol.name s) ^ "'");
                       TypeAndBindings(Absyn.ErrorType, vartable))))

    | Preabsyn.App(_, _, _) ->
        translateApp ty

    | Preabsyn.Arrow(_, _, _) ->
        translateArrow ty

    | Preabsyn.ErrorType -> TypeAndBindings(Absyn.ErrorType, vartable)

and translateTypeAnnot ty table amodule =
  let rationalizeVar sym symtable _ =
    let t = Absyn.makeTypeVariable () in
      TypeAndBindings(t, (Table.add sym t symtable)) 
  in
    rationalizeType ty table 
      (Absyn.getModuleKindTable amodule) 
      (Absyn.getModuleTypeAbbrevTable amodule)
      rationalizeVar 

(**********************************************************************
*translateConstantTypeSkeleton:
* Translate a preabsyn representation of a constant's type into an absyn type
* skeleton.
**********************************************************************)
and translateConstantTypeSkeleton ty kindtable typeabbrevtable =
  (* Before removing this piece of code, be sure that the order
   * in rationalizeType is OK *)
  (*********************************************************************
  *translateArrow:
  * Translate an arrow, with a check to ensure that
  *********************************************************************)
 (* let translateArrow = function Preabsyn.Arrow(l, r, pos) ->
    (*******************************************************************
    *getTarget:
    * Get the target of an arrow type.
    *******************************************************************)
    let rec getTarget t = 
      match t with
        Preabsyn.Arrow(l,r,p) -> getTarget r
      | _ -> t
    in
    
    (*******************************************************************
    *getArgs:
    * Get the arguments of an arrow type in list form.
    *******************************************************************) 
    let rec getArgs t =
      match t with
        Preabsyn.Arrow(l, r, p) -> l::(getArgs r)
      | _ -> []
    in
    
    (*******************************************************************
    *translateArgs:
    * Takes a list of arguments, and translates them.  Passes along the
    * environment.
    *******************************************************************)
    let rec translateArgs args ts =
      match args with
        arg::rest ->
          let TypeAndBindings(t, ts') =
            rationalizeType arg ts kindtable typeabbrevtable 
                            rationalizeSkeletonVar in
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
    let TypeAndBindings(target', ts) =
      rationalizeType target Table.empty kindtable typeabbrevtable 
                      rationalizeSkeletonVar in

    (*  Translate all of the arguments  *)
    let args' = translateArgs args ts in
      
      (*  Rebuild the arrow type as absyn *)
      buildArrow target' args'
    
  | _ -> invalid_arg "Types.translateArrow: invalid type"
  in
  *)
  
  let _ = typeSkeletonIndex := 0 in
  let rationalizeSkeletonVar sym symtable _ =
    let t = Absyn.SkeletonVarType(ref !typeSkeletonIndex) in
      (typeSkeletonIndex := !typeSkeletonIndex + 1;
       TypeAndBindings(t, (Table.add sym t symtable)))
  in
  let TypeAndBindings(ty', _) = 
    rationalizeType ty Table.empty kindtable typeabbrevtable 
      rationalizeSkeletonVar in
    TypeAndEnvironment(ty', !typeSkeletonIndex) 

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
    (******************************************************************
    *addFixities:
    * Given a list of symbols, a fixity, a precedence, a position, and
    * a constant table, updates the information in the table for each
    * symbol.  Simply recurses over the list of symbols and adds the
    * information to each element in the table with the given symbol.
    *
    * TODO: Convert to a fold for clarity.
    ******************************************************************)
    let rec addFixities syms k prec pos ctable =
      (****************************************************************
      *getFixity:
      * Straightforward preabsyn fixity to absyn fixity conversion.
      ****************************************************************)
      let getFixity k =
        match k with
          Preabsyn.Infix(_) -> Absyn.Infix
        | Preabsyn.Infixl(_) -> Absyn.Infixl
        | Preabsyn.Infixr(_) -> Absyn.Infixr
        | Preabsyn.Prefix(_) -> Absyn.Prefix
        | Preabsyn.Prefixr(_) -> Absyn.Prefixr
        | Preabsyn.Postfix(_) -> Absyn.Postfix
        | Preabsyn.Postfixl(_) -> Absyn.Postfixl
      in
      
      (****************************************************************
      *checkFixityArity:
      * Ensure that the arity of the constant is correct with respect
      * to the fixity it is being declared with.  Specifically, pre-
      * and postfix constants must have arity equal to 1, and infix
      * constants must have arity greater than or equal to 2.
      ****************************************************************)
      let checkFixityArity k ta =
        match k with
          Preabsyn.Infix(_)
        | Preabsyn.Infixl(_)
        | Preabsyn.Infixr(_) -> ta >= 2
        | Preabsyn.Prefix(_)
        | Preabsyn.Prefixr(_)
        | Preabsyn.Postfix(_)
        | Preabsyn.Postfixl(_) -> ta = 1
      in
            
      match syms with
        [] -> ctable
      | Preabsyn.Symbol(sym,_,pos)::syms' ->
          (match Table.find sym ctable with
            Some c ->
              let skel = (Absyn.getConstantSkeleton c) in
              let fix = Absyn.getConstantFixityRef c in
              let prec' = Absyn.getConstantPrecRef c in
              let pos' = Absyn.getConstantPos c in
              
              if Option.isSome skel then
                let t' = Absyn.getSkeletonType (Option.get skel) in
                if not (checkFixityArity k (Absyn.getArrowTypeArity t')) then
                  (Errormsg.error pos 
                     ("declared fixity is incompatible with " ^ 
                      "declared type arity" ^
                      (Errormsg.see pos' "constant declaration"));
                  addFixities syms' k prec pos ctable)
                else if not (checkPrec !prec' prec) then
                  (Errormsg.error pos
                     ("constant " ^ (Symbol.name sym) ^ 
                      " already declared with precedence " ^
                      (string_of_int !prec') ^
                      (Errormsg.see pos' "constant declaration"));
                  addFixities syms' k prec pos ctable)
                else if not (checkFixity !fix (getFixity k)) then
                  (Errormsg.error pos
                     ("constant " ^ (Symbol.name sym) ^ 
                      " already declared with fixity " ^
                      (Absyn.string_of_fixity !fix) ^
                      (Errormsg.see pos' "constant declaration"));
                  addFixities syms' k prec pos ctable)
                else
                  (fix := (getFixity k);
                  prec' := prec;
                  addFixities syms' k prec pos ctable)
              else
                (* No skeleton *)
                if not (checkPrec !prec' prec) then
                  (Errormsg.error pos
                     ("constant " ^ (Symbol.name sym) ^ 
                      " already declared with precedence " ^
                      (string_of_int !prec') ^
                      (Errormsg.see pos' "constant declaration"));
                  addFixities syms' k prec pos ctable)
                else if not (checkFixity !fix (getFixity k)) then
                  (Errormsg.error pos
                     ("constant " ^ (Symbol.name sym) ^ 
                      " already declared with fixity " ^
                      (Absyn.string_of_fixity !fix) ^
                      (Errormsg.see pos' "constant declaration"));
                  ctable)
                else
                  (fix := (getFixity k);
                  prec' := prec;
                  addFixities syms' k prec pos ctable)
          | None ->
              (Errormsg.error pos 
                 ("fixity declaration: undeclared constant " ^ 
                  (Symbol.name sym));
              addFixities syms' k prec pos ctable))
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
and translateKinds klist buildkind =
  let rec translate'  klist result =
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
  * passed to translateKind to construct a kind, and then adds it to the list.
  ********************************************************************)
  let rec addKind = fun syms a pos result ->
    (match syms with
      [] -> result
    | Preabsyn.Symbol(sym, _, _)::ss -> 
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
and buildGlobalConstant = fun sym tyskel esize pos ->
  Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref false, ref false,
                 ref false, ref true, ref false, ref false, tyskel,
                 ref esize, ref (Some(Array.make esize true)), 
                 ref (Some(Array.make esize true)),
                 ref None, ref Absyn.GlobalConstant, ref 0, pos)

and translateGlobalConstants clist kindtable typeabbrevtable =
  translateConstants clist kindtable typeabbrevtable buildGlobalConstant

(**********************************************************************
*translateLocalConstants:
**********************************************************************)
and buildLocalConstant sym tyskel esize pos =
  Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref false, ref false,
                 ref false, ref true, ref false, ref false, tyskel,
                 ref esize, ref (Some(Array.make esize true)), 
                 ref (Some(Array.make esize true)),
                 ref None, ref Absyn.LocalConstant, ref 0, pos)
    
and translateLocalConstants clist kindtable typeabbrevtable =
  translateConstants clist kindtable typeabbrevtable buildLocalConstant

(**********************************************************************
*translateUseOnlyConstants:
**********************************************************************)
and translateUseOnlyConstants owner clist kindtable typeabbrevtable =
  let buildConstant = fun sym tyskel esize pos ->
    Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref false, ref true,
                   ref true, ref true, ref false, ref false, tyskel,
                   ref esize, ref (Some(Array.make esize true)), 
                   ref (Some(Array.make esize true)),
                   ref None, ref Absyn.GlobalConstant, ref 0, pos)
  in
  translateConstants clist kindtable typeabbrevtable buildConstant

(**********************************************************************
*translateExportdefConstants:
**********************************************************************)
and translateExportdefConstants owner clist kindtable typeabbrevtable =
  let buildConstant = fun sym tyskel esize pos ->
    Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref true, ref false,
                   ref (not owner), ref true, ref false, ref false, tyskel,
                   ref esize, ref (Some(Array.make esize true)), 
                   ref (Some(Array.make esize true)),
                   ref None, ref Absyn.GlobalConstant, ref 0, pos)
  in
  translateConstants clist kindtable typeabbrevtable buildConstant

(**********************************************************************
*translateClosedConstants:
**********************************************************************)
and translateClosedConstants clist kindtable typeabbrevtable =
  let buildConstant = fun sym tyskel esize pos ->
    Absyn.Constant(sym, ref Absyn.NoFixity, ref (-1), ref false, ref false,
                   ref false, ref true, ref false, ref false, tyskel,
                   ref esize, ref (Some(Array.make esize true)), 
                   ref (Some(Array.make esize true)),
                   ref None, ref Absyn.GlobalConstant, ref 0, pos)
  in
  translateConstants clist kindtable typeabbrevtable buildConstant

(********************************************************************
*translateConstants:
* Translates the list clist of constant declarations in preabsyn
* representation into an absyn constant list.
*
* buildConstant is a function which builds a constant
* according to its classification (local/global/useonly/expordef)
*
* kindtable and typeabbrevtable will be used later to detect clashes
* or eventually merge multiple declarations
********************************************************************)
and translateConstants clist kindtable typeabbrevtable buildconstant =
(*  List.map *)
(*    (fun const ->*)
(*       (translateConstant const result kindtable typeabbrevtable buildconstant)*)
  let rec translate' clist result =
    match clist with
      c::cs ->
        let result' = (translateConstant c result kindtable 
             typeabbrevtable buildconstant) in
          translate' cs result'
    | [] -> result
  in
  translate' clist []

(**********************************************************************
*translateConstant:
* Translate a preabsyn constant into an absyn constant and enter it
* into a table.
*
* We can have several resulting constants since a single preabsyn constant
* may carry several symbols (as in the declaration: type p, q o.)
**********************************************************************)
and translateConstant c clist kindtable typeabbrevtable buildconstant =
  (********************************************************************
  *translate':
  * Enter all names into table.
  ********************************************************************)
  let rec enter names tyskel esize clist =
    match names with
      Preabsyn.Symbol(name, _, p)::ns ->
        let clist' = (buildconstant name tyskel esize p) :: clist in
          (enter ns tyskel esize clist')
    | [] -> clist
  in
  
  match c with
    | Preabsyn.Constant(names, Some t, _) ->
      (* A regular constant declaration *)
      let TypeAndEnvironment(ty, size) = 
        translateConstantTypeSkeleton t kindtable typeabbrevtable in
      (* let ty = translateType' t kindtable typeabbrevtable in 
        If necessary, translateType' was removed in commit 1067 *)
        (enter names (ref(Some(Absyn.Skeleton(ty, ref None, ref false))))
                     size clist)
    | Preabsyn.Constant(names, None, _) ->
        (* This just a closed/exportdef or useonly declaration *) 
        (enter names (ref None) 0 clist)

(********************************************************************
*translateTypeAbbrevs:
* Translates a list of type abbreviations in preabsyn representation
* into a type abbreviation table.
* Abbreviations are merged one by one during calls to translateTypeAbbrev
********************************************************************)
and translateTypeAbbrevs tabbrevs kindtable =
  let rec translate' tlist abbrevtable =
    match tlist with
      | [] -> abbrevtable
      | t::ts ->
          let abbrevtable' = translateTypeAbbrev t abbrevtable kindtable in
            translate' ts abbrevtable'
  in
    translate' tabbrevs Table.empty

(********************************************************************
*translateTypeAbbrev:
* Translate a type abbreviation from preabsyn to absyn.
* abbrev is the preabsyn abbreviation to translate and to merge
* into abbrevtable.
* The kinds table kindtable from the module is used to detect clashes
* between type abbreviations and kind declarations
********************************************************************)
and translateTypeAbbrev abbrev abbrevtable kindtable =
  let Preabsyn.TypeAbbrev(name, arglist, ty, pos) = abbrev in
  
  (****************************************************************
  *getName:
  ****************************************************************)
  let getName = function
    Preabsyn.Symbol(n, Preabsyn.ConstID, _) -> n
  | Preabsyn.Symbol(n, _, p) ->
      (Errormsg.error p "expected type abbreviation name";
      n)
  in
  
  (****************************************************************
  *checkArgs:
  ****************************************************************)
  let rec checkArgs = function
    [] -> []
  | Preabsyn.Symbol(n, Preabsyn.CVID, _)::ss -> n::(checkArgs ss)
  | Preabsyn.Symbol(_, _, p)::_ ->
      (Errormsg.error p "expected type abbreviation argument name";
      [])
  in
  
  (****************************************************************
  *buildTable:
  ****************************************************************)
  let rec buildTable syms i =
    match syms with
      [] -> Table.empty
    | sym::ss ->
        (Table.add sym (Absyn.SkeletonVarType(ref i)) (buildTable ss (i + 1)))
  in
  
  (*  Get the name and arguments  *)
  let abbrevname = getName name in
  let args = checkArgs arglist in
  
  (*  Build a symbol table of the args  
  * All the variables appearing in the abbreviated type are in this table *)
  let symtable = buildTable args 0 in

  (*  Translate the type body i.e. the abbreviated type *)
  let rationalizeTypeAbbrevVar sym symtable p =
    (* All variables appearing in the abbreviated type have to already
    * be in the table *)
    (Errormsg.error p ("unbound variable " ^ (Symbol.name sym) ^ 
                       " in type abbreviation");
     TypeAndBindings(Absyn.ErrorType, symtable))
  in
  let TypeAndBindings(bodytype, _) = 
    rationalizeType ty symtable kindtable abbrevtable
                     rationalizeTypeAbbrevVar in
  
  (* GN, Aug 23, 2012: We assume here that kindtable contains ALL the kind 
     declarations pertinent to the present module/signature and we check for
     clashes with this when populating the type abbreviations table. *)
  let kindInTab = Table.find abbrevname kindtable in
    (match kindInTab with
         Some (Absyn.Kind(_, _, _, _, p')) -> 
           (Errormsg.error pos ("type abbreviation name '" ^
                                (Symbol.name abbrevname) ^ 
                                "' clashes with kind name" ^
                                (Errormsg.see p' "kind declaration"));
            abbrevtable)
       | None -> 
           (mergeTypeAbbrev abbrevname 
              (Absyn.TypeAbbrev(abbrevname, args, bodytype, pos))
              abbrevtable))

(********************************************************************
*translateTypeAbbrevCall:
* Given a variable table and arguments, instantiates a type abbrev.
********************************************************************)
and translateTypeAbbrevCall abbrev args vartable pos =
  let Absyn.TypeAbbrev(name, syms, target, pos') = abbrev in
  
  let rec replaceArg = fun argnum a t ->
    match t with
      Absyn.ArrowType(l,r) ->
        Absyn.ArrowType(replaceArg argnum a l, replaceArg argnum a r)
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
    (Errormsg.error pos ("type abbreviation '" ^ (Symbol.name name) ^
      "' has arity " ^ (string_of_int (List.length syms)) ^ 
      (Errormsg.see pos' "type abbreviation declaration"));
    TypeAndBindings(Absyn.ErrorType, vartable))
  else
    TypeAndBindings((replaceAll 0 args target), vartable)

(******************************************************************
*mergeTypeAbbrev:
******************************************************************)
and mergeTypeAbbrev sym tabbrev table =
  let Absyn.TypeAbbrev(s, args, ty, p) = tabbrev in
    match (Table.find sym table) with
      | Some Absyn.TypeAbbrev(s', args', ty', p') ->
          (* GN, Aug 20, 2012: should not be checking name identity here 
          * since different abbrev presentations could use different names 
          * if args <> args' then *)
          if (List.length args) <> (List.length args') then 
            (Errormsg.error p 
               ("conflict in argument numbers for type abbreviation" ^
                (Errormsg.see p' "type abbreviation declaration"));
                table)
          (* GN, Aug 20, 2012: 
           * May be better not to rely on OCaml's equality here! *)
          else if ty <> ty' then
            (Errormsg.error p 
               ("conflict in type for type abbreviation" ^
                (Errormsg.see p' "type abbreviation declaration"));
             table)
          else 
            table
      | None ->
          (Table.add sym tabbrev table)


(******************************************************************
*mergeTypeAbbrevs:
******************************************************************)
and mergeTypeAbbrevs t1 t2 ktable = 
   (Table.fold 
       (fun sym abbrev abbtab ->
            let kindInTab = Table.find sym ktable in
              (match kindInTab with
                  Some (Absyn.Kind(_,_,_,_,p')) -> 
                      (Errormsg.error (Absyn.getTypeAbbrevPos abbrev)
                         ("type abbreviation name '" ^ (Symbol.name sym) ^ 
                          "' clashes with kind name" ^
                          (Errormsg.see p' "kind declaration"));
                       abbtab)
                | None -> (mergeTypeAbbrev sym abbrev abbtab)))
       t1 t2)

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
    (Errormsg.error p 
       ("constant '" ^ name ^ "' already declared with precedence" ^
        (string_of_int prec') ^ (Errormsg.see p' "constant declaration"));
     false)
  else if (Option.isSome skel) && (Option.isSome skel') then
    let t1 = Absyn.getSkeletonType (Option.get skel) in
    let t2 = Absyn.getSkeletonType (Option.get skel') in
    if not (Absyn.types_equal t1 t2) then
      (Errormsg.error p 
         ("constant '" ^ name ^ "' declared with incompatible type '" ^
          (Absyn.string_of_skeleton (Option.get skel)) ^ "'" ^
          (Errormsg.see p' 
             ("constant declaration with type '" ^ 
              (Absyn.string_of_skeleton (Option.get skel')) ^ "'")));
      false)
    else
      true
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
* Ensures that all constants in the given constant table have skeletons;
* used to ensure that the user didn't only provide constant declarations
* that don't require skeletons.
**********************************************************************)
and checkConstantBodies ktable ctable =
  let result = ref true in
  let check s c =
    let p = (Absyn.getConstantPos c) in
    let name = (Absyn.getConstantPrintName c) in
    match (Absyn.getConstantSkeleton c) with
      Some skel ->
        (*  Check that the type declared here is correct for
        useonly and exportdef constants.  *)
        let ty = Absyn.getSkeletonType skel in
        if (Absyn.getConstantUseOnly c) && (Absyn.getConstantExportDef c) then
          (Errormsg.error p
            ("constant '" ^ name ^ "' declared as both exportdef and useonly");
          result := false)
        else if (Absyn.getConstantUseOnly c) || 
                (Absyn.getConstantExportDef c) then
          let sort = if (Absyn.getConstantUseOnly c) 
                     then "useonly" 
                     else "exportdef" in
          let targ = Absyn.getArrowTypeTarget ty in
          let k = (Absyn.getTypeKind targ) in

          if (Absyn.isConstantType targ) &&
            (Pervasive.iskbool k) then
            ()
          else
            (Errormsg.error p
              ("constant '" ^ name ^ "' declared as " ^ sort ^ 
               " without target type 'o'");
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
* Checks whether two precedences are compatible, using -1 as a value
* indicating that no precedence has been defined.
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
    let constant' = Table.find sym table in
    (f constant' constant table)
  in
  List.fold_left merge ctable clist

(**********************************************************************
*previouslyExists:
* Checks that the constant already existed.
**********************************************************************)
and previouslyExists =
  fun currentConstant _ ->
    if Option.isSome currentConstant then
      let c = Option.get currentConstant in
        not (Absyn.getConstantRedefinable c)
    else
      false

(**********************************************************************
*hasSkeleton:
* Ensures that the new constant has a skeleton.
**********************************************************************)
and hasSkeleton =
  fun _ newConstant ->
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
      Errormsg.impossible Errormsg.none 
        "Translate.mustCompare: invalid constant"

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
  let s' = "constant '" ^ (Absyn.getConstantPrintName newConstant) ^ "' " ^ s in
  if Option.isSome currentConstant then
    let p' = Absyn.getConstantPos (Option.get currentConstant) in
    (Errormsg.error p
      (s' ^ (Errormsg.see p' "constant declaration"));
    ctable)
  else
    (Errormsg.error p s';
    ctable)

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

and isNewConstantPervasive =
  fun current newConstant ->
    match (Absyn.getConstantType newConstant) with
        Absyn.PervasiveConstant(_) -> true
      | _ -> false

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
        ("setting useonly '" ^ (Absyn.getConstantPrintName newConstant) ^
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
        ("setting exportdef '" ^ (Absyn.getConstantPrintName newConstant) ^
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
        ("set exportdef '" ^ (Absyn.getConstantPrintName newConstant) ^ 
         "' to " ^ (string_of_bool !edref));
      tyref := Absyn.getConstantType newConstant;
      skelref := Absyn.getConstantSkeleton newConstant;
      uoref := Absyn.getConstantUseOnly newConstant;
      edref := Absyn.getConstantExportDef newConstant;
      closedref := Absyn.getConstantClosed newConstant;
      ctable)
    else
      Errormsg.impossible Errormsg.none
        "Translate.copyConstant: invalid current constant"
        
(**********************************************************************
*Copiers:
* Copiers are used when merging constants while translating a signature.
* They handle moving information from a newly translated constant back
* into the symbol table if the constant already exists therein.
*
* This merging should probably be handled in the same way that it is
* in translateModule.
**********************************************************************)
let copy currentConstant newConstant =
  let _ = Errormsg.log Errormsg.none
    ("copying constant '" ^ (Absyn.getConstantPrintName newConstant) ^ "'") in
  let skelref = Absyn.getConstantSkeletonRef currentConstant in
  let sizeref = Absyn.getConstantTypeEnvSizeRef currentConstant in
  let tref = Absyn.getConstantTypeRef currentConstant in
  let _ = tref := Absyn.getConstantType newConstant in
  if (Option.isNone !skelref) then
    (sizeref := Absyn.getConstantTypeEnvSize false newConstant;
    skelref := Absyn.getConstantSkeleton newConstant)
  else
    ()

let copyAccum currentConstant newConstant =
  let _ = Errormsg.log Errormsg.none
    ("copying accum '" ^ (Absyn.getConstantPrintName newConstant) ^ "'") in

  let currentType = Absyn.getConstantType currentConstant in
  let _ = copy currentConstant newConstant in
  (Absyn.getConstantTypeRef currentConstant) := currentType

let copyExportdef generalCopier owner currentConstant newConstant =
  let _ = Errormsg.log Errormsg.none
    ("copying exportdef '" ^ (Absyn.getConstantPrintName newConstant) ^ "'") in
  let _ = generalCopier currentConstant newConstant in
  
  if owner then
    let edref = Absyn.getConstantExportDefRef currentConstant in
    let nodefsref = Absyn.getConstantNoDefsRef currentConstant in
      (edref := true;
       nodefsref := not owner)
  else
    ()

let copyUseonly generalCopier owner currentConstant newConstant =
  let _ = Errormsg.log Errormsg.none
    ("copying useonly '" ^ (Absyn.getConstantPrintName newConstant) ^ "'") in
  let _ = generalCopier currentConstant newConstant in
  if owner then
    let uoref = Absyn.getConstantUseOnlyRef currentConstant in
    let nodefsref = Absyn.getConstantNoDefsRef currentConstant in
    (Errormsg.log Errormsg.none "setting useonly";
     uoref := true;
     nodefsref := true)
  else
    ()

(**********************************************************************
*translate:
* Convert from a preabsyn module to an absyn module by translating the
* module signature and then using the generated kind, constant, and
* abbreviation tables to translate the module itself.
**********************************************************************)
let rec translate mod' sig' =
    let (asig, (ktable, ctable, atable)) =
      translateSignature sig' true true copy in
    let amod = translateModule mod' ktable ctable atable in
    (amod, asig)

(**********************************************************************
*translateSignature:
* Translates a signature from preabsyn to a set of tables corresponding
* to constants, kinds, and type abbreviations.
*
* Arguments:
*   s: the signature to parse
*   owner: is this signature the current module's or is it an
*     an accumulated module's signature
*   accumOrUse: whether the signature is being translated due to an
*     accum_sig or a use_sig.  If true, the signature is being translated
*     due to an accum_sig, and so is treated normally.  If false, the
*     signature is being parsed due to a use_sig, and so exportdef
*     constants should be marked as useonly instead of exportdef.
*   generalCopier: the copier to use to move information back into
*     the symbol table when translating a constant that already
*     exists therein.
* Return:
*   an absyn signature
*   a tuple of the updated constant, kind and abbreviation tables.
**********************************************************************)
and translateSignature s owner accumOrUse generalCopier =
  match s with
    Preabsyn.Module(_) ->
      (Errormsg.impossible
        Errormsg.none
        "Translate.translateSignature: expected Preabsyn.Signature.")
  | Preabsyn.Signature(name, gconsts, uconsts, econsts, gkinds, tabbrevs,
      fixities, accumsigs, usesigs) ->

  let _ = Errormsg.log Errormsg.none ("translating signature '" ^ name ^ "'") in
  
  (******************************************************************
  *mergeKinds:
  * Adds the kinds from one signature into the kinds of all signatures.
  ******************************************************************)
  let mergeKinds klist kt =
    let merge ktable kind =
      match kind with
        Absyn.Kind(s, Some a, _, Absyn.GlobalKind, p) ->
          (*  If the kind is already in the table, match the arity.
              Otherwise, add it to the table. *)
	        let kindInTab = Table.find s ktable in
          (match kindInTab with
             Some (Absyn.Kind(s', Some a', _, Absyn.PervasiveKind, p')) ->
                             Table.add s kind ktable
	   | Some (Absyn.Kind(s', Some a', _, Absyn.GlobalKind, p')) ->
	       if a <> a' 
               then (Errormsg.error p 
                       ("kind '" ^ (Symbol.name s) ^ 
                        "' already declared with arity " ^
                        (string_of_int a') ^ 
                        (Errormsg.see p' "kind declaration"));
                     ktable)
               else (*  Leave the existing global kind. *)
                     ktable
           | Some k ->
               Errormsg.impossible (Absyn.getKindPos k)
                 ("invalid kind type " ^ (Absyn.string_of_kind k))
           | None -> 
               (*  Isn't in the table, so just add it. *)
               Table.add s kind ktable)
      | Absyn.Kind(_,_,_,Absyn.PervasiveKind,_) ->
          ktable (*  Ignore pervasive kinds. *)
      | _ -> Errormsg.impossible Errormsg.none 
                                 "Invalid kind encountered in mergeKinds"
    in
    (List.fold_left merge kt klist)
  in
  
  (******************************************************************
  *mergeConstants:
  * Adds the constants from one signature into the constants from
  * all accumulated signatures.  To merge constants first it must
  * be verified that they compare (same fixity, precedence, type,
  * etc.)  
  ******************************************************************)
  let mergeSigConstants clist ctable copier =
    let merge ctable c =
      let s = Absyn.getConstantSymbol c in
      let pos = Absyn.getConstantPos c in
      match (Table.find s ctable) with
        Some c2 ->  (*  Already in the table. *)
          if Absyn.isPervasiveConstant c2 
          then
            if Absyn.isPervasiveConstant c 
            then (*  Don't overwrite a pervasive with another pervasive. *)
              ctable
            else 
              if Absyn.getConstantRedefinable c2 
              then (Table.add s c ctable)
              else  (Errormsg.error pos 
                       ("redefinition of '" ^ (Symbol.name s) ^ 
                        "' is not allowed");
                     ctable)
          else if not (compareConstants c c2) 
               then (*  Prints an error; no need to worry about the renaming as
                        we won't be compiling.  *)
                   ctable
               else let _ = copier c2 c in ctable

      | None -> (*  Not in the table, so put it in. *)
          let _ = copier c c in
          let t = if Absyn.isLocalConstant c then "local" else "global" in
          let _ = Errormsg.log pos (t ^" constant '" ^ (Symbol.name s) ^  
                                     "' not in table") in
            (Table.add s c ctable)
    in
    (List.fold_left merge ctable clist)
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
  *mergeTables:
  ******************************************************************)
  let mergeKindandConstTables ktable ctable tables =
    let mergeTable (ktable,ctable) (ktable',ctable',_) =
      let kinds = getFromTable (fun _ -> true) ktable' in
      let constants = getFromTable (fun _ -> true) ctable' in
      let (ktable'') = mergeKinds kinds ktable in
      let (ctable'') = mergeSigConstants constants ctable generalCopier in
      (ktable'', ctable'')
    in
    List.fold_left mergeTable (ktable, ctable) tables
  in

  (******************************************************************
  *translateSigs:
  * Generalizes processing of accumulated and used signatures.
  ******************************************************************)
  let rec translateSigs tables accum sigs = 
    match sigs with
      s::rest ->	
        let (asig, table) = translateSignature s owner accum generalCopier
        in
        (translateSigs (table::tables) accum rest)
    | [] ->
        tables
  in
  
  (******************************************************************
  *translateAccumSigs:
  * Translate all accumulated signatures.  Just reads the signature
  * and imports the appropriate symbols, etc., recursively.
  ******************************************************************)
  let translateAccumSigs = translateSigs [] accumOrUse in
  
  (******************************************************************
  *translateUseSigs:
  * Translate all used signatures.  This processing is the same as
  * for accumulated signatures, but all exportdef constants are
  * switched to useonly.
  ******************************************************************)
  let rec translateUseSigs = translateSigs [] false in
  
  (*  Process accumulated signatures: *)
  let sigs = processSigs accumsigs in
  let acctables = translateAccumSigs sigs in
  
  let (ktable, ctable) = 
    mergeKindandConstTables Pervasive.pervasiveKinds 
      Pervasive.pervasiveConstants acctables in
  
  (*  Process used signatures:  *)
  let sigs = processSigs usesigs in
  let usetables = translateUseSigs sigs in
  let (ktable, ctable) = mergeKindandConstTables ktable ctable usetables in
  
  (*  Process kinds *)
  let kindlist = translateKinds gkinds buildGlobalKind in
  let ktable = mergeKinds kindlist ktable in


  (* GN, Aug 23, 2012: merging of type abbreviations tables should be 
     done after the kind table has been constructed to ensure complete
     checking of clashes of abbreviation names with kind names *)
  (*  Process type abbreviations  *)
  let atable' = translateTypeAbbrevs tabbrevs ktable in
  let atable = 
    let mergeTypeAbbrevs' t1 t2 = mergeTypeAbbrevs t2 t1 ktable in
    let acctypeabbrevtabs = (List.map (fun (_, _, t) -> t) acctables) in
    let usetypeabbrevtabs = (List.map (fun (_, _, t) -> t) usetables) in
      (List.fold_left mergeTypeAbbrevs'  
         (List.fold_left mergeTypeAbbrevs' 
            (mergeTypeAbbrevs' atable' Pervasive.pervasiveTypeAbbrevs)
            acctypeabbrevtabs)
         usetypeabbrevtabs) in
  
  (*  Translate constants *)
  let constantlist = 
    translateConstants gconsts ktable atable buildGlobalConstant in
  let uconstantlist = translateUseOnlyConstants owner uconsts ktable atable in
  let econstantlist =
    if accumOrUse then
      translateExportdefConstants owner econsts ktable atable
    else
      translateUseOnlyConstants owner econsts ktable atable
  in
  
  let ctable = mergeSigConstants constantlist ctable generalCopier in
  let ctable =
    mergeSigConstants uconstantlist ctable (copyUseonly generalCopier owner)
  in
      
  let ecopier = if accumOrUse then copyExportdef else copyUseonly in
  let ctable =
    mergeSigConstants econstantlist ctable (ecopier generalCopier owner)
  in

  (*  Normalizing.  *)
  let ctable = normalizeTable ktable ctable in
  
  (*  Renaming. *)
  let kindRenaming = 
    getFromTable (fun k -> not (Absyn.isPervasiveKind k)) ktable in
  let constRenaming = 
    getFromTable (fun c -> not (Absyn.isPervasiveConstant c)) ctable in
  
  (*  Translate fixities *)
  let ctable = translateFixities fixities ctable in
 
    (Absyn.Signature(name, kindRenaming, constRenaming),
     (ktable, ctable, atable))

(**********************************************************************
*translateModule:
* Translates a module from preabsyn to absyn.  To do so it parses and
* translates all of the accumulated or imported module signatures,
* updating the passed kind, constant, and abbreviation tables.  It
* then translates all kinds, constants, and abbreviations in the module
* itself, and merges them into the kind, constant, and abbreviation
* tables.  Next it translates fixities and precedences.  It then
* accumulates lists of global and local kinds and constants (for use
* during renaming).  Finally, it verifies that constants have bodies.
**********************************************************************)
and translateModule mod' ktable ctable atable =
  let _ = Errormsg.log 
            Errormsg.none "Translate.translateModule: Translating module..." in
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
  let mergeLocalKinds klist kt =
    let merge ktable kind =
      match kind with
        Absyn.Kind(s, Some a, _, Absyn.LocalKind, p) ->
          (match (Table.find s ktable) with
            Some Absyn.Kind(s', Some a', _, Absyn.GlobalKind, p') ->
              if a <> a' then
                (Errormsg.error 
                   p ("kind already declared with arity " ^ (string_of_int a));
                 ktable)
              else
                (Table.add s kind ktable)
          | Some Absyn.Kind(s', Some a', _, Absyn.LocalKind, p') ->
              if a <> a' then
                (Errormsg.error 
                   p ("kind already declared with arity " ^ (string_of_int a));
                 ktable)
              else
                ktable
          | Some k ->
              (Errormsg.impossible 
                 Errormsg.none "Translate.translateModule: invalid kind")
          | None -> (Table.add s kind ktable))
      | Absyn.Kind(s, None, _, Absyn.LocalKind, p) ->
          (match (Table.find s ktable) with
            Some Absyn.Kind(s', Some a', m, Absyn.GlobalKind, p') ->
              (Table.add s 
                 (Absyn.Kind(s', Some a', m,Absyn.LocalKind, p')) ktable)
          | Some Absyn.Kind(_, Some _, _, Absyn.LocalKind, _) ->
              ktable
          | Some k ->
              (Errormsg.impossible (Absyn.getKindPos k) "invalid kind type ")
          | None ->
              (Errormsg.error p ("undeclared kind " ^ (Symbol.name s));
               ktable))
      | _ ->
          (Errormsg.impossible 
             Errormsg.none "Translate.translateModule: invalid kind")
    in
    (List.fold_left merge kt klist)
  in
    
  (********************************************************************
  *mergeGlobalKinds:
  * Merges the global kinds declared in the module with the kind table.
  * All global kinds are added as locals unless they appear as globals
  * in the kindtable already.
  ********************************************************************)
  let mergeGlobalKinds klist kt =
    let merge ktable kind =
      match kind with
        Absyn.Kind(s,Some a,m, Absyn.GlobalKind,p) ->
          (match (Table.find s ktable) with
              Some Absyn.Kind(s',Some a',_,Absyn.GlobalKind, p') ->
                if a <> a' then
                  (Errormsg.error 
                     p ("kind already declared with arity " ^ (string_of_int a)
                        ^ (Errormsg.see p' "kind declaration"));
                   ktable)
                else
                  ktable	      
            | Some Absyn.Kind(s',Some a',_,Absyn.LocalKind, p') ->
	              if a <> a' then
                  (Errormsg.error 
                     p ("kind already declared with arity " ^ (string_of_int a)
                        ^ (Errormsg.see p' "kind declaration"));
                   ktable)
                else
                  ktable
            | Some k ->  
                (Errormsg.impossible (Absyn.getKindPos k) "invalid kind type ")
            | None ->
              (Table.add s 
                 (Absyn.Kind(s, Some a, m,Absyn.LocalKind, p)) ktable))
      | Absyn.Kind(_,_,_,Absyn.PervasiveKind,_) -> 
          ktable (*  Don't add pervasives. *)
      | _ -> Errormsg.impossible 
               Errormsg.none "Non-global kind encountered in mergeGlobalKinds"
    in
    (List.fold_left merge kt klist)
  in

  (********************************************************************
  *mergeLocalConstants:
  * Remember that local constants declared in two steps like:
  *   type foo o.
  *   local foo.
  * appear both in the global and the local constants lists (of the module) .
  *
  * Merge the local constants in the module into the constant table.
  * If a constant is declared as local and has no declared type then
  * the constant must also exist as a global.  If the type is
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
          (error ("declared as useonly without corresponding" ^
                    " declaration in signature")))
        (ifThenElse
          (andAlso
            [previouslyExists; hasCurrentConstantType Absyn.GlobalConstant;
            hasCurrentConstantUseonly true; hasCurrentConstantExportdef false; 
            mustCompare])
          (success)
          (error ("declared as useonly without corresponding" ^ 
                 "declaration in signature")))
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
          (error ("declared as exportdef without corresponding " ^
                  " declaration in signature")))
        (ifThenElse
          (andAlso
            [previouslyExists; hasCurrentConstantType Absyn.GlobalConstant;
            hasCurrentConstantUseonly false; hasCurrentConstantExportdef true;
            mustCompare])
          (success)
          (error ("declared as exportdef without corresponding " ^ 
                  "declaration in signature")))
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
  *processSignatures:
  * Convert a list of signature filenames in to a list of preabsyn
  * signatures.
  ******************************************************************)
  let processSignatures sigs = 
    List.map
      (fun (Preabsyn.Symbol(accum, _, _)) ->
        Compile.compileSignature (Symbol.name accum))
      sigs
  in
    
  (******************************************************************
  *mergeTables:
  * Merges the tables from accumulated or imported modules into the
  * global symbol tables.
  ******************************************************************)
  let mergeKindandConstTables ktable ctable tables =
    let mergeKindandConstTable (ktable,ctable) (ktable',ctable',_) =
      let kinds = getFromTable (fun k -> not (Absyn.isPervasiveKind k)) 
                               ktable' in
      let constants = 
            getFromTable (fun c -> not (Absyn.isPervasiveConstant c)) 
                         ctable' in
      let ktable'' = mergeGlobalKinds kinds ktable in
      let ctable'' = mergeGlobalConstants constants ctable in
      (ktable'', ctable'')
    in
    List.fold_left mergeKindandConstTable (ktable, ctable) tables
  in

  (******************************************************************
  *normalizeRenaming:
  * Takes anything that is in the accumulated module that is in the
  * accumulating module and puts it in the renaming list.
  ******************************************************************)
  let normalizeRenaming ktable ctable atable asig =
    let renameKind k =
      match Table.find (Absyn.getKindSymbol k) ktable with
          Some k' -> k'
        | _ ->
            Errormsg.impossible Errormsg.none
              "Translate.normalizeRenaming: invalid constant"
    in
    let renameConst c =
     match Table.find (Absyn.getConstantSymbol c) ctable with
          Some c' -> c'
        | _ ->
            Errormsg.impossible Errormsg.none
              "Translate.normalizeRenaming: invalid constant"
    in
    
    match asig with
        Absyn.Signature(name,kindRenaming,constRenaming) ->
          let kindRenaming' = List.map renameKind kindRenaming in
          let constRenaming' = List.map renameConst constRenaming in
          Absyn.Signature(name, kindRenaming', constRenaming')
      | _ ->
          Errormsg.impossible Errormsg.none
            "Translate.updateRenaming: invalid signature"
  in
  
  let normalizeImports ktable ctable atable imps =
    let normalize (Absyn.ImportedModule(n,s)) =
      Absyn.ImportedModule(n, normalizeRenaming ktable ctable atable s)
    in
    List.map normalize imps
  in
  let normalizeAccums ktable ctable atable accums =
    let normalize (Absyn.AccumulatedModule(n,s)) =
      Absyn.AccumulatedModule(n, normalizeRenaming ktable ctable atable s)
    in
    List.map normalize accums
  in

  (******************************************************************
  *translateMods:
  * Generalizes processing of imported, used, and accumulated modules
  * and signatures.
  ******************************************************************)
  let rec translateMods make copier sigs tables l =
    match l with
        [] -> (sigs, tables)
      | l'::ls ->
          let (asig, table) = translateSignature l' false true copier in
          let s = make asig in
            translateMods make copier (s::sigs) (table::tables) ls
  in

  (********************************************************************
  *translateAccumSigs:
  ********************************************************************)
  let translateAccumSigs sigs =
    let make asig = asig in
      translateMods make copyAccum [] [] sigs
  in
  
  (********************************************************************
  *translateUseSigs:
  ********************************************************************)
  let translateUseSigs sigs =
    let make asig = asig in
      translateMods make (copyUseonly copyAccum false) [] [] sigs
  in
  
  (********************************************************************
  *translateAccumMods:
  *********************************************************************)
  let translateAccumMods accums =
    let make asig =
      Absyn.AccumulatedModule(Absyn.getSignatureName asig, asig)
    in
      translateMods make copyAccum [] [] accums
  in
  
  (********************************************************************
  *translateImpMods:
  ********************************************************************)
  let translateImpMods imps =
    let make asig =
      Absyn.ImportedModule(Absyn.getSignatureName asig, asig)
    in
      translateMods make copyAccum [] [] imps
  in
 
  (*  Get the pieces of the module  *)
  match mod' with
    Preabsyn.Module(name, gconsts, lconsts, cconsts, uconsts, 
                    econsts, fixities,
                    gkinds, lkinds, tabbrevs, clauses, accummods,
                    accumsigs, usesigs, impmods) ->
      (*  Translate the accumulated signatures  *)
      let accumsigs' = processSignatures accumsigs in
      let (_, accsigstables) = translateAccumSigs accumsigs' in
      
      (*  Translate the used signatures *)
      let usesigs' = processSignatures usesigs in
      let (_, usesigstables) = translateUseSigs usesigs' in
      
      (*  Translate the accumulated modules *)
      let accummods' = processSignatures accummods in
      let (accums, acctables) = translateAccumMods accummods' in
      
      (*  Translate the imported modules  *)
      let impmods' = processSignatures impmods in
      let (imps, imptables) = translateImpMods impmods' in

      let (ktable, ctable) = 
        mergeKindandConstTables ktable ctable accsigstables in
      let (ktable, ctable) = 
        mergeKindandConstTables ktable ctable usesigstables in
      let (ktable, ctable) = mergeKindandConstTables ktable ctable acctables in
      let (ktable, ctable) = mergeKindandConstTables ktable ctable imptables in

      (*  Translate local and global kinds, and get associated tables *)
      let gkindlist = translateGlobalKinds gkinds in
      let lkindlist = translateLocalKinds lkinds in
      let ktable = mergeGlobalKinds gkindlist ktable in
      let ktable = mergeLocalKinds lkindlist ktable in

      (* GN, Aug 23, 2012: merging of type abbreviations tables should be 
         done after the kind table has been constructed to ensure complete
         checking of clashes of abbreviation names with kind names *)
      (*  Translate type abbreviations and get the associated table *)
      let atable' = translateTypeAbbrevs tabbrevs ktable in
      let atable = 
         let mergeTypeAbbrevs' t1 t2 = mergeTypeAbbrevs t2 t1 ktable in
         let accsigstypeabbrevtabs = 
           (List.map (fun (_,_,t) -> t) accsigstables) in
         let usesigstypeabbrevtabs = 
           (List.map (fun (_,_,t) -> t) usesigstables) in
         let accstypeabbrevtabs = (List.map (fun (_,_,t) -> t) acctables) in
         let impstypeabbrevtabs = (List.map (fun (_,_,t) -> t) imptables) in
           (List.fold_left mergeTypeAbbrevs'  
                (List.fold_left mergeTypeAbbrevs' 
                     (List.fold_left mergeTypeAbbrevs' 
                          (List.fold_left mergeTypeAbbrevs' 
                               (mergeTypeAbbrevs' atable' atable)
                               accsigstypeabbrevtabs)
                          usesigstypeabbrevtabs)
                     accstypeabbrevtabs)
                impstypeabbrevtabs) 
      in

      (*  Translate local, global, closed, useonly, and exportdef constants
          and get the associated tables. *)
      let gconstlist = translateGlobalConstants gconsts ktable atable in
      let lconstlist = translateLocalConstants lconsts ktable atable in
      let cconstlist = translateClosedConstants cconsts ktable atable in
      let uconstlist = translateUseOnlyConstants true uconsts ktable atable in
      let econstlist = translateExportdefConstants true econsts ktable atable in
      
      (*  Merge all of the various constants into the constant table. *)
      let ctable = mergeGlobalConstants gconstlist ctable in
      let ctable = mergeLocalConstants lconstlist ctable in
      let ctable = mergeClosedConstants cconstlist ctable in
      let ctable = mergeUseonlyConstants uconstlist ctable in
      let ctable = mergeExportdefConstants econstlist ctable in
      
      (*  Apply fixity flags  *)
      let ctable = translateFixities fixities ctable in
      
      (*  Normalize the constant table.  This ensures that any kinds
          referred to in the constant table are correct.  *)
      let ctable = normalizeTable ktable ctable in
      let imps = normalizeImports ktable ctable atable imps in
      let accums = normalizeAccums ktable ctable atable accums in

      (*  Get local and global kind lists.  *)
      let globalkinds = Table.fold (getGlobalKind) ktable [] in
      let localkinds = Table.fold (getLocalKind) ktable [] in
      
      (*  Get local and global constant lists.  *)
      let globalconstants = 
        Table.fold (getConstant Absyn.GlobalConstant) ctable [] in
      let localconstants = 
        Table.fold (getConstant Absyn.LocalConstant) ctable [] in
      
      (*  Verify that all constants have a body, and
          all kinds have an arity.  *)
      if (checkConstantBodies ktable ctable) && (checkKindArities ktable) then
        (*  Get constant skeletons. *)
        let skeletons = (List.fold_left (getSkeleton) [] globalconstants) in
        let skeletons = 
          (List.fold_left (getSkeleton) skeletons localconstants) in
        let amod =
          Absyn.Module(name, imps, accums, ref ctable, ref ktable,
                       atable, [], globalkinds, localkinds, globalconstants, 
                       localconstants, ref [], skeletons, ref [], 
                       ref(Absyn.ClauseBlocks([]))) in
          let _ = Errormsg.log Errormsg.none
            "Translate.translateModule: Translated module." in
        amod
      else
        Absyn.ErrorModule
  | Preabsyn.Signature _ ->
      invalid_arg 
        "Types.translateModule: attempted to translate Preabsyn.Signature()"

(******************************************************************
*normalizeTable:
* Ensures that all kinds referred to in all constants in a constant
* table point at kinds in the given kind table.
******************************************************************)
and normalizeTable ktable ctable =
  (******************************************************************
  *normalizeType:
  * Change a type so that kinds in it are found in the kind table.
  ******************************************************************)
  let rec normalizeType ktable t =
    match t with
        Absyn.ArrowType(t1,t2) ->
          Absyn.ArrowType(normalizeType ktable t1, normalizeType ktable t2)
      | Absyn.ApplicationType(k,args) ->
          let ko = Table.find (Absyn.getKindSymbol k) ktable in
          if Option.isSome ko then
            Absyn.ApplicationType(Option.get ko, 
                                  List.map (normalizeType ktable) args)
          else
            Errormsg.impossible 
              Errormsg.none "Translate.normalizeType: invalid kind"
      | t' -> t'
  in

  (******************************************************************
  *normalizeConstant:
  ******************************************************************)
  let normalizeConstant ktable sym c =
    let Absyn.Constant(_, f, p, ed, uo, nd, c, tp, red, 
                       skel, tes, skelneed, need, ci, ct, i, pos) = c in
    if Option.isSome !skel then
      let Absyn.Skeleton(ty, p, b) = (Option.get !skel) in
      let ty' = normalizeType ktable ty in
        skel := Some(Absyn.Skeleton(ty',p,b))
    else
      ()
  in
    (Table.iter (normalizeConstant ktable) ctable;
     ctable)
