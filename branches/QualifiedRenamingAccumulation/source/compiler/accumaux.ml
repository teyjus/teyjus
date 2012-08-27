(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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

(****************************************************************************
 * AccumAux Module:
 * Supplementary functionality related to accumulation. Currently, this
 * functionality is mainly to support sort and constant renaming in
 * accumulation statements (i.e., accumulate, accum_sig, use_sig),
****************************************************************************)

(******************************************************************
* checkRenamingInjectivity:
* Check that accumulated signature/modules have qualified renaming mappings
* that are injective on a per module basis.
******************************************************************)
let checkRenamingInjectivity accums = 
  let eq x y =
    (((Preabsyn.isTypeRenamingP x) == (Preabsyn.isTypeRenamingP y))
    && (Symbol.equal
      (Preabsyn.getSymbol (Preabsyn.getRenamingToPsymbol x))
      (Preabsyn.getSymbol (Preabsyn.getRenamingToPsymbol y))))
  in
  let testItemInjectivity rs x =
    try let _ = List.find (eq x) rs in false with Not_found -> true
  in
  let rec testListInjectivity rs = 
    match rs with
      [] -> true
    | r::rs' -> (testItemInjectivity rs' r) && (testListInjectivity rs')
  in
  let checkRenamingInjectivityPerModule renamings = 
    match renamings with
      None -> true
    | Some rs -> testListInjectivity rs 
  in 
  (* Tests renaming injectivity for each module. *)
  let mapper a =            
    if checkRenamingInjectivityPerModule (Preabsyn.getAccumulateRenaming a)
    then None
    else Some (Preabsyn.getSymbolPos (Preabsyn.getAccumulatePsymbol a))
  in
  (* Fold all the errors. *)
  let folder _ = function 
    | None -> ()
    | (Some p) -> Errormsg.error p "Renaming mapping not injective."
  in
  let mapped = List.map mapper accums in List.fold_left folder () mapped

(******************************************************************
 * checkRenamingDomainElements
 * Checks to make sure that all the "from" renaming components refer
 * to valid symbol in the signature.
 ******************************************************************)
let checkRenamingDomainElements renameopt kindlist constantlist tabbrevlist = 
  let eq = Symbol.equal in 
  let finder r =
    let p = Preabsyn.getRenamingPos r in 
    let fromsym = Preabsyn.getSymbol (Preabsyn.getRenamingFromPsymbol r) in
    let fromstr = Symbol.name fromsym in 
    if not (Preabsyn.isTypeRenamingP r)
    then 
      let ktest = function  
        | (Preabsyn.Kind(Preabsyn.Symbol(n,a,b)::rest,c,d)) -> eq n fromsym
        | _ -> false
      in
      try 
        let _ = List.find ktest kindlist in ()
      with
        Not_found -> Errormsg.error p 
          (fromstr ^ " is not a global kind in this signature.")
    else
      let ctest = function  
        | (Preabsyn.Constant(Preabsyn.Symbol(n,a,b)::rest,c,d)) -> eq n fromsym
        | _ -> false
      in
      let ttest = function  
        | (Preabsyn.TypeAbbrev(Preabsyn.Symbol(n,a,b),c,d,e)) -> eq n fromsym
      in
      try 
        let _ = List.find ctest constantlist in ()
      with
        Not_found -> 
          try 
            let _ = List.find ttest tabbrevlist in ()
          with
            Not_found -> Errormsg.error p 
              (fromstr 
               ^ " is not a global constant/type abbreviation" ^
                 " in this signature.")
  in
  match renameopt with
    None -> () 
  | Some rs -> List.fold_left (fun _ _ -> () ) () (List.map finder rs)

(******************************************************************
* getRenameListForModule:
* Find renaming information for a given module.
******************************************************************)
let getRenameListForModule 
    (* The module to find. *)
    (name:string)
    (* unique id for a particular included file *)
    (id:int)
    (* Renaming information *)
    (renamings:Preabsyn.paccum list)
: Preabsyn.prenaming list option
= 
  let finder acc = 
    ((compare 
      (Symbol.name (Preabsyn.getSymbol (Preabsyn.getAccumulatePsymbol acc)))
      name) == 0)
      && (id == (Preabsyn.getAccumulateID acc))
  in
  try Preabsyn.getAccumulateRenaming (List.find finder renamings) with
    Not_found -> Errormsg.impossible Errormsg.none
      ("Translate.getSignatureRenamingInformation:'"^ name 
       ^ "' has not been accumulated or used.")

(******************************************************************
* addOmittedAccumSymbols:
* Add sorts/constants that have been omitted via qualifed accumulation
* statements to the modules which are being accumulated and to the
* symbols tables.
******************************************************************)

let addOmittedAccumSymbols accummods ktable ctable atable accums = 
  let addOmitted ktable ctable atable accum =
    match accum with 
      (Absyn.AccumulatedModule(n,s,id)) ->
        match s with
          Absyn.Signature(n,kl,cl,okl,ocl) ->
            let _ = (getRenameListForModule n id accummods) in
            let kindModification k = k in
            let constantModification c = c in  
            let tableBuilder modfunc namefunc table s = 
              let s = modfunc s in 
              Table.add (namefunc s) s table 
            in
            let renameBuilder modfunc list s = let s = modfunc s in s::list in
            let kl = List.fold_left (renameBuilder kindModification) kl okl in
            let cl = List.fold_left (renameBuilder constantModification) 
              cl ocl 
            in
          let ktable = 
            List.fold_left 
              (tableBuilder kindModification Absyn.getKindSymbol) ktable okl 
          in
          let ctable = 
            List.fold_left 
              (tableBuilder constantModification Absyn.getConstantSymbol) 
              ctable 
              ocl 
          in
          (ktable,ctable,atable,
           Absyn.AccumulatedModule(n,
                                   Absyn.Signature(n,kl,cl,okl,ocl),
                                   id))
        | Absyn.Module(_) -> 
          Errormsg.impossible Errormsg.none
            "translate.addOmittedAccums: not a signature"
        | Absyn.ErrorModule -> 
          Errormsg.impossible Errormsg.none
            "translate.addOmittedAccums: Invalid Module"
  in
  let folder (ktable,ctable,atable,accums) accum=
    let (ktable,ctable,atable,accum) = addOmitted ktable ctable atable accum in
    (ktable,ctable,atable,accum::accums)
  in
  List.fold_left folder (ktable,ctable,atable,[]) accums
(******************************************************************
* renameSymbol:
* If the renaming list is None or a renaming symbol is found in the list
* the symbol is renamed by identity or the provided function respectively.
* Otherwise, None is returned.
******************************************************************)
let renameSymbol 
    (* True if this symbol is a kind, false otherwise. *)
    (kindP:bool) 
    (* List of renaming entries to search through. If None the identity 
       function is used for renaming () *)
    (renamings:Preabsyn.prenaming list option ) 
    (* The symbol to search for *)
    (sym:Symbol.symbol) 
:Symbol.symbol option
=    match renamings with
      None -> Some sym
    | Some lst -> 
      try 
        match 
          List.find 
            (function 
               (Preabsyn.TypeRenaming
                  (Preabsyn.Symbol(f,_,_),Preabsyn.Symbol(t,_,_),_))
               -> (not kindP) && Symbol.equal f sym
            |  (Preabsyn.KindRenaming
                  (Preabsyn.Symbol(f,_,_),Preabsyn.Symbol(t,_,_),_))
                -> kindP && Symbol.equal f sym)
            lst
        with
          (Preabsyn.TypeRenaming(_,Preabsyn.Symbol(t,_,_),_)) -> Some t
        | (Preabsyn.KindRenaming(_,Preabsyn.Symbol(t,_,_),_)) -> Some t
      with
        Not_found -> None
(******************************************************************
* revertSymbol:
* Given a symbol that has already been renamed, find the original
* name for that symbol.
******************************************************************)
let revertSymbol 
    (* True if this symbol is a kind, false otherwise. *)
    (kindP:bool) 
    (* List of renaming entries to search through. If None the identity 
       function is used for renaming () *)
    (renamings:Preabsyn.prenaming list option ) 
    (* The symbol to search for *)
    (sym:Symbol.symbol) 
:Symbol.symbol option
=    match renamings with
      None -> Some sym
    | Some lst -> 
      try 
        match 
          List.find 
            (function 
               (Preabsyn.TypeRenaming
                  (Preabsyn.Symbol(t,_,_),Preabsyn.Symbol(f,_,_),_))
               -> (not kindP) && Symbol.equal f sym
            |  (Preabsyn.KindRenaming
                  (Preabsyn.Symbol(t,_,_),Preabsyn.Symbol(f,_,_),_))
                -> kindP && Symbol.equal f sym)
            lst
        with
          (Preabsyn.TypeRenaming(Preabsyn.Symbol(t,_,_),_,_)) -> Some t
        | (Preabsyn.KindRenaming(Preabsyn.Symbol(t,_,_),_,_)) -> Some t
      with
        Not_found -> Errormsg.impossible Errormsg.none
          ("translate.revertSymbol: Could not find " ^ (Symbol.name sym))
(******************************************************************
* renameKindFolder:
* Fold function for renaming pkinds. 
******************************************************************)
let renameKindFolder 
  (* Renaming information *)
  (renamesopt:Preabsyn.prenaming list option)
  (* The new list of pkinds *)
  (syms:Preabsyn.pkind list) 
  (* The kind to be renamed *)
  (pk:Preabsyn.pkind) 
: Preabsyn.pkind list
=
  match pk with 
    (Preabsyn.Kind(Preabsyn.Symbol(n,a,b)::rest,c,d)) ->
      (match renameSymbol true renamesopt n with
        None -> syms
      | Some s -> (Preabsyn.Kind(Preabsyn.Symbol(s,a,b)::rest,c,d))::syms)
  | _ -> Errormsg.impossible Errormsg.none 
      "Translate.renameKindFolder: pkind is not well-formed."
(******************************************************************
* omittedKindFolder:
* Fold function for omitted pkinds. 
******************************************************************)
let omittedKindFolder 
  (* Renaming information *)
  (renamesopt:Preabsyn.prenaming list option)
  (* The new list of pkinds *)
  (syms:Preabsyn.pkind list) 
  (* The kind to be renamed *)
  (pk:Preabsyn.pkind) 

: Preabsyn.pkind list
=
  match pk with 
    (Preabsyn.Kind(Preabsyn.Symbol(n,a,b)::rest,c,d)) ->
      (match renameSymbol true renamesopt n with
        None -> pk::syms
      | Some s -> syms)
  | _ -> Errormsg.impossible Errormsg.none 
      "Translate.omitKindFolder: pkind is not well-formed."
(******************************************************************
* renameType:
* Rename a ptype.
******************************************************************)
let rec renameType 
  (* Renaming information *)
  (renamesopt:Preabsyn.prenaming list option)
  (* Type to rename *)
  (t:Preabsyn.ptype)  
: Preabsyn.ptype
=
    match t with
      Preabsyn.Arrow(t1,t2,p) ->
        Preabsyn.Arrow(renameType renamesopt t1,renameType renamesopt t2,p)
    | Preabsyn.App(t1,t2,p) -> 
        Preabsyn.App(renameType renamesopt t1,renameType renamesopt t2,p)
    | Preabsyn.Atom(s,k,p) ->
      (match (renameSymbol true renamesopt s) with
        Some s -> Preabsyn.Atom(s,k,p)
      | None -> 
        (match (renameSymbol false renamesopt s) with
          Some s -> Preabsyn.Atom(s,k,p)
        | None -> Preabsyn.Atom(s,k,p)))
    | t' -> t'
(******************************************************************
* renameConstantFolder:
* Fold function for renaming pconstants.
******************************************************************)
let renameConstantFolder 
  (* Renaming information *)
  (renamesopt:Preabsyn.prenaming list option)
  (* The new list of pkinds *)
  (syms:Preabsyn.pconstant list) 
  (* The constant to be renamed *)
  (pc:Preabsyn.pconstant) 

: Preabsyn.pconstant list
=
  match pc with 
    (Preabsyn.Constant(Preabsyn.Symbol(n,a,b)::rest,c,d)) ->
      (match renameSymbol false renamesopt n with
        None -> syms
      | Some s -> 
        let c = match c with
            None -> None
          | Some t -> Some (renameType renamesopt t)
        in
        (Preabsyn.Constant(Preabsyn.Symbol(s,a,b)::rest,c,d))::syms)
  | _ -> Errormsg.impossible Errormsg.none 
    "Translate.renameConstantFolder: pconstant is not well-formed."
(******************************************************************
* omittedConstantFolder:
* Fold function for omitted pconstants.
******************************************************************)
let omittedConstantFolder 
  (* Renaming information *)
  (renamesopt:Preabsyn.prenaming list option)
  (* The new list of pkinds *)
  (syms:Preabsyn.pconstant list) 
  (* The constant to be renamed *)
  (pc:Preabsyn.pconstant) 

: Preabsyn.pconstant list
=
  match pc with 
    (Preabsyn.Constant(Preabsyn.Symbol(n,a,b)::rest,c,d)) ->
      (match renameSymbol false renamesopt n with
        Some _ -> syms
      | None -> 
        let c = match c with
            None -> None
          | Some t -> Some (renameType renamesopt t)
        in
        (Preabsyn.Constant(Preabsyn.Symbol(n,a,b)::rest,c,d))::syms)
  | _ -> Errormsg.impossible Errormsg.none 
    "Translate.omittedConstantFolder: pconstant is not well-formed."
(******************************************************************
* renameTypeAbvFolder:
* Fold function for renaming ptypeabrevs.
******************************************************************)
let renameTypeAbvsFolder 
  (* Renaming information *)
  (renamesopt:Preabsyn.prenaming list option)
  (* The new list of pkinds *)
  (syms:Preabsyn.ptypeabbrev list) 
  (* The type abbreviation to be renamed *)
  (tak:Preabsyn.ptypeabbrev) 
: Preabsyn.ptypeabbrev list
=  match tak with 
    (Preabsyn.TypeAbbrev(Preabsyn.Symbol(n,a,b),c,d,e)) ->
      (match renameSymbol false renamesopt n with
        None -> syms
      | Some s -> 
        let d = renameType renamesopt d in 
        (Preabsyn.TypeAbbrev(Preabsyn.Symbol(n,a,b),c,d,e))::syms)



