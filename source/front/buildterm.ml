(* for generating names of new logic variables *)
let newFVarBasename = ref "X"
let newFVarCounter = ref 0
let set_newFV_basename name =
  if name = (!newFVarBasename)
  then ()
  else newFVarBasename := name;
       newFVarCounter := 0


let nameInFVarTab name freeVarTab =
  Intmap.fold (fun i s v -> if v then v else (Absyn.getTypeSymbolName s = name)) 
              freeVarTab false

let generateFVarName freeVarTab =
  let nameInFVarTab name =
    Intmap.fold (fun i s v -> if v then v else (Absyn.getTypeSymbolName s = name)) 
                freeVarTab 
                false
  in
  let rec generate () =
    let name = newFVarCounter := (!newFVarCounter) + 1;
               (!newFVarBasename) ^ (string_of_int (!newFVarCounter)) in
    if nameInFVarTab name 
    then generate ()
    else name
  in
  generate ()

(* to make sure that changes in the C code don't 
   snowball to changes here, lookup the tag values 
   and store as constants *)
let tag_const = Ccode_stubs.getConstTag() 
let tag_fvar = Ccode_stubs.getFVarTag() 
let tag_bvar = Ccode_stubs.getBVarTag() 
let tag_abs = Ccode_stubs.getAbsTag()
let tag_app = Ccode_stubs.getAppTag()


let make_constmap constTab =
  let insert symb c imap =
    Intmap.add (Absyn.getConstantIndex c) c imap
  in
  Table.fold insert constTab Intmap.empty


let rec build_term tmPtr freeVarTab constIdxMap =
  let rec build_term_aux tmPtr freeVarTab =
    match (Ccode_stubs.getTermTag tmPtr) with
        t when t = tag_const -> 
          let idx = Ccode_stubs.getConstData(tmPtr) in
          (match Intmap.find idx constIdxMap with
               Some(const) ->
                 (Absyn.ConstantTerm(const, [], Errormsg.none), freeVarTab)
             | None ->
                 Errormsg.error Errormsg.none
                                ("Error: build_solution: build_term: No constant matching index `" ^ (string_of_int idx) ^ "' found.");
                 (Absyn.ErrorTerm, freeVarTab))
      | t when t = tag_fvar -> 
          let idx = Ccode_stubs.getFVarData(tmPtr) in
          (match Intmap.find idx freeVarTab with
               Some(tysymb) ->
                 (Absyn.FreeVarTerm(Absyn.NamedFreeVar(tysymb),Errormsg.none), freeVarTab)
             | None -> (* previously unseen free var *)
                 let name = generateFVarName freeVarTab in
                 let tysymb = Absyn.ImplicitVar(Symbol.symbol name, ref None, ref true, ref None) in
                 let freeVarTab' = Intmap.add idx tysymb freeVarTab in
                 (Ccode_stubs.setFVarTabName idx name);
                 (Absyn.FreeVarTerm(Absyn.NamedFreeVar(tysymb), Errormsg.none), freeVarTab'))
      | t when t = tag_bvar -> 
          let idx = Ccode_stubs.getBVarData(tmPtr) in
          (Absyn.BoundVarTerm(Absyn.DBIndex(idx),Errormsg.none), freeVarTab)
      | t when t = tag_abs -> 
          let (numabs, body_tmptr) = Ccode_stubs.getAbsData(tmPtr) in
          let (body, freeVarTab') = build_term_aux body_tmptr freeVarTab in
          let rec build_abs i =
            (match i with
                 0 -> body
               | n -> Absyn.AbstractionTerm(
                        Absyn.NestedAbstraction(Absyn.BoundVar(Symbol.generate (), ref None, ref true, ref None),
                                                build_abs (i-1)),
                        Errormsg.none))
          in
          (build_abs numabs, freeVarTab')
      | t when t = tag_app -> 
          let (head_tmptr, arg_tmptrs, numArgs) = Ccode_stubs.getAppData(tmPtr) in
          let (head, freeVarTab') = build_term_aux head_tmptr freeVarTab in
          let (args, freeVarTab'') = 
            List.fold_left (fun (args, fvtab) tmptr -> 
                              let (arg, fvtab') = build_term_aux tmptr fvtab in
                              (List.append args [arg], fvtab')) 
                           ([], freeVarTab') 
                           arg_tmptrs
          in
          (Absyn.ApplicationTerm(Absyn.FirstOrderApplication(head, args, numArgs), Errormsg.none), freeVarTab'')
  in
  build_term_aux tmPtr freeVarTab

  

let build_subst lpmod freeVarTab constIdxMap = 
  let numfvars = Ccode_stubs.getNumQueryVars() in
(*  let _ = print_endline ("There are "^(string_of_int numfvars)^" free variables.") in *)
  let rec build_subst_aux i freeVarTab =
    if i = numfvars
    then
      ([], freeVarTab)
    else
      match Intmap.find i freeVarTab with
          Some(tysymb) ->
            let (tm, freeVarTab') = (*set_newFV_basename (Absyn.getTypeSymbolName tysymb); *)
                                    build_term (Ccode_stubs.getSubTerm(i)) freeVarTab constIdxMap in
            let (rest, freeVarTab'') = build_subst_aux (i + 1) freeVarTab' in
            ((tysymb, tm) :: rest, freeVarTab'')
        | None ->
            (* this shouldn't ever happen, but try to continue if it does. *)
            Errormsg.error Errormsg.none 
                           ("Error: build_solution: build_subst: No entry in free variable table with index "^(string_of_int i));
            build_subst_aux (i + 1) freeVarTab
  in
  build_subst_aux 0 freeVarTab

let build_disprs lpmod freeVarTab constIdxMap =
  let dispr_tmptrs = Ccode_stubs.getDisSet () in
  let rec build_disprs_aux disprs freeVarTab =
    match disprs with
        ((tmptr1, tmptr2) :: rest) ->
          let (tm1, freeVarTab') = build_term tmptr1 freeVarTab constIdxMap in
          let (tm2, freeVarTab'') = build_term tmptr2 freeVarTab' constIdxMap in
          let (tmprs, freeVarTab''') = build_disprs_aux rest freeVarTab'' in
          ((tm1, tm2)::tmprs, freeVarTab''')
      | [] ->
          ([], freeVarTab)
  in
  let (disprs, freeVarTab') = build_disprs_aux dispr_tmptrs freeVarTab in
  disprs

let build_solution lpmod freeVarTab =
  let constIdxMap = make_constmap (Absyn.getModuleConstantTable lpmod) in
  let (subst, freeVarTab') = build_subst lpmod freeVarTab constIdxMap in
  let disprs = build_disprs lpmod freeVarTab' constIdxMap in
  Ccode_stubs.resetFreeVarTab();
  (subst, disprs)
