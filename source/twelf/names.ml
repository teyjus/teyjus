  (*! structure IntSyn = IntSyn' !*)

  exception Error of string

  (*
     Unprintable is raised when trying to resolve the names
     of unnamed variables.  Usually, this signals an error
     in Twelf; the only exception is the use of anonymous
     bound variables [_] or {_} in the source.
  *)
  exception Unprintable

  (* argNumber (fix) = minimum # of explicit arguments required *)
  (* for operator with fixity fix (0 if there are no requirements) *)
let argNumber f =
  match f with
      (Fixity.Nonfix) -> 0
    | (Fixity.Infix _) -> 2
    | (Fixity.Prefix _) -> 1
    | (Fixity.Postfix _) -> 1

  (* checkAtomic (name, V, n) = ()
     if V expects exactly n arguments,
     raises Error(msg) otherwise
  *)
let checkAtomic args =
  match args with
      (name, IntSyn.Pi (D, V), 0) -> true
      (* allow extraneous arguments, Sat Oct 23 14:18:27 1999 -fp *)
      (* raise Error ("Constant " ^ name ^ " takes too many explicit arguments for given fixity") *)
    | (name, IntSyn.Pi (D, V), n) ->
	checkAtomic (name, V, n-1)
    | (_, IntSyn.Uni _, 0) -> true
    | (_, IntSyn.Root _, 0) -> true
    | (name, _, _) -> false

  (* checkArgNumber (c, n) = ()
     if constant c expects exactly n explicit arguments,
     raises Error (msg) otherwise
  *)
let checkArgNumber args =
  match args with
      (IntSyn.ConDec (name, _, i, _, V, L), n) ->
	checkAtomic (name, V, i+n)
    | (IntSyn.SkoDec (name, _, i, V, L), n) ->
	checkAtomic (name, V, i+n)
    | (IntSyn.ConDef (name, _, i, _, V, L, _), n) ->
	checkAtomic (name, V, i+n)
    | (IntSyn.AbbrevDef (name, _, i, _, V, L), n) ->
	checkAtomic (name, V, i+n)

  (* checkFixity (name, cidOpt, n) = ()
     if n = 0 (no requirement on arguments)
     or name is declared and has n exactly explicit arguments,
     raises Error (msg) otherwise
  *)
let checkFixity args =
  match args with
      (name, _, 0) -> ()
    | (name, cid, n) ->
        if checkArgNumber (IntSyn.sgnLookup (cid), n) then ()
        else raise Error ("Constant " ^ name ^ " takes too few explicit arguments for given fixity")

  (****************************************)
  (* Constants Names and Name Preferences *)
  (****************************************)

  (*
     Names (strings) are associated with constants (cids) in two ways:
     (1) There is an array nameArray mapping constants to print names (string),
     (2) there is a hashtable sgnHashTable mapping identifiers (strings) to constants.

     The mapping from constants to their print names must be maintained
     separately from the global signature, since constants which have
     been shadowed must be marked as such when printing.  Otherwise,
     type checking can generate very strange error messages such as
     "Constant clash: c <> c".

     In the same table we also record the fixity property of constants,
     since it is more a syntactic then semantic property which would
     pollute the lambda-calculus core.

     The mapping from identifers (strings) to constants is used during
     parsing.

     There are global invariants which state the mappings must be
     consistent with each other.
  *)

type qid = Qid of string list * string
    
let qidToString (Qid (ids, name)) =
      List.foldr (fn (id, s) -> id ^ "." ^ s) name ids

let validateQualName l =
  match l with
      [] -> None
    | (id::ids) ->
        if List.exists (fn s -> s = "") l
        then None
        else Some (Qid (rev ids, id))

let stringToQid name =
      validateQualName (rev (String.fields (fn c -> c = #".") name))

let unqualified arg =
  match arg with
      (Qid ([], id)) -> Some id
    | _ -> None

type namespace = IntSyn.mid Symboltable.table ref * IntSyn.cid Symboltable.table ref

let newNamespace () : namespace = 
        (ref Symboltable.empty, ref Symboltable.empty)

let insertConst ((structTable, constTable), cid) =
  let condec = IntSyn.sgnLookup cid in
  let id = IntSyn.conDecName condec in
(*
  match Symboltable.insertShadow constTable (id, cid) with
      None -> ()
    | Some _ ->
        raise Error ("Shadowing: A constant named " ^ id
                     ^ "\nhas already been declared in this signature")
*)
  constTable := Symboltable.insert (!constTable) (Symb.symbol id) cid; ()  

let insertStruct ((structTable, constTable), mid) =
  let strdec = IntSyn.sgnStructLookup mid in
  let id = IntSyn.strDecName strdec in
(*
  match StringTree.insertShadow structTable (id, mid) with
      None -> ()
    | Some _ ->
         raise Error ("Shadowing: A structure named " ^ id
                      ^ "\nhas already been declared in this signature")
*)
  structTable := Symboltable.insert (!structTable) (Symb.symbol id) strdec; ()
     
let appConsts f (structTable, constTable) =
  Symboltable.iter (!constTable) (fun s a -> constTable := Symboltable.insert (!constTable) s (f (Symb.name s,a)))
      (* StringTree.app f constTable *)

let appStructs f (structTable, constTable) =
  Symboltable.iter (!structTable) (fun s a -> structTable := Symboltable.insert (!structTable) s (f (Symb.name s,a)))
(*      StringTree.app f structTable *)

let fromTo f (from, to) = 
  if from >= to 
  then ()
  else (f from; fromTo f (from+1, to))

let maxCid = Global.maxCid
let shadowArray : IntSyn.cid option array ref =
          ref (Array.make (maxCid+1) None)
let shadowClear () = shadowArray := Array.make (Array.length (!shadowArray)) None
let fixityArray : Fixity.fixity array ref =
          ref (Array.make (maxCid+1) Fixity.Nonfix)
let fixityClear () = fixityArray := Array.make (Array.length (!fixityArray)) Fixity.Nonfix
let namePrefArray : (string list * string list) option array ref=
          ref (Array.make (maxCid+1) None)
let namePrefClear () = namePrefArray := Array.make (Array.length (!namePrefArray)) None

let topNamespace : IntSyn.cid Symboltable.table ref = ref Symboltable.empty
let topInsert = (*Symboltable.insertShadow topNamespace *) fun k v -> topNamespace := Symboltable.insert (!topNamespace) (Symb.symbol k) v; None
let topLookup = fun k -> Symboltable.lookup (!topNamespace) (Symb.symbol k)
let topDelete = fun k -> topNamespace := Symboltable.remove (!topNamespace) (Symb.symbol k); ()
let topClear () = topNamespace := Symboltable.empty

let dummyNamespace = (ref Symboltable.empty, ref Symboltable.empty) : namespace

let maxMid = Global.maxMid
let structShadowArray : IntSyn.mid option array ref =
      ref (Array.make (maxMid+1) None)
let structShadowClear () = structShadowArray := Array.make (Array.length (!structShadowArray)) None
let componentsArray : namespace array ref =
      ref (Array.make (maxMid+1) dummyNamespace)
let componentsClear () = componentsArray := Array.make (Array.length (!componentsArray)) dummyNamespace

let topStructNamespace : IntSyn.mid Symboltable.table ref = ref Symboltable.empty
let topStructInsert = fun k v -> topNamespace := Symboltable.insert (!topNamespace) (Symb.symbol k) v; None
let topStructLookup = fun k -> Symboltable.lookup (!topNamespace) (Symb.symbol k)
let topStructDelete = fun k -> topNamespace := Symboltable.remove (!topNamespace) (Symb.symbol k); ()
let topStructClear () = topNamespace := Symboltable.empty

    (* installName (name, cid) = ()
       Effect: update mapping from identifiers
               to constants, taking into account shadowing
    *)
  let installConstName cid =
    let condec = IntSyn.sgnLookup cid in
    let id = IntSyn.conDecName condec in
      match topInsert (id, cid) with
          None -> ()
        | Some (_, cid') -> Array.set (!shadowArray) cid (Some cid')
    
  let uninstallConst cid =
    let condec = IntSyn.sgnLookup cid in
    let id = IntSyn.conDecName condec in
      match Array.get (!shadowArray) cid with
          None -> (if topLookup id = Some cid
                          then topDelete id
                        else ())
        | Some cid' -> (topInsert (id, cid');
                       Array.set (!shadowArray) cid None);
                       Array.set (!fixityArray) cid Fixity.Nonfix;
                       Array.set (!namePrefArray) cid None

  let installStructName mid =
    let strdec = IntSyn.sgnStructLookup mid in
    let id = IntSyn.strDecName strdec in
      match topStructInsert (id, mid) with
          None -> ()
        | Some (_, mid') -> Array.set (!structShadowArray) mid (Some mid')

  let uninstallStruct mid =
    let strdec = IntSyn.sgnStructLookup mid in
    let id = IntSyn.strDecName strdec in
      match Array.get (!structShadowArray) mid with
          None -> if topStructLookup id = Some mid
                  then topStructDelete id
                  else ()
        | Some mid' -> (topStructInsert (id, mid');
                       Array.set (!structShadowArray) mid None);
	               Array.set (!componentsArray) mid dummyNamespace

  let resetFrom (mark, markStruct) =
    let (limit, limitStruct) = IntSyn.sgnSize () in
    let ct f (i, j) = if j < i then ()
                      else (f j; ct f (i, j-1))
    in
    ct uninstallConst (mark, limit-1);
    ct uninstallStruct (markStruct, limitStruct-1)

    (* reset () = ()
       Effect: clear name tables related to constants
    *)
  let reset () = (topClear (); topStructClear ();
                  shadowClear (); structShadowClear ();
                  fixityClear (); namePrefClear ();
                  componentsClear ())

  let structComps mid = fst (Array.get (!componentsArray) mid)
  let constComps mid = snd (Array.get (!componentsArray) mid)

  let findStruct args =
    (structTable, [id]) ->
       Symboltable.lookup (!structTable) (Symb.symbol id)
  | (structTable, id::ids) ->
      (match Symboltable.lookup (!structTable) (Symb.symbol id) with
           None -> None
         | Some mid -> findStruct (structComps mid, ids))

  let findTopStruct l =
    match l with
        [id] ->
          Symboltable.lookup (!topStructNamespace) (Symb.symbol id)
      | (id::ids) ->
          (match Symboltable.lookup (!topStructNamespace) (Symb.symbol id) with
               None -> None
             | Some mid -> findStruct (structComps mid, ids))

  let findUndefStruct args =
    match args with
        (structTable, [id], ids') ->
          (match Symboltable.lookup (!structTable) (Symb.symbol id) with
               None -> Some (Qid (rev ids', id))
             | Some _ -> None)
      | (structTable, id::ids, ids') ->
          (match Symboltable.lookup (!structTable) (Symb.symbol id) with
               None -> Some (Qid (rev ids', id))
             | Some mid -> findUndefStruct (structComps mid, ids, id::ids'))

  let findTopUndefStruct arg
    match arg with
        [id] ->
          (match Symboltable.lookup (!topStructNamespace) (Symb.symbol id) with
               None -> Some (Qid ([], id))
             | Some _ -> None)
      | (id::ids) ->
          (match Symboltable.lookup (!topStructNamespace) (Symb.symbol id) with
               None -> Some (Qid ([], id))
             | Some mid -> findUndefStruct (structComps mid, ids, [id]))

  let constLookupIn args =
    match args with
        ((structTable, constTable), Qid ([], id)) ->
          Symboltable.lookup (!constTable) (Symb.symbol id)
      | ((structTable, constTable), Qid (ids, id)) ->
          (match findStruct ((!structTable), ids) with
               None -> None
             | Some mid -> Symboltable.lookup (constComps mid) (Symb.symbol id))

  let structLookupIn args =
    match args with
        ((structTable, constTable), Qid ([], id)) ->
          Symboltable.lookup (!structTable) (Symb.symbol id)
      | ((structTable, constTable), Qid (ids, id)) ->
          (match findStruct ((!structTable), ids) with
               None -> None
             | Some mid -> Symboltable.lookup (structComps mid) (Symb.symbol id))

  let constUndefIn args =
    match args with
        ((structTable, constTable), Qid ([], id)) ->
          (match Symboltable.lookup (!constTable) (Symb.symbol id) with
               None -> Some (Qid ([], id))
             | Some _ -> None)
      | ((structTable, constTable), Qid (ids, id)) ->
          (match findUndefStruct (structTable, ids, []) with
               (Some _) as opt -> opt
             | None ->
                (match Symboltable.lookup (constComps (valOf (findStruct (structTable, ids)))) (Symb.symbol id) with
                     None -> Some (Qid (ids, id))
                   | Some _ -> None))

  let structUndefIn args =
    match args with
        ((structTable, constTable), Qid ([], id)) ->
          (match Symboltable.lookup (!structTable) (Symb.symbol id) with
               None -> Some (Qid ([], id))
             | Some _ -> None)
      | ((structTable, constTable), Qid (ids, id)) ->
          (match findUndefStruct (structTable, ids, []) with
               (Some _) as opt -> opt
             | None ->
                  (match Symboltable.lookup (structComps (valOf (findStruct (structTable, ids)))) (Symb.symbol id) with
                       None -> Some (Qid (ids, id))
                     | Some _ -> None))

    (* nameLookup (qid) = SOME(cid),  if qid refers to cid in the current context,
                        = None,       if there is no such constant
    *)
  let constLookup arg =
    match arg with
        (Qid ([], id)) ->
          Symboltable.lookup (!topNamespace) (Symb.symbol id)
      | (Qid (ids, id)) ->
          (match findTopStruct ids with
               None -> None
             | Some mid -> Symboltable.lookup (constComps mid) (Symb.symbol id))

  let structLookup arg =
    match arg with
        (Qid ([], id)) ->
          Symboltable.lookup (!topStructNamespace) (Symb.symbol id)
      | (Qid (ids, id)) ->
          (match findTopStruct ids with
               None -> None
             | Some mid -> Symboltable.lookup (structComps mid) (Symb.symbol id))

  let constUndef arg =
    match arg with
        (Qid ([], id)) ->
          (match Symboltable.lookup (!topNamespace) (Symb.symbol id) with
               None -> Some (Qid ([], id))
             | Some _ -> None)
      | (Qid (ids, id)) ->
        (match findTopUndefStruct ids with
             (Some _) as opt -> opt
           | None -> 
               (match Symboltable.lookup (constComps (valOf (findTopStruct ids))) (Symb.symbol id) with
                    None -> Some (Qid (ids, id))
                  | Some _ -> None))

  let structUndef arg =
    match arg with
        (Qid ([], id)) ->
          (match Symboltable.lookup (!topStructNamespace) (Symb.symbol id)
               None -> Some (Qid ([], id))
             | Some _ -> None)
      | (Qid (ids, id)) ->
          (match findTopUndefStruct ids with
            (Some _) as opt -> opt
            | None -> 
               (match Symboltable.lookup (structComps (valOf (findTopStruct ids))) (Symb.symbol id) with
                    None -> Some (Qid (ids, id))
                  | Some _ -> None))

  let structPath (mid, ids) =
    let strdec = IntSyn.sgnStructLookup mid in
    let ids' = IntSyn.strDecName strdec::ids in
    match IntSyn.strDecParent strdec with
        None -> ids'
      | Some mid' -> structPath (mid', ids')
      
  let maybeShadow args =
    match args with
        (qid, false) -> qid
      | (Qid ([], id), true) -> Qid ([], "%" ^ id ^ "%")
      | (Qid (id::ids, name), true) -> Qid ("%" ^ id ^ "%"::ids, name)

  let conDecQid condec =
    let id = IntSyn.conDecName condec in
      match  IntSyn.conDecParent condec with
          None -> Qid ([], id)
        | Some mid -> Qid (structPath (mid, []), id)
        
    (* constQid (cid) = qid,
       where `qid' is the print name of cid
    *)
  let constQid cid =
    let condec = IntSyn.sgnLookup cid in
    let qid = conDecQid condec in
    maybeShadow (qid, constLookup qid <> Some cid)

  let structQid mid =
    let strdec = IntSyn.sgnStructLookup mid in
    let id = IntSyn.strDecName strdec in
    let qid = 
      match IntSyn.strDecParent strdec  with
          None -> Qid ([], id)
        | Some mid -> Qid (structPath (mid, []), id)
    in
    maybeShadow (qid, structLookup qid <> Some mid)

    (* installFixity (cid, fixity) = ()
       Effect: install fixity for constant cid,
               possibly print declaration depending on chatter level
    *)
  let installFixity (cid, fixity) =
    let name = qidToString (constQid cid) in
    checkFixity (name, cid, argNumber fixity);
    Array.update (fixityArray, cid, fixity)
   
    (* getFixity (cid) = fixity
       fixity defaults to Fixity.Nonfix, if nothing has been declared
    *)
  let getFixity (cid) = Array.get (!fixityArray) cid

    (* fixityLookup qid = fixity
       where fixity is the fixity associated with constant named qid,
       defaults to Fixity.Nonfix if qid or fixity are undeclared
    *)
  let fixityLookup qid =
    (match constLookup qid with
         None -> Fixity.Nonfix
       | Some cid -> getFixity cid)

    (* Name Preferences *)
    (* ePref is the name preference for existential variables of given type *)
    (* uPref is the name preference for universal variables of given type *)

    (* installNamePref' (cid, (ePref, uPref)) see installNamePref *)
  let installNamePref' (cid, (ePref, uPref)) =
    let L = IntSyn.constUni (cid) in
    let _ = match L with
	        IntSyn.Type ->
		       raise Error ("Object constant " ^ qidToString (constQid cid) ^ " cannot be given name preference\n"
				    ^ "Name preferences can only be established for type families")
	      | IntSyn.Kind -> ()
    in
    Array.set (!namePrefArray) cid (Some (ePref, uPref))

    (* installNamePref (cid, (ePref, uPrefOpt)) = ()
       Effect: install name preference for type family cid
       raise Error if cid does not refer to a type family
    *)
  let installNamePref args =
    match args with
        (cid, (ePref, [])) =
          installNamePref' (cid, (ePref, [String.lowercase_ascii (hd ePref)]))
      | (cid, (ePref, uPref)) =
          installNamePref' (cid, (ePref, uPref))
      
  let getNamePref cid = Array.get (!namePrefArray) cid

  let installComponents (mid, namespace) =
        Array.set (!componentsArray) mid namespace

  let getComponents mid =
	Array.get (!componentsArray) mid

    (* local names are more easily re-used: they don't increment the
       counter associated with a name
    *)
  type extent = Local | Global
  type role = Exist | Univ of extent

  let extent extent =
    match extent with
        (Exist) -> Global
      | (Univ (ext)) -> ext

  let namePrefOf'' args =
    match args with
        (Exist, None) -> "X"
      | (Univ _, None) -> "x"
      | (Exist, Some(ePref, uPref)) -> hd ePref
      | (Univ _, Some(ePref, uPref)) -> hd uPref

  let namePrefOf' args =
    match args with
        (Exist, None) -> "X"
      | (Univ _, None) -> "x"
      | (role, Some(IntSyn.Const cid)) -> namePrefOf'' (role, Array.get (!namePrefArray) cid)
      | (role, Some(IntSyn.Def cid)) -> namePrefOf'' (role, Array.get (!namePrefArray) cid)
        (* the following only needed because reconstruction replaces
           undetermined types with FVars *)
      | (role, Some(IntSyn.FVar _)) -> namePrefOf'' (role, None)

      | (role, Some(IntSyn.NSDef cid)) -> namePrefOf'' (role, Array.sget (!namePrefArray) cid)

    (* namePrefOf (role, V) = name
       where name is the preferred base name for a variable with type V

       V should be a type, but the code is robust, returning the default "X" or "x"
    *)
  let namePrefOf (role, V) -> namePrefOf' (role, IntSyn.targetHeadOpt V)

  (******************)
  (* Variable Names *)
  (******************)

  (*
     Picking variable names is tricky, since we need to avoid capturing.
     This is entirely a matter of parsing and printing, since the
     internal representation relies on deBruijn indices and explicit
     substitutions.

     During parsing, a name is resolved as follows:
       lower case id => bound variable, constant, error
       upper case id => bound variable, constant, free variable
     where a free variable could become either an FVar
     (in constant declarations) or an EVar (in queries).

     Names are either given by the declaration or query itself, or
     assigned as late as possible.  For example, EVars which are never
     printed are never assigned a name.

     This may be a problem for contexts: if a name is not assigned when
     a declaration is entered into the context, terms in this context
     may not be printable.  Function decName below guarantees that new
     names are assigned to variables added to a context.
  *)

  (*
     There are three data structures:
     1. varTable mapping names (strings) to EVars and FVar types
          -- Actually, FVar types now handled entirely in recon-term.fun
          -- where there needs to be more info for approximate types.
          -- I don't see why EVar/BVar names should be generated apart from
          -- FVar names anyway, since the latter are printed with "`".
          -- -kw
     2. evarList mapping EVars to names (string)
     3. indexTable mapping base names B to integer suffixes to generate
        new names B1, B2, ...

     These are reset for each declaration or query, since
     EVars and FVars are local.
  *)
  type varEntry = EVAR of IntSyn.Exp (* X *)
      (* remove this datatype? -kw *)

    (* varTable mapping identifiers (strings) to EVars and FVars *)
    (* A hashtable is too inefficient, since it is cleared too often; *)
    (* so we use a red/black trees instead *)
  let varTable : varEntry Symboltable.table ref = ref Symboltable.empty
  let varInsert = fun k v -> varTable := Symboltable.insert (!varTable) (Symb.symbol k) v; None
  let varLookup = fun k -> Symboltable.lookup (!varTable) (Symb.symbol k)
  let varClear () = varTable := Symboltable.empty


    (* what is this for?  -kw *)
  let varContext : IntSyn.dctx ref = ref IntSyn.Null

    (* evarList mapping EVars to names *)
    (* names are assigned only when EVars are parsed or printed *)
    (* the mapping must be implemented as an association list *)
    (* since EVars are identified by reference *)
    (* an alternative becomes possible if time stamps are introduced *)
  let evarList : (IntSyn.Exp * string) list ref = ref []

  let evarReset () = (evarList := [])
  let evarLookup (X) =
    let evlk args =
      match args with
          (r, []) = None
	| (r, (IntSyn.EVar(r',_,_,_), name)::l) ->
	        if r = r' then Some(name) else evlk (r, l)
        | (r, ((IntSyn.AVar(r'), name)::l)) ->
	        if r = r' then Some(name) else evlk (r, l)
    in
    match X with
	IntSyn.EVar(r,_,_,_) -> evlk (r, (!evarList))
      | IntSyn.AVar(r) -> evlk (r, (!evarList))

  let evarInsert entry = (evarList := entry::(!evarList))

  let namedEVars () = !evarList

    (* Since constraints are not printable at present, we only *)
    (* return a list of names of EVars that have constraints on them *)
    (* Note that EVars which don't have names, will not be considered! *)
  let evarCnstr' args =
    match args with
        ([], acc) -> acc
      | (((IntSyn.EVar(ref(None), _, _, cnstrs), name) as Xn)::l, acc) ->
          (match Constraints.simplify (!cnstrs) with
               [] -> evarCnstr' (l, acc)
             | (_::_) -> evarCnstr' (l, Xn::acc))
      | (_::l, acc) -> evarCnstr' (l, acc)
  let evarCnstr () = evarCnstr' (!evarList, [])

    (* The indexTable maps a name to the last integer suffix used for it.
       The resulting identifer is not guaranteed to be new, and must be
       checked against the names of constants, FVars, EVars, and BVars.
    *)
  let indexTable : int Symboltable.table ref = ref Symboltable.empty
  let indexInsert = fun k v -> indexTable := Symboltable.insert (!indexTable) (Symb.symbol k) v; None
  let indexLookup = fun k -> Symboltable.lookup (!indexTable) (Symb.symbol k)
  let indexClear () = indexTable := Symboltable.empty

  let nextIndex' args =
    match args with
        (name, None) -> (indexInsert (name, 1); 1)
      | (name, Some(i)) -> (indexInsert (name, i+1); i+1)

    (* nextIndex (name) = i
       where i is the next available integer suffix for name,
       starting at 1.
       Effect: initialize or increment the indexTable entry for name
    *)
  let nextIndex (name) = nextIndex' (name, indexLookup (name))

    (* varReset () = ()
       Effect: clear variable tables
       This must be called for each declaration or query
    *)
  let varReset G = (varClear (); evarReset (); indexClear ();
                      varContext := G)

    (* addEVar (X, name) = ()
       effect: adds (X, name) to varTable and evarList
       assumes name not already used *)
  let addEVar (X, name) =
      (evarInsert (X, name);
       varInsert (name, EVAR(X)))

  let getEVarOpt (name) =
    (match varLookup name with
	 None -> None
       | Some(EVAR(X)) -> Some(X))

    (* varDefined (name) = true iff `name' refers to a free variable, *)
    (* which could be an EVar for constant declarations or FVar for queries *)
  let varDefined (name) =
    (match varLookup name with
	 None -> false
       | Some _ -> true)

    (* conDefined (name) = true iff `name' refers to a constant *)
  let conDefined (name) =
    (match constLookup (Qid ([], name)) with
	 None -> false
       | Some _ -> true)

    (* ctxDefined (G, name) = true iff `name' is declared in context G *)
  let ctxDefined (G, name) =
    let cdfd arg =
      match arg with
          (IntSyn.Null) -> false
	| (IntSyn.Decl(G', IntSyn.Dec(Some(name'),_))) ->
            name = name' || cdfd G'
        | (IntSyn.Decl(G', IntSyn.BDec(Some(name'),_))) ->
            name = name' || cdfd G'
        | (IntSyn.Decl(G', IntSyn.NDec(Some(name')))) ->
            name = name' || cdfd G'
        | (IntSyn.Decl(G', _)) -> cdfd G'
    in
    cdfd G

    (* tryNextName (G, base) = baseN
       where N is the next suffix such that baseN is unused in
       G, as a variable, or as a constant.
    *)
  let tryNextName (G, base) =
    let name = base ^ Int.toString (nextIndex (base)) in
    if varDefined name || conDefined name || ctxDefined (G,name)
    then tryNextName (G, base)
    else name

  let findNameLocal (G, base, i) =
    let name = base ^ (if i = 0 then "" else (string_of_int i)) in
    if varDefined name || conDefined name || ctxDefined (G, name)
    then findNameLocal (G, base, i+1)
    else name

  let findName args =
    match args with
        (G, base, Local) -> findNameLocal (G, base, 0)
      | (G, base, Global) -> tryNextName (G, base)
        

  let takeNonDigits = Substring.takel (not o Char.isDigit)

    (* baseOf (name) = name',
       where name' is the prefix of name not containing a digit
    *)
  let baseOf (name) = Substring.string (takeNonDigits (Compat.Substring.full name))

    (* newEvarName (G, X) = name
       where name is the next unused name appropriate for X,
       based on the name preference declaration for A if X:A
    *)
  let newEVarName (G, X) =
    match (G, X) with
        (G, IntSyn.EVar(r, _, V, Cnstr)) ->
	  (* use name preferences below *)
          let name = tryNextName (G, namePrefOf (Exist, V)) in
          (evarInsert (X, name); name)
      | (G, IntSyn.AVar(r)) ->
	  (* use name preferences below *)
	  let name = tryNextName (G, namePrefOf' (Exist, None)) in
	  (evarInsert (X, name);
	   name)

    (* evarName (G, X) = name
       where `name' is the print name X.
       If no name has been assigned yet, assign a new one.
       Effect: if a name is assigned, update varTable
    *)
  let evarName (G, X) =
    (match evarLookup X with
         None -> let name = newEVarName (G, X) in
		 (varInsert (name, EVAR(X));
			 name)
       | Some (name) -> name)


    (* bvarName (G, k) = name
       where `name' is the print name for variable with deBruijn index k.
       Invariant: 1 <= k <= |G|
                  G_k must assign a name
       If no name has been assigned, the context might be built the wrong
       way---check decName below instread of IntSyn.Dec
    *)
    let bvarName (G, k) =
        case IntSyn.ctxLookup (G, k)
	  of IntSyn.Dec(Some(name), _) -> name
	   | IntSyn.ADec(Some(name), _) ->  name
	   | IntSyn.NDec(Some(name)) ->  name (* Evars can depend on NDec :-( *)
	   | IntSyn.ADec(None, _) -> "ADec_" 
	   | IntSyn.Dec(None, _) -> "Dec_" 
	   | _ -> raise Unprintable

    (* decName' role (G, D) = G,D'
       where D' is a possible renaming of the declaration D
       in order to avoid shadowing other variables or constants
       If D does not assign a name, this picks, based on the name
       preference declaration.
    *)
    let decName' role (G, IntSyn.Dec (None, V)) ->
        let
	  let name = findName (G, namePrefOf (role, V), extent (role))
	in
	  IntSyn.Dec (Some(name), V)
	end
      | decName' role (G, D as IntSyn.Dec (Some(name), V)) ->
	if varDefined name || conDefined name
	  || ctxDefined (G, name)
	  then IntSyn.Dec (Some (tryNextName (G, baseOf name)), V)
	else D
      | decName' role (G, D as IntSyn.BDec (None, b as (cid, t))) ->
        (* use #l as base name preference for label l *)
	let
	  let name = findName (G, "#" ^ IntSyn.conDecName (IntSyn.sgnLookup cid), Local)
	in
	  IntSyn.BDec (Some(name), b)
	end
      | decName' role (G, D as IntSyn.BDec (Some(name), b as (cid, t))) ->
	if varDefined name || conDefined name
	  || ctxDefined (G, name)
	  then IntSyn.BDec (Some (tryNextName (G, baseOf name)), b)
	else D
      | decName' role (G, IntSyn.ADec (None, d)) ->
        let
	  let name = findName (G, namePrefOf' (role, None), extent (role))
	in
	  IntSyn.ADec (Some(name), d)
	end
      | decName' role (G, D as IntSyn.ADec (Some(name), d)) ->
(*	IntSyn.ADec(Some(name), d) *)
	if varDefined name || conDefined name
	  || ctxDefined (G, name)
	  then IntSyn.ADec (Some (tryNextName (G, baseOf name)), d)
	else D
      | decName' role (G, D as IntSyn.NDec None) -> 
	let 
	  let name = findName (G, "@x", Local)
	    let _ = print name
	     
	in 
	  IntSyn.NDec (Some name)
	end
      | decName' role (G, D as IntSyn.NDec (Some name)) -> 
	if varDefined name || conDefined name
	  || ctxDefined (G, name)
	  then IntSyn.NDec (Some (tryNextName (G, baseOf name)))
	else D

    let decName = decName' Exist
    let decEName = decName' Exist
    let decUName = decName' (Univ (Global))
    let decLUName = decName' (Univ (Local))

    (* ctxName G = G'
       
        Invariant:
	|- G == G' ctx
	where some Declaration in G' have been named/renamed
    *)
    let ctxName (IntSyn.Null) -> IntSyn.Null
      | ctxName (IntSyn.Decl (G, D)) -> 
        let
	  let G' = ctxName G
	in
	  IntSyn.Decl (G', decName (G', D))
	end

    (* ctxLUName G = G'
       like ctxName, but names assigned are local universal names.
    *)
    let ctxLUName (IntSyn.Null) -> IntSyn.Null
      | ctxLUName (IntSyn.Decl (G, D)) -> 
        let
	  let G' = ctxLUName G
	in
	  IntSyn.Decl (G', decLUName (G', D))
	end

    (* pisEName' (G, i, V) = V'
       Assigns names to dependent Pi prefix of V with i implicit abstractions
       Used for implicit EVar in constant declarations after abstraction.
    *)
    let pisEName' (G, 0, V) -> V
      | pisEName' (G, i, IntSyn.Pi ((D, IntSyn.Maybe), V)) ->
        (* i > 0 *)
        let
	  let D' = decEName (G, D)
	in
	  IntSyn.Pi ((D', IntSyn.Maybe),
		     pisEName' (IntSyn.Decl (G, D'), i-1, V))
	end
      (* | pisEName' (G, i, V) = V *)

    let pisEName (i, V) = pisEName' (IntSyn.Null, i, V)

    (* defEName' (G, i, (U,V)) = (U',V')
       Invariant: G |- U : V  and G |- U' : V' since U == U' and V == V'.
       Assigns name to dependent Pi prefix of V and corresponding lam prefix of U
       with i implicit abstractions
       Used for implicit EVar in constant definitions after abstraction.
    *)
    let defEName' (G, 0, UV) -> UV
      | defEName' (G, i, (IntSyn.Lam (D, U), IntSyn.Pi ((_ (* = D *), P), V))) ->
        (* i > 0 *)
        let
	  let D' = decEName (G, D)
	  let (U', V') = defEName' (IntSyn.Decl (G, D'), i-1, (U, V))
	in
	  (IntSyn.Lam (D', U'), IntSyn.Pi ((D', P), V'))
	end
      (* | defEName' (G, i, (U, V)) = (U, V) *)

    let defEName (imp, UV) = defEName' (IntSyn.Null, imp, UV)

    let nameConDec' (IntSyn.ConDec (name, parent, imp, status, V, L)) ->
          IntSyn.ConDec (name, parent, imp, status, pisEName (imp, V), L)
      | nameConDec' (IntSyn.ConDef (name, parent, imp, U, V, L, Anc)) ->
	let 
	  let (U', V') = defEName (imp, (U, V))
	in
	  IntSyn.ConDef (name, parent, imp, U', V', L, Anc)
	end
      | nameConDec' (IntSyn.AbbrevDef (name, parent, imp, U, V, L)) ->
	let 
	  let (U', V') = defEName (imp, (U, V))
	in
	  IntSyn.AbbrevDef (name, parent, imp, U', V', L)
	end
      | nameConDec' (skodec) -> skodec (* fix ??? *)

    (* Assigns names to variables in a constant declaration *)
    (* The varReset (); is necessary so that explicitly named variables keep their name *)
    let nameConDec (conDec) =
        (varReset IntSyn.Null;			(* declaration is always closed *)
	 nameConDec' conDec)

    let skonstName (name) =
          tryNextName (IntSyn.Null, name)

    let namedEVars = namedEVars
    let evarCnstr = evarCnstr
