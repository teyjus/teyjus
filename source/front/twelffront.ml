(** Frontend for the tjtwelf tool *)

(* the signature file to parse. Should become a parameter. *)
let filename = ref "arith.elf"

(* the Twelf context for parsing & type reconstruction *)
let context : Names.namespace option ref = ref None

(* Compile and link the module before loading *)
let compile_and_link () =
  Sys.command "./tjcc top";
  print_endline "compiled!";
  Sys.command "./tjlink top";
  print_endline "linked!"


(* Generate the sig and mod files, and the metadata file. *)
let output_files metadata signature modul =
  let (out_md, out_sig, out_mod) = (open_out "top.md", open_out "top.sig", open_out "top.mod") in
  Printf.fprintf out_md "%s\n" metadata; 
  Printf.fprintf out_sig "%s\n" signature;
  Printf.fprintf out_mod "%s\n" modul;
  close_out out_md;
  close_out out_sig;
  close_out out_mod        

(* Generate the sig and mod file contents *)
let string_of_sig kinds constants terms =
  let per_kind kind =
    let rec getKindType n =
      match n with
          0 -> "type"
        | _ -> "type -> " ^ getKindType (n-1) 
    in
    "kind " ^ (Absyn.string_of_kind kind) ^ " " ^ (getKindType (Absyn.getKindArity kind)) ^"."
  in
  let per_const constant =
    "type " ^ (Absyn.getConstantPrintName constant) ^ " " ^(Absyn.string_of_skeleton (Absyn.getConstantSkeletonValue constant)) ^ "."
  in
  let per_term term =
    (Absyn.string_of_term term) ^ "."
  in
  let sigstr = Table.fold (fun s c b -> b ^ (per_const c) ^ "\n") 
                          constants 
                          ((Table.fold (fun s k b -> b ^ (per_kind k) ^ "\n") 
                                       kinds 
                                       "sig top.\n%%%kind decls\n") ^ "%%%const decls\n")
  in
  let modstr = List.fold_left (fun s t -> s ^ (per_term t) ^ "\n")
                              "module top.\n"
                              terms
                              
  in (sigstr, modstr)

(* parse the implicit LF (Twelf-style) signature *)

let spine_map f s =
  let rec aux s =
    match s with
        IntSyn.Nil -> []
      | IntSyn.App(e, s') -> 
        let e' = f e in
        (e' :: aux s')
      | IntSyn.SClo(s',IntSyn.Shift(0)) -> aux s'
  in
  aux s 

    (* moving from Twelf structures to my structures *)
let rec exp_to_kind bvars e =
  match e with
      IntSyn.Uni (IntSyn.Type) -> Lfabsyn.Type
    | IntSyn.Pi ((IntSyn.Dec(name_op,ty),_), body) ->
        let t = exp_to_type bvars ty in
        if Option.isSome name_op
        then
          let b = exp_to_kind ((Option.get name_op,t) :: bvars) body in
          Lfabsyn.PiKind(Lfabsyn.Var(Option.get name_op, t), t, b)
        else
          (* need to generate a name *)
          let name = Names.skonstName "A" in
          let b = exp_to_kind ((name,t) :: bvars) body in
          Lfabsyn.PiKind(Lfabsyn.Var(name,t), t, b)
    | _ ->
        Errormsg.error Errormsg.none ("exp_to_kind: This expression is not a valid kind.");
        (* trying to continue on errors *)
        Lfabsyn.Type

and exp_to_type bvars e =
  match e with
      IntSyn.Pi ((IntSyn.Dec(name_op,ty),_), body) ->
        let t = exp_to_type bvars ty in
        if Option.isSome name_op
        then
          let b = exp_to_type ((Option.get name_op,t)::bvars) body in
          Lfabsyn.PiType(Lfabsyn.Var (Option.get name_op, t), t, b)
        else
          (* need to generate a name *)
          let name = Names.skonstName "A" in
          let b = exp_to_type ((name,t)::bvars) body in
          Lfabsyn.PiType(Lfabsyn.Var(name,t), t, b)
    | IntSyn.Root(IntSyn.Const(cid),IntSyn.Nil) ->
        let Names.Qid(_,name) = Names.constQid(cid) in
        Lfabsyn.IdType(Lfabsyn.Const(name))
    | IntSyn.Root(IntSyn.Const(cid), spine) ->
        let Names.Qid(_,name) = Names.constQid(cid) in
        let args = spine_map (exp_to_term bvars) spine in
        Lfabsyn.AppType(Lfabsyn.Const(name),args)
    | IntSyn.EClo(e', IntSyn.Shift(0)) -> exp_to_type bvars e'
    | IntSyn.EClo (e',sub) -> 
        let e'' = exp_to_type bvars e' in
        (match (e'', sub) with
             (_, IntSyn.Shift(0)) -> e''
           | (Lfabsyn.IdType(id), _) ->
               Lfabsyn.AppType(id, List.rev (sub_to_args bvars sub)) )
    | _ ->
        Errormsg.error Errormsg.none ("exp_to_type: This expression type: `"^(IntSyn.exp_to_string e)^"' is not a valid type.");
        (* try to continue with dummy type? *)
        Lfabsyn.IdType(Lfabsyn.Const("dummy"))


and exp_to_term bvars e =
  match e with
      IntSyn.Lam(IntSyn.Dec(name_op,ty), body) ->
        let t = exp_to_type bvars ty in
        let name =
          (match name_op with
             Some(name) ->
               name
           | None ->
               (* get a new name *)
               Names.skonstName "x")
        in
        let b = exp_to_term ((name,t) :: bvars) body in
        Lfabsyn.AbsTerm(Lfabsyn.Var(name,t), t, b)
    | IntSyn.Root(h,IntSyn.Nil) ->
        (match h with
             IntSyn.Const(cid) ->
               let Names.Qid(_,name) = Names.constQid(cid) in
               Lfabsyn.IdTerm(Lfabsyn.Const(name))
           | IntSyn.BVar(i) ->
               let (name, t) = List.nth bvars (i-1) in
               Lfabsyn.IdTerm(Lfabsyn.Var(name,t))
(* there shouldn't be any free variables after processing
           | IntSyn.FVar(name,ty,_) ->
               let t = exp_to_type bvars ty in
               Lfabsyn.IdTerm(Lfabsyn.LogicVar(name, t))
*)
           | _ ->
               Errormsg.error Errormsg.none ("exp_to_term: This head has an unexpected form.");
               (* try to continue with dummy term? *)
               Lfabsyn.IdTerm(Lfabsyn.Const("dummy")))
    | IntSyn.Root(h,spine) ->
        let args = spine_map (exp_to_term bvars) spine in
        (match h with
             IntSyn.Const(cid) ->
               let Names.Qid(_,name) = Names.constQid(cid) in
               Lfabsyn.AppTerm(Lfabsyn.Const(name), args)
           | IntSyn.BVar(i) ->
               let (name, t) = List.nth bvars (i-1) in
               Lfabsyn.AppTerm(Lfabsyn.Var(name,t), args)
(* there shouldn't be any free variables after processing
           | IntSyn.FVar(name,ty,_) ->
               let t = exp_to_type bvars ty in
               Lfabsyn.AppTerm(Lfabsyn.LogicVar(name,t), args)
*)
           | _ ->
               Errormsg.error Errormsg.none ("exp_to_term: This head has an unexpected form.");
               (* try to continue with dummy term? *)
               Lfabsyn.IdTerm(Lfabsyn.Const("dummy")))
    | IntSyn.EVar(r,IntSyn.Null,ty,c) when  !c = [] ->
(*        let _ = print_endline ("See EVar: "^(IntSyn.exp_to_string e)) in *)
        if Option.isSome (!r)
        then
          exp_to_term bvars (Option.get (!r))
        else
          let t = exp_to_type bvars ty in
          let name = Names.evarName (IntSyn.Null, e) in 
          Lfabsyn.IdTerm(Lfabsyn.LogicVar(name, t))
    | IntSyn.EVar(r,dctx,ty,c) when !c = [] ->
        if Option.isSome(!r)
        then
          exp_to_term bvars (Option.get (!r))
        else
          let name = Names.evarName (IntSyn.Null, e) in
          let ty_head = exp_to_type bvars ty in
          let ty = build_type_from_dctx bvars ty_head dctx in
          Lfabsyn.IdTerm(Lfabsyn.LogicVar(name, ty))
    | IntSyn.EClo(e', IntSyn.Shift(0)) -> exp_to_term bvars e'
    | IntSyn.EClo (e',sub) -> 
        let e'' = exp_to_term bvars e' in
        (match (e'', sub) with
             (_, IntSyn.Shift(0)) -> e''
           | (Lfabsyn.IdTerm(id), _) ->
               Lfabsyn.AppTerm(id, List.rev (sub_to_args bvars sub)) )
    | _ ->
        Errormsg.error Errormsg.none ("exp_to_term: This expression: `"^(IntSyn.exp_to_string e)^"' is not a valid term.");
        (* try to continue with dummy term? *)
        Lfabsyn.IdTerm(Lfabsyn.Const("dummy"))   
and sub_to_args bvars sub =
  match sub with
      IntSyn.Dot(IntSyn.Idx(k), sub') -> 
        let (name, ty) = List.nth bvars (k-1) in
        (Lfabsyn.IdTerm(Lfabsyn.Var(name, ty)) :: (sub_to_args bvars sub'))
    | IntSyn.Dot(IntSyn.Exp(e), sub') -> (exp_to_term bvars e) :: (sub_to_args bvars sub')
    | IntSyn.Shift(k) -> []
and build_type_from_dctx bvars ty ctx =
  match ctx with
      IntSyn.Null -> ty
    | IntSyn.Decl(ctx', IntSyn.Dec(None,e)) ->
        let t = exp_to_type bvars e in
        build_type_from_dctx bvars (Lfabsyn.ImpType(t, ty)) ctx'
    | IntSyn.Decl(ctx', IntSyn.Dec(Some(name),e)) ->
        let t = exp_to_type bvars e in
        build_type_from_dctx bvars (Lfabsyn.PiType(Lfabsyn.Var(name, t), t, ty)) ctx'

let conDec_to_typeFam (IntSyn.ConDec(name, id, implicit, _, kind, _)) =
  let k = exp_to_kind [] kind in 
  Lfabsyn.TypeFam(Lfabsyn.Const(name), k, Lfabsyn.NoFixity, Lfabsyn.None, 0, ref [], 0)

let conDec_to_obj (IntSyn.ConDec(name, id, implicit, _, ty, _)) =
  let typ = exp_to_type [] ty in
  let Lfabsyn.Const(tyhead) = Lfabsyn.get_typ_head typ in
  (Lfabsyn.Object(Lfabsyn.Const(name), typ, Lfabsyn.NoFixity, Lfabsyn.None, 0, 0), tyhead)

let query_to_query (queryty, name_op, evars) =
  let pt = match name_op with Some(n) -> n | None -> "" in
  let f l (IntSyn.EVar(e,ctx,ty,c),name) = 
    let ty_head = exp_to_type [] ty in
    let t = 
      match ctx with
          IntSyn.Null -> ty_head
        | _ -> build_type_from_dctx [] ty_head ctx
    in
    (Lfabsyn.LogicVar(name,t), t) :: l
  in
  let qty = exp_to_type [] queryty in
  let fvars = List.fold_left f [] evars in
  Lfabsyn.Query(fvars, Lfabsyn.LogicVar(pt, qty), qty)



let parse_sig filename =
  try
    let inchann = open_in filename in
    let parseStream = Parser.parseStream inchann in
    let _ = context := Some(Names.newNamespace ()) in
    let readDec stream (Lfsig.Signature(name, decls)) =
      let rec aux stream decls =
        match Tparsing.Parsing.Lexer'.Stream'.expose stream with
  	    Tparsing.Parsing.Lexer'.Stream'.Empty -> decls
          | Tparsing.Parsing.Lexer'.Stream'.Cons((Parser.ConDec(condec), r), stream') ->
	     let (conDec_op, occTree_op) = ReconCondec.condecToConDec (condec, Paths.Loc("test", r), false) in
             (match conDec_op with
                  Some(conDec) ->
                    let cid = IntSyn.sgnAdd conDec in
                    let _ = 
                      try
                        match !context with
                            Some(namespace) -> Names.insertConst (namespace,cid)
                          | None -> ()
                      with Names.Error msg -> raise (Names.Error (Paths.wrap (r, msg)))
                    in
                    let _ = Names.installConstName cid in
                    (match IntSyn.conDecUni conDec with
                         IntSyn.Kind ->
                           let typefam = conDec_to_typeFam conDec in
                           let decls' = Symboltable.insert decls (Symb.symbol (Lfabsyn.get_typefam_name typefam)) typefam in
                           aux stream' decls'
                       | IntSyn.Type ->
                           let (obj, target) = conDec_to_obj conDec in
                           match Symboltable.lookup decls (Symb.symbol target) with
                               None -> Errormsg.error Errormsg.none ("Type constructor "^target^" not found in signature.");
                                     (* try to continue if error *)
                                     aux stream' decls
                             | Some(Lfabsyn.TypeFam(a,b,c,d,e,objs,f)) -> 
                                 let _ = objs := (List.append !objs [ref obj]) in
                                 aux stream' decls)
                | None ->  
                    aux stream' decls )
      in
      Lfsig.Signature(name, aux stream decls)
    in
    let lfsig = readDec parseStream (Lfsig.Signature(ref ["top"], Symboltable.empty)) in
    close_in inchann; Some(lfsig)
  with
    Failure(s) -> (print_endline ("Error: " ^ s ^ "."); None)

(* parse the implicit LF (Twelf-style) query *)
let parse_query () =
  let parseStream = Parser.parseTerminalQ((*"["^"top"^"] ?- "*)"", "") in
  match Tparsing.Parsing.Lexer'.Stream'.expose parseStream with
      Tparsing.Parsing.Lexer'.Stream'.Cons(query, parseStream') ->
        let (ty, name_op, evars) = ReconQuery.queryToQuery(query, Paths.Loc ("stdIn", Paths.Reg(0,0))) in
        let query = query_to_query (ty, name_op, evars) in
        Some(query)
    | Tparsing.Parsing.Lexer'.Stream'.Empty -> None

(** main *)
let _ = 
  let _ = print_string "tjtwelf started!\n" in
  (* parse LF signature *)
  let res = parse_sig (!filename) in
  let sign =
    (match res with
         Some(s) -> (*print_string (Lfsig.string_of_sig s); *)
                    s
       | None -> print_string "failed to parse.\n";
                 exit 1) in

  (* translate LF sig and generate files. *)
  let _ = Translator.set_translation "optimized";
          Optimization.Swap.set true;
          Optimization.Specialize.set true in
  let (metadata, kinds, constants, terms) = 
    match Translator.get_translation () with
        "optimized" -> Translator.OptimizedTranslation.translate sign 
      | "naive" -> Translator.NaiveTranslation.translate sign
  in
  let (sigstr, modstr) = string_of_sig kinds constants terms in
  let metadatastr = Metadata.string_of_metadata metadata in
  let _ = output_files metadatastr sigstr modstr in

  (* compile and link LP module, load query state *)
  let _ = compile_and_link () in
  let (currmod, md) = Loader.load "top" in
 
  (* Query solving interaction loop
     (based on funciton from simulatorfront)
     finds a solution for query and prints out. *)
  let rec solveQueryInteract () =
    let rec moreAnswers () =
      print_string "\nMore solutions (y/n)? ";
      match read_line () with
        | "y" -> true
        | "n" -> false
        | _ ->
            print_endline ("\nSorry, only options are `y' or `n'.\n" ^ 
                           "Let's try it again:");
            moreAnswers ()
    in
    if (Query.solveQuery ()) then
      if (Query.queryHasVars ()) then
        (Lfquery.show_answers currmod sign md; 
(*        (Query.showAnswers ();*)
         if (moreAnswers ()) then
           solveQueryInteract ()
         else
           print_endline "\nyes\n")
      else
        print_endline "\nyes\n"
    else
      print_endline "\nno (more) solutions\n"
  in
  let solveQuery query =
    (match query with 
         Some(lfquery) -> 
           print_endline ("LF query: " ^ Lfabsyn.string_of_query' lfquery);
           if Lfquery.submit_query lfquery md (Absyn.getModuleKindTable currmod) (Absyn.getModuleConstantTable currmod) then 
             solveQueryInteract ()
           else
             prerr_endline ""
       | None -> print_string "failed to parse query.\n");
    Module.cleanModule (); 
    Front.simulatorReInit false ;
    Module.initModuleContext ()  
  in
  (* enter interactive mode *)
  while true do 
(*    let _ = print_string ("[" ^ "top" ^"] ?- ") in *)
    solveQuery (parse_query ()) 
  done
