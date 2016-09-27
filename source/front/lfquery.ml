
(** datastructures needed for inverting solutions:
      1. fvartypes
         a symbol table mapping free variables to their LF type
      2. freeVarTab
         a table with the type symbol for each translated free variable
 *)
let fvar_types = ref Table.empty

let freeVarTab = ref Intmap.empty

let fvartypes_init (Lfabsyn.Query(fvars, pt, ty)) =
  let types = List.fold_left (fun tbl (id,t) -> Table.add (Symbol.symbol (Lfabsyn.get_id_name id)) t tbl) 
                             Table.empty 
                             ((pt, ty) :: fvars) in
  fvar_types := types

(* initialize free var table with fvars *)
let freeVarTab_init fvars =
  let rec init_aux vars idx table =
    match vars with
        (v :: vars') ->
          init_aux vars' (idx + 1) (Intmap.add idx v table)
      | [] -> table
  in
  freeVarTab := init_aux fvars 0 Intmap.empty;
  true

let submit_query query metadata kinds constants =
  let (term, fvars) = 
    match Translator.get_translation () with
        "naive" -> Translator.NaiveTranslation.translate_query query metadata kinds constants
      | "optimized" -> Translator.OptimizedTranslation.translate_query query metadata kinds constants in
  let (term',_,_) = Parse.fixTerm (Parse.removeNestedAbstractions term) in
  let _ = print_endline ("translated query: "^(Absyn.string_of_term term')) in
  Ccode_stubs.setTypeAndTermLocation (); 
  Readterm.readTermAndType term' (Types.Molecule(Absyn.ApplicationType(Pervasive.kbool,[]),[])) fvars [];
  fvartypes_init query;
  freeVarTab_init fvars

let show_answers lpmodule lfsig metadata = 
  let lpsol = Buildterm.build_solution lpmodule (!freeVarTab) in 
  let lfsol = Inverse.invert lfsig metadata (!fvar_types) lpsol in
  print_endline (Lfabsyn.string_of_solution lfsol)
