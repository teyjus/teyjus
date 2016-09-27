module IntMap = Map.Make(struct type t = int let compare = Pervasives.compare end)

let freeVarTab = ref IntMap.empty

(* initialize free var table with fvars *)
let freeVarTab_init fvars =
  let rec init_aux vars idx table =
    match vars with
        (v :: vars') ->
          init_aux vars' (idx + 1) (IntMap.add idx v table)
      | [] -> table
  in
  freeVarTab := init_aux fvars 0 IntMap.empty;
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
  freeVarTab_init fvars

let show_answers = Query.showAnswers
