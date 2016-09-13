let submit_query query metadata kinds constants =
  let (term, fvars) = 
    match Translator.get_translation () with
        "naive" -> Translator.NaiveTranslation.translate_query query metadata kinds constants
      | "optimized" -> Translator.OptimizedTranslation.translate_query query metadata kinds constants in
  let (term',_,_) = Parse.fixTerm (Parse.removeNestedAbstractions term) in
  let _ = print_endline ("translated query: "^(Absyn.string_of_term term')) in
  Ccode_stubs.setTypeAndTermLocation (); 
  Readterm.readTermAndType term' (Types.Molecule(Absyn.ApplicationType(Pervasive.kbool,[]),[])) fvars [];
  true

let solve_query = Query.solveQuery

let show_answers = Query.showAnswers
