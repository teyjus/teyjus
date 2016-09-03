let submit_query query metadata kinds constants =
  let (term, fvars) = 
    match Translator.get_translation () with
        "naive" -> Translator.NaiveTranslation.translate_query query metadata kinds constants
      | "optimized" -> Translator.OptimizedTranslation.translate_query query metadata kinds constants in
(*  Ccode_stubs.setTypeAndTermLocation (); *)
  Readterm.readTermAndType term (Types.Molecule(Absyn.ApplicationType(Pervasive.kbool,[]),[])) fvars [];
  true

let solve_query = Query.solveQuery

let show_answers = Query.showAnswers
