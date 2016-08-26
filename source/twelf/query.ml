let submit_query query metadata kinds constants =
  let (term, fvars) = Translator.get_translation.translate_query query metadata in
  Ccode_stubs.setTypeAndTermLocation ();
  Readterm.readTermAndType term (Types.Molecule(Absyn.ApplicationType(Pervasive.kbool,[]),[])) fvars [];
  true

let solve_query = Query.solveQuery

let show_answers = Query.showAnswers
