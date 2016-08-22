let submit_query query metadata =
  let (term, tymol, fvars, tyfvars) = Translator.get_translation.translate_query query metadata in
  Ccode_stubs.setTypeAndTermLocation ();
  Readterm.readTermAndType term tymol fvars tyfvars;
  true

let solve_query = Query.solveQuery
