let abortOnError () =
  if !Errormsg.anyErrors then
    exit 1

let compileQuery query amod =
  let pterm = Compile.compileString query in
  if Option.isNone pterm then
    0
  else
    let pterm = Option.get pterm in
    let result = Parse.translateTermTopLevel pterm amod in
    if Option.isNone result then
      0
    else
      let (term, tymol, fvars, tyfvars) = Option.get result in
