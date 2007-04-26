let kint = Absyn.PervasiveKind((Symbol.symbol "int"), (Some 0), (ref 0), Errormsg.none)

let kreal = Absyn.PervasiveKind((Symbol.symbol "real"), (Some 0), (ref 1), Errormsg.none)

let kbool = Absyn.PervasiveKind((Symbol.symbol "o"), (Some 0), (ref 2), Errormsg.none)

let kstring = Absyn.PervasiveKind((Symbol.symbol "string"), (Some 0), (ref 3), Errormsg.none)

let klist = Absyn.PervasiveKind((Symbol.symbol "list"), (Some 1), (ref 4), Errormsg.none)

let kinstream = Absyn.PervasiveKind((Symbol.symbol "in_stream"), (Some 0), (ref 5), Errormsg.none)

let koutstream = Absyn.PervasiveKind((Symbol.symbol "out_stream"), (Some 0), (ref 6), Errormsg.none)


let buildPervasiveKinds = function () ->
  let t = Table.SymbolTable.empty in
  let t = Table.add (Symbol.symbol "int") kint t in
  let t = Table.add (Symbol.symbol "real") kreal t in
  let t = Table.add (Symbol.symbol "o") kbool t in
  let t = Table.add (Symbol.symbol "string") kstring t in
  let t = Table.add (Symbol.symbol "list") klist t in
  let t = Table.add (Symbol.symbol "in_stream") kinstream t in
  let t = Table.add (Symbol.symbol "out_stream") koutstream t in
  t


let iskint tm = (tm == kint)

let iskreal tm = (tm == kreal)

let iskbool tm = (tm == kbool)

let iskstring tm = (tm == kstring)

let isklist tm = (tm == klist)

let iskinstream tm = (tm == kinstream)

let iskoutstream tm = (tm == koutstream)



let tyskel0 = (Some (Absyn.Skeleton(Absyn.SkeletonVarType((ref 0)), (ref None), (ref false))))

let tyskel1 = (Some (Absyn.Skeleton(Absyn.ApplicationType(klist, (Absyn.SkeletonVarType((ref 0)) :: [])), (ref None), (ref false))))

let tyskel2 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.SkeletonVarType((ref 0)), Absyn.ArrowType(Absyn.ApplicationType(klist, (Absyn.SkeletonVarType((ref 0)) :: [])), Absyn.ApplicationType(klist, (Absyn.SkeletonVarType((ref 0)) :: [])))), (ref None), (ref false))))

let tyskel3 = (Some (Absyn.Skeleton(Absyn.ApplicationType(kint, []), (ref None), (ref false))))

let tyskel4 = (Some (Absyn.Skeleton(Absyn.ApplicationType(kreal, []), (ref None), (ref false))))

let tyskel5 = (Some (Absyn.Skeleton(Absyn.ApplicationType(kstring, []), (ref None), (ref false))))

let tyskel6 = (Some (Absyn.Skeleton(Absyn.ApplicationType(kbool, []), (ref None), (ref false))))

let tyskel7 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ApplicationType(kint, [])), (ref None), (ref false))))

let tyskel8 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ApplicationType(kint, []))), (ref None), (ref false))))

let tyskel9 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel10 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ApplicationType(kreal, [])), (ref None), (ref false))))

let tyskel11 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kreal, []), Absyn.ApplicationType(kint, [])), (ref None), (ref false))))

let tyskel12 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kreal, []), Absyn.ApplicationType(kreal, [])), (ref None), (ref false))))

let tyskel13 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kreal, []), Absyn.ApplicationType(kstring, [])), (ref None), (ref false))))

let tyskel14 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kreal, []), Absyn.ArrowType(Absyn.ApplicationType(kreal, []), Absyn.ApplicationType(kreal, []))), (ref None), (ref false))))

let tyskel15 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kreal, []), Absyn.ArrowType(Absyn.ApplicationType(kreal, []), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel16 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ApplicationType(kint, [])), (ref None), (ref false))))

let tyskel17 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ApplicationType(kstring, [])), (ref None), (ref false))))

let tyskel18 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ApplicationType(kstring, []))), (ref None), (ref false))))

let tyskel19 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel20 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ApplicationType(kstring, [])))), (ref None), (ref false))))

let tyskel21 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kbool, []), Absyn.ArrowType(Absyn.ApplicationType(kbool, []), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel22 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ArrowType(Absyn.SkeletonVarType((ref 0)), Absyn.ApplicationType(kbool, [])), Absyn.ApplicationType(kbool, [])), (ref None), (ref false))))

let tyskel23 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.SkeletonVarType((ref 0)), Absyn.ArrowType(Absyn.SkeletonVarType((ref 0)), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel24 = (Some (Absyn.Skeleton(Absyn.ApplicationType(kinstream, []), (ref None), (ref false))))

let tyskel25 = (Some (Absyn.Skeleton(Absyn.ApplicationType(koutstream, []), (ref None), (ref false))))

let tyskel26 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ArrowType(Absyn.ApplicationType(kinstream, []), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel27 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ArrowType(Absyn.ApplicationType(koutstream, []), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel28 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kinstream, []), Absyn.ApplicationType(kbool, [])), (ref None), (ref false))))

let tyskel29 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(koutstream, []), Absyn.ApplicationType(kbool, [])), (ref None), (ref false))))

let tyskel30 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.SkeletonVarType((ref 0)), Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel31 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ArrowType(Absyn.SkeletonVarType((ref 0)), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel32 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(koutstream, []), Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel33 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kinstream, []), Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ApplicationType(kbool, [])))), (ref None), (ref false))))

let tyskel34 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kinstream, []), Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel35 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ApplicationType(kbool, [])), (ref None), (ref false))))

let tyskel36 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.SkeletonVarType((ref 0)), Absyn.ApplicationType(kbool, [])), (ref None), (ref false))))

let tyskel37 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(koutstream, []), Absyn.ArrowType(Absyn.SkeletonVarType((ref 0)), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel38 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kinstream, []), Absyn.ArrowType(Absyn.SkeletonVarType((ref 0)), Absyn.ApplicationType(kbool, []))), (ref None), (ref false))))

let tyskel39 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kbool, []), Absyn.ApplicationType(kbool, [])), (ref None), (ref false))))

let tyskel40 = (Some (Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(kstring, []), Absyn.ArrowType(Absyn.ApplicationType(kint, []), Absyn.ArrowType(Absyn.ApplicationType(kinstream, []), Absyn.ArrowType(Absyn.ApplicationType(koutstream, []), Absyn.ApplicationType(kbool, []))))), (ref None), (ref false))))

let tysetvarIR = Absyn.makeTypeSetVariable (Absyn.ApplicationType(kint,[])) (Absyn.ApplicationType(kint,[]) :: Absyn.ApplicationType(kreal,[]) :: [])
let overloadTySkel1 = (ref (Some (Absyn.Skeleton(Absyn.ArrowType(tysetvarIR, tysetvarIR), (ref None), (ref false)))))
let overloadTySkel2 = (ref (Some (Absyn.Skeleton(Absyn.ArrowType(tysetvarIR, Absyn.ArrowType(tysetvarIR, tysetvarIR)), (ref None), (ref false)))))

let tysetvarIRS = Absyn.makeTypeSetVariable (Absyn.ApplicationType(kint,[])) (Absyn.ApplicationType(kint,[]) :: Absyn.ApplicationType(kreal,[]) :: Absyn.ApplicationType(kstring, []) :: [])
let overloadTySkel3 = (ref (Some (Absyn.Skeleton(Absyn.ArrowType(tysetvarIRS, Absyn.ArrowType(tysetvarIRS, Absyn.ApplicationType(kbool, []))), (ref None), (ref false)))))



let univConstant = Absyn.Constant((Symbol.symbol "<constant>"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel0), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 85), Errormsg.none)

let nilConstant = Absyn.Constant((Symbol.symbol "nil"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel1), (ref 1), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 89), Errormsg.none)

let consConstant = Absyn.Constant((Symbol.symbol "::"), (ref Absyn.Infixr), (ref 140), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel2), (ref 1), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 93), Errormsg.none)

let intcConstant = Absyn.Constant((Symbol.symbol "<int_constant>"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel3), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 90), Errormsg.none)

let realcConstant = Absyn.Constant((Symbol.symbol "<real_constant>"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel4), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 91), Errormsg.none)

let strcConstant = Absyn.Constant((Symbol.symbol "<str_constant>"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel5), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 92), Errormsg.none)

let trueConstant = Absyn.Constant((Symbol.symbol "true"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel6), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 4), Errormsg.none)

let cutConstant = Absyn.Constant((Symbol.symbol "!"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel6), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 5), Errormsg.none)

let failConstant = Absyn.Constant((Symbol.symbol "fail"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel6), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 6), Errormsg.none)

let haltConstant = Absyn.Constant((Symbol.symbol "halt"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel6), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 9), Errormsg.none)

let stopConstant = Absyn.Constant((Symbol.symbol "stop"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel6), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 10), Errormsg.none)

let intuminusConstant = Absyn.Constant((Symbol.symbol "%i~"), (ref Absyn.Prefix), (ref (Absyn.maxPrec + 1)), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel7), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 56), Errormsg.none)

let modConstant = Absyn.Constant((Symbol.symbol "mod"), (ref Absyn.Infixl), (ref 160), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel7), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 61), Errormsg.none)

let iabsConstant = Absyn.Constant((Symbol.symbol "%iabs"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel7), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 63), Errormsg.none)

let intplusConstant = Absyn.Constant((Symbol.symbol "%i+"), (ref Absyn.Infixl), (ref 150), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel8), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 57), Errormsg.none)

let intminusConstant = Absyn.Constant((Symbol.symbol "%i-"), (ref Absyn.Infixl), (ref 150), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel8), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 58), Errormsg.none)

let intmultConstant = Absyn.Constant((Symbol.symbol "%i*"), (ref Absyn.Infixl), (ref 160), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel8), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 59), Errormsg.none)

let intdivConstant = Absyn.Constant((Symbol.symbol "div"), (ref Absyn.Infixl), (ref 160), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel8), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 60), Errormsg.none)

let intlssConstant = Absyn.Constant((Symbol.symbol "%i<"), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel9), (ref 0), (ref None), (ref (Some (Absyn.Builtin(4)))), (ref (Absyn.PervasiveConstant(false))), (ref 19), Errormsg.none)

let intgrtConstant = Absyn.Constant((Symbol.symbol "%i>"), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel9), (ref 0), (ref None), (ref (Some (Absyn.Builtin(5)))), (ref (Absyn.PervasiveConstant(false))), (ref 20), Errormsg.none)

let intleqConstant = Absyn.Constant((Symbol.symbol "%i<="), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel9), (ref 0), (ref None), (ref (Some (Absyn.Builtin(6)))), (ref (Absyn.PervasiveConstant(false))), (ref 21), Errormsg.none)

let intgeqConstant = Absyn.Constant((Symbol.symbol "%i>="), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel9), (ref 0), (ref None), (ref (Some (Absyn.Builtin(7)))), (ref (Absyn.PervasiveConstant(false))), (ref 22), Errormsg.none)

let timeConstant = Absyn.Constant((Symbol.symbol "time"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel9), (ref 0), (ref None), (ref (Some (Absyn.Builtin(36)))), (ref (Absyn.PervasiveConstant(true))), (ref 51), Errormsg.none)

let itorConstant = Absyn.Constant((Symbol.symbol "int_to_real"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel10), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 62), Errormsg.none)

let floorConstant = Absyn.Constant((Symbol.symbol "floor"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel11), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 74), Errormsg.none)

let ceilConstant = Absyn.Constant((Symbol.symbol "ceil"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel11), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 75), Errormsg.none)

let truncConstant = Absyn.Constant((Symbol.symbol "truncate"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel11), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 76), Errormsg.none)

let realuminusConstant = Absyn.Constant((Symbol.symbol "%r~"), (ref Absyn.Prefix), (ref (Absyn.maxPrec + 1)), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel12), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 64), Errormsg.none)

let sqrtConstant = Absyn.Constant((Symbol.symbol "sqrt"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel12), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 69), Errormsg.none)

let sinConstant = Absyn.Constant((Symbol.symbol "sin"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel12), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 70), Errormsg.none)

let cosConstant = Absyn.Constant((Symbol.symbol "cos"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel12), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 71), Errormsg.none)

let arctanConstant = Absyn.Constant((Symbol.symbol "arctan"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel12), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 72), Errormsg.none)

let logConstant = Absyn.Constant((Symbol.symbol "ln"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel12), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 73), Errormsg.none)

let rabsConstant = Absyn.Constant((Symbol.symbol "%rabs"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel12), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 77), Errormsg.none)

let rtosConstant = Absyn.Constant((Symbol.symbol "real_to_string"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel13), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 84), Errormsg.none)

let realplusConstant = Absyn.Constant((Symbol.symbol "%r+"), (ref Absyn.Infixl), (ref 150), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel14), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 65), Errormsg.none)

let realminusConstant = Absyn.Constant((Symbol.symbol "%r-"), (ref Absyn.Infixl), (ref 150), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel14), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 66), Errormsg.none)

let realmultConstant = Absyn.Constant((Symbol.symbol "%r*"), (ref Absyn.Infixl), (ref 160), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel14), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 67), Errormsg.none)

let realdivConstant = Absyn.Constant((Symbol.symbol "/"), (ref Absyn.Infixl), (ref 160), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel14), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 68), Errormsg.none)

let reallssConstant = Absyn.Constant((Symbol.symbol "%r<"), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel15), (ref 0), (ref None), (ref (Some (Absyn.Builtin(8)))), (ref (Absyn.PervasiveConstant(false))), (ref 23), Errormsg.none)

let realgrtConstant = Absyn.Constant((Symbol.symbol "%r>"), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel15), (ref 0), (ref None), (ref (Some (Absyn.Builtin(9)))), (ref (Absyn.PervasiveConstant(false))), (ref 24), Errormsg.none)

let realleqConstant = Absyn.Constant((Symbol.symbol "%r<="), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel15), (ref 0), (ref None), (ref (Some (Absyn.Builtin(10)))), (ref (Absyn.PervasiveConstant(false))), (ref 25), Errormsg.none)

let realgeqConstant = Absyn.Constant((Symbol.symbol "%r>="), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel15), (ref 0), (ref None), (ref (Some (Absyn.Builtin(11)))), (ref (Absyn.PervasiveConstant(false))), (ref 26), Errormsg.none)

let slenConstant = Absyn.Constant((Symbol.symbol "size"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel16), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 79), Errormsg.none)

let stoiConstant = Absyn.Constant((Symbol.symbol "string_to_int"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel16), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 81), Errormsg.none)

let itochrConstant = Absyn.Constant((Symbol.symbol "chr"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel17), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 80), Errormsg.none)

let itostrConstant = Absyn.Constant((Symbol.symbol "int_to_string"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel17), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 83), Errormsg.none)

let scatConstant = Absyn.Constant((Symbol.symbol "^"), (ref Absyn.Infix), (ref 150), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel18), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 78), Errormsg.none)

let strlssConstant = Absyn.Constant((Symbol.symbol "%s<"), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel19), (ref 0), (ref None), (ref (Some (Absyn.Builtin(12)))), (ref (Absyn.PervasiveConstant(true))), (ref 27), Errormsg.none)

let strgrtConstant = Absyn.Constant((Symbol.symbol "%s>"), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel19), (ref 0), (ref None), (ref (Some (Absyn.Builtin(13)))), (ref (Absyn.PervasiveConstant(true))), (ref 28), Errormsg.none)

let strleqConstant = Absyn.Constant((Symbol.symbol "%s<="), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel19), (ref 0), (ref None), (ref (Some (Absyn.Builtin(14)))), (ref (Absyn.PervasiveConstant(true))), (ref 29), Errormsg.none)

let strgeqConstant = Absyn.Constant((Symbol.symbol "%s>="), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel19), (ref 0), (ref None), (ref (Some (Absyn.Builtin(15)))), (ref (Absyn.PervasiveConstant(true))), (ref 30), Errormsg.none)

let getenvConstant = Absyn.Constant((Symbol.symbol "getenv"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel19), (ref 0), (ref None), (ref (Some (Absyn.Builtin(34)))), (ref (Absyn.PervasiveConstant(true))), (ref 49), Errormsg.none)

let substrConstant = Absyn.Constant((Symbol.symbol "substring"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel20), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 82), Errormsg.none)

let andConstant = Absyn.Constant((Symbol.symbol ","), (ref Absyn.Infixl), (ref 110), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel21), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 0), Errormsg.none)

let orConstant = Absyn.Constant((Symbol.symbol ";"), (ref Absyn.Infixl), (ref 100), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel21), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 1), Errormsg.none)

let ampandConstant = Absyn.Constant((Symbol.symbol "&"), (ref Absyn.Infixr), (ref 120), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel21), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 8), Errormsg.none)

let colondashConstant = Absyn.Constant((Symbol.symbol ":-"), (ref Absyn.Infixl), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel21), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 11), Errormsg.none)

let implConstant = Absyn.Constant((Symbol.symbol "=>"), (ref Absyn.Infixr), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel21), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 12), Errormsg.none)

let someConstant = Absyn.Constant((Symbol.symbol "sigma"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel22), (ref 1), (ref (Some (Array.make 1 true))), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 2), Errormsg.none)

let allConstant = Absyn.Constant((Symbol.symbol "pi"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel22), (ref 1), (ref (Some (Array.make 1 true))), (ref None), (ref (Absyn.PervasiveConstant(false))), (ref 3), Errormsg.none)

let isConstant = Absyn.Constant((Symbol.symbol "is"), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel23), (ref 1), (ref (Some (Array.make 1 true))), (ref (Some (Absyn.Builtin(1)))), (ref (Absyn.PervasiveConstant(false))), (ref 16), Errormsg.none)

let eqConstant = Absyn.Constant((Symbol.symbol "="), (ref Absyn.Infix), (ref 130), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel23), (ref 1), (ref (Some (Array.make 1 true))), (ref (Some (Absyn.Builtin(3)))), (ref (Absyn.PervasiveConstant(false))), (ref 18), Errormsg.none)

let stdinConstant = Absyn.Constant((Symbol.symbol "std_in"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel24), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 86), Errormsg.none)

let stdoutConstant = Absyn.Constant((Symbol.symbol "std_out"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel25), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 87), Errormsg.none)

let stderrConstant = Absyn.Constant((Symbol.symbol "std_err"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel25), (ref 0), (ref None), (ref None), (ref (Absyn.PervasiveConstant(true))), (ref 88), Errormsg.none)

let openinConstant = Absyn.Constant((Symbol.symbol "open_in"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel26), (ref 0), (ref None), (ref (Some (Absyn.Builtin(16)))), (ref (Absyn.PervasiveConstant(true))), (ref 31), Errormsg.none)

let openstrConstant = Absyn.Constant((Symbol.symbol "open_string"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel26), (ref 0), (ref None), (ref (Some (Absyn.Builtin(21)))), (ref (Absyn.PervasiveConstant(true))), (ref 36), Errormsg.none)

let openoutConstant = Absyn.Constant((Symbol.symbol "open_out"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel27), (ref 0), (ref None), (ref (Some (Absyn.Builtin(17)))), (ref (Absyn.PervasiveConstant(true))), (ref 32), Errormsg.none)

let openappConstant = Absyn.Constant((Symbol.symbol "open_append"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel27), (ref 0), (ref None), (ref (Some (Absyn.Builtin(18)))), (ref (Absyn.PervasiveConstant(true))), (ref 33), Errormsg.none)

let closeinConstant = Absyn.Constant((Symbol.symbol "close_in"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel28), (ref 0), (ref None), (ref (Some (Absyn.Builtin(19)))), (ref (Absyn.PervasiveConstant(true))), (ref 34), Errormsg.none)

let eofConstant = Absyn.Constant((Symbol.symbol "eof"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel28), (ref 0), (ref None), (ref (Some (Absyn.Builtin(26)))), (ref (Absyn.PervasiveConstant(true))), (ref 41), Errormsg.none)

let closeoutConstant = Absyn.Constant((Symbol.symbol "close_out"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel29), (ref 0), (ref None), (ref (Some (Absyn.Builtin(20)))), (ref (Absyn.PervasiveConstant(true))), (ref 35), Errormsg.none)

let flushConstant = Absyn.Constant((Symbol.symbol "flush"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel29), (ref 0), (ref None), (ref (Some (Absyn.Builtin(27)))), (ref (Absyn.PervasiveConstant(true))), (ref 42), Errormsg.none)

let termtostrConstant = Absyn.Constant((Symbol.symbol "term_to_string"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel30), (ref 1), (ref (Some (Array.init 1 (fun x -> if x >= 0 then false else true)))), (ref (Some (Absyn.Builtin(31)))), (ref (Absyn.PervasiveConstant(true))), (ref 46), Errormsg.none)

let strtotermConstant = Absyn.Constant((Symbol.symbol "string_to_term"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel31), (ref 1), (ref (Some (Array.make 1 true))), (ref (Some (Absyn.Builtin(32)))), (ref (Absyn.PervasiveConstant(true))), (ref 47), Errormsg.none)

let outputConstant = Absyn.Constant((Symbol.symbol "output"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel32), (ref 0), (ref None), (ref (Some (Absyn.Builtin(23)))), (ref (Absyn.PervasiveConstant(true))), (ref 38), Errormsg.none)

let inputConstant = Absyn.Constant((Symbol.symbol "input"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel33), (ref 0), (ref None), (ref (Some (Absyn.Builtin(22)))), (ref (Absyn.PervasiveConstant(true))), (ref 37), Errormsg.none)

let inputlineConstant = Absyn.Constant((Symbol.symbol "input_line"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel34), (ref 0), (ref None), (ref (Some (Absyn.Builtin(24)))), (ref (Absyn.PervasiveConstant(true))), (ref 39), Errormsg.none)

let lookaheadConstant = Absyn.Constant((Symbol.symbol "look_ahead"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel34), (ref 0), (ref None), (ref (Some (Absyn.Builtin(25)))), (ref (Absyn.PervasiveConstant(true))), (ref 40), Errormsg.none)

let printConstant = Absyn.Constant((Symbol.symbol "print"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel35), (ref 0), (ref None), (ref (Some (Absyn.Builtin(28)))), (ref (Absyn.PervasiveConstant(true))), (ref 43), Errormsg.none)

let readConstant = Absyn.Constant((Symbol.symbol "read"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel36), (ref 1), (ref (Some (Array.make 1 true))), (ref (Some (Absyn.Builtin(29)))), (ref (Absyn.PervasiveConstant(true))), (ref 44), Errormsg.none)

let printtermConstant = Absyn.Constant((Symbol.symbol "printterm"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel37), (ref 1), (ref (Some (Array.init 1 (fun x -> if x >= 0 then false else true)))), (ref (Some (Absyn.Builtin(30)))), (ref (Absyn.PervasiveConstant(true))), (ref 45), Errormsg.none)

let readtermConstant = Absyn.Constant((Symbol.symbol "readterm"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel38), (ref 1), (ref (Some (Array.make 1 true))), (ref (Some (Absyn.Builtin(33)))), (ref (Absyn.PervasiveConstant(true))), (ref 48), Errormsg.none)

let solveConstant = Absyn.Constant((Symbol.symbol "solve"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel39), (ref 0), (ref None), (ref (Some (Absyn.Builtin(0)))), (ref (Absyn.PervasiveConstant(false))), (ref 15), Errormsg.none)

let notConstant = Absyn.Constant((Symbol.symbol "not"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref true), (ref false), (ref tyskel39), (ref 0), (ref None), (ref (Some (Absyn.Builtin(2)))), (ref (Absyn.PervasiveConstant(false))), (ref 17), Errormsg.none)

let opensocketConstant = Absyn.Constant((Symbol.symbol "open_socket"), (ref Absyn.NoFixity), (ref 0), (ref false), (ref false), (ref false), (ref false), (ref false), (ref false), (ref tyskel40), (ref 0), (ref None), (ref (Some (Absyn.Builtin(35)))), (ref (Absyn.PervasiveConstant(true))), (ref 50), Errormsg.none)

let genericApplyConstant = Absyn.Constant((Symbol.symbol " apply"), ref Absyn.Infixl, ref (Absyn.maxPrec + 2), (ref false), (ref false), (ref false), (ref false), ref false, (ref false), ref(Some(Absyn.Skeleton(Absyn.ErrorType, ref None, ref false))), ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)

let overloadUMinusConstant = Absyn.Constant((Symbol.symbol "~"), ref Absyn.Prefix, ref (Absyn.maxPrec + 1), (ref false), (ref false), (ref false), (ref false), ref true, (ref false), overloadTySkel1, ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)

let overloadAbsConstant = Absyn.Constant((Symbol.symbol "abs"), ref Absyn.NoFixity, ref 0, (ref false), (ref false), (ref false), (ref false), ref true, (ref false), overloadTySkel1, ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)

let overloadPlusConstant = Absyn.Constant((Symbol.symbol "+"), ref Absyn.Infixl, ref 150, (ref false), (ref false), (ref false), (ref false), ref true, (ref false), overloadTySkel2, ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)

let overloadMinusConstant = Absyn.Constant((Symbol.symbol "-"), ref Absyn.Infixl, ref 150, (ref false), (ref false), (ref false), (ref false), ref true, (ref false), overloadTySkel2, ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)

let overloadTimeConstant = Absyn.Constant((Symbol.symbol "*"), ref Absyn.Infixl, ref 160, (ref false), (ref false), (ref false), (ref false), ref true, (ref false), overloadTySkel2, ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)

let overloadLTConstant = Absyn.Constant((Symbol.symbol "<"), ref Absyn.Infix, ref 130, (ref false), (ref false), (ref false), (ref false), ref true, (ref false), overloadTySkel3, ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)

let overloadGTConstant = Absyn.Constant((Symbol.symbol ">"), ref Absyn.Infix, ref 130, (ref false), (ref false), (ref false), (ref false), ref true, (ref false), overloadTySkel3, ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)

let overloadLEConstant = Absyn.Constant((Symbol.symbol "<"), ref Absyn.Infix, ref 130, (ref false), (ref false), (ref false), (ref false), ref true, (ref false), overloadTySkel3, ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)

let overloadGEConstant = Absyn.Constant((Symbol.symbol ">="), ref Absyn.Infix, ref 130, (ref false), (ref false), (ref false), (ref false), ref true, (ref false), overloadTySkel3, ref 0, ref None, ref None, ref(Absyn.PervasiveConstant(false)), (ref 0), Errormsg.none)


let buildPervasiveConstants = function () ->
  let t = Table.SymbolTable.empty in
  let t = Table.add (Symbol.symbol "<constant>") univConstant t in
  let t = Table.add (Symbol.symbol "nil") nilConstant t in
  let t = Table.add (Symbol.symbol "::") consConstant t in
  let t = Table.add (Symbol.symbol "<int_constant>") intcConstant t in
  let t = Table.add (Symbol.symbol "<real_constant>") realcConstant t in
  let t = Table.add (Symbol.symbol "<str_constant>") strcConstant t in
  let t = Table.add (Symbol.symbol "true") trueConstant t in
  let t = Table.add (Symbol.symbol "!") cutConstant t in
  let t = Table.add (Symbol.symbol "fail") failConstant t in
  let t = Table.add (Symbol.symbol "halt") haltConstant t in
  let t = Table.add (Symbol.symbol "stop") stopConstant t in
  let t = Table.add (Symbol.symbol "%i~") intuminusConstant t in
  let t = Table.add (Symbol.symbol "mod") modConstant t in
  let t = Table.add (Symbol.symbol "%iabs") iabsConstant t in
  let t = Table.add (Symbol.symbol "%i+") intplusConstant t in
  let t = Table.add (Symbol.symbol "%i-") intminusConstant t in
  let t = Table.add (Symbol.symbol "%i*") intmultConstant t in
  let t = Table.add (Symbol.symbol "div") intdivConstant t in
  let t = Table.add (Symbol.symbol "%i<") intlssConstant t in
  let t = Table.add (Symbol.symbol "%i>") intgrtConstant t in
  let t = Table.add (Symbol.symbol "%i<=") intleqConstant t in
  let t = Table.add (Symbol.symbol "%i>=") intgeqConstant t in
  let t = Table.add (Symbol.symbol "time") timeConstant t in
  let t = Table.add (Symbol.symbol "int_to_real") itorConstant t in
  let t = Table.add (Symbol.symbol "floor") floorConstant t in
  let t = Table.add (Symbol.symbol "ceil") ceilConstant t in
  let t = Table.add (Symbol.symbol "truncate") truncConstant t in
  let t = Table.add (Symbol.symbol "%r~") realuminusConstant t in
  let t = Table.add (Symbol.symbol "sqrt") sqrtConstant t in
  let t = Table.add (Symbol.symbol "sin") sinConstant t in
  let t = Table.add (Symbol.symbol "cos") cosConstant t in
  let t = Table.add (Symbol.symbol "arctan") arctanConstant t in
  let t = Table.add (Symbol.symbol "ln") logConstant t in
  let t = Table.add (Symbol.symbol "%rabs") rabsConstant t in
  let t = Table.add (Symbol.symbol "real_to_string") rtosConstant t in
  let t = Table.add (Symbol.symbol "%r+") realplusConstant t in
  let t = Table.add (Symbol.symbol "%r-") realminusConstant t in
  let t = Table.add (Symbol.symbol "%r*") realmultConstant t in
  let t = Table.add (Symbol.symbol "/") realdivConstant t in
  let t = Table.add (Symbol.symbol "%r<") reallssConstant t in
  let t = Table.add (Symbol.symbol "%r>") realgrtConstant t in
  let t = Table.add (Symbol.symbol "%r<=") realleqConstant t in
  let t = Table.add (Symbol.symbol "%r>=") realgeqConstant t in
  let t = Table.add (Symbol.symbol "size") slenConstant t in
  let t = Table.add (Symbol.symbol "string_to_int") stoiConstant t in
  let t = Table.add (Symbol.symbol "chr") itochrConstant t in
  let t = Table.add (Symbol.symbol "int_to_string") itostrConstant t in
  let t = Table.add (Symbol.symbol "^") scatConstant t in
  let t = Table.add (Symbol.symbol "%s<") strlssConstant t in
  let t = Table.add (Symbol.symbol "%s>") strgrtConstant t in
  let t = Table.add (Symbol.symbol "%s<=") strleqConstant t in
  let t = Table.add (Symbol.symbol "%s>=") strgeqConstant t in
  let t = Table.add (Symbol.symbol "getenv") getenvConstant t in
  let t = Table.add (Symbol.symbol "substring") substrConstant t in
  let t = Table.add (Symbol.symbol ",") andConstant t in
  let t = Table.add (Symbol.symbol ";") orConstant t in
  let t = Table.add (Symbol.symbol "&") ampandConstant t in
  let t = Table.add (Symbol.symbol ":-") colondashConstant t in
  let t = Table.add (Symbol.symbol "=>") implConstant t in
  let t = Table.add (Symbol.symbol "sigma") someConstant t in
  let t = Table.add (Symbol.symbol "pi") allConstant t in
  let t = Table.add (Symbol.symbol "is") isConstant t in
  let t = Table.add (Symbol.symbol "=") eqConstant t in
  let t = Table.add (Symbol.symbol "std_in") stdinConstant t in
  let t = Table.add (Symbol.symbol "std_out") stdoutConstant t in
  let t = Table.add (Symbol.symbol "std_err") stderrConstant t in
  let t = Table.add (Symbol.symbol "open_in") openinConstant t in
  let t = Table.add (Symbol.symbol "open_string") openstrConstant t in
  let t = Table.add (Symbol.symbol "open_out") openoutConstant t in
  let t = Table.add (Symbol.symbol "open_append") openappConstant t in
  let t = Table.add (Symbol.symbol "close_in") closeinConstant t in
  let t = Table.add (Symbol.symbol "eof") eofConstant t in
  let t = Table.add (Symbol.symbol "close_out") closeoutConstant t in
  let t = Table.add (Symbol.symbol "flush") flushConstant t in
  let t = Table.add (Symbol.symbol "term_to_string") termtostrConstant t in
  let t = Table.add (Symbol.symbol "string_to_term") strtotermConstant t in
  let t = Table.add (Symbol.symbol "output") outputConstant t in
  let t = Table.add (Symbol.symbol "input") inputConstant t in
  let t = Table.add (Symbol.symbol "input_line") inputlineConstant t in
  let t = Table.add (Symbol.symbol "look_ahead") lookaheadConstant t in
  let t = Table.add (Symbol.symbol "print") printConstant t in
  let t = Table.add (Symbol.symbol "read") readConstant t in
  let t = Table.add (Symbol.symbol "printterm") printtermConstant t in
  let t = Table.add (Symbol.symbol "readterm") readtermConstant t in
  let t = Table.add (Symbol.symbol "solve") solveConstant t in
  let t = Table.add (Symbol.symbol "not") notConstant t in
  let t = Table.add (Symbol.symbol "open_socket") opensocketConstant t in
  let t = Table.add (Symbol.symbol "~") overloadUMinusConstant t in
  let t = Table.add (Symbol.symbol "abs") overloadAbsConstant t in
  let t = Table.add (Symbol.symbol "+") overloadPlusConstant t in
  let t = Table.add (Symbol.symbol "-") overloadMinusConstant t in
  let t = Table.add (Symbol.symbol "*") overloadTimeConstant t in
  let t = Table.add (Symbol.symbol "<") overloadLTConstant t in
  let t = Table.add (Symbol.symbol ">") overloadGTConstant t in
  let t = Table.add (Symbol.symbol "<=") overloadLEConstant t in
  let t = Table.add (Symbol.symbol ">=") overloadGEConstant t in
  t


let isunivConstant tm = (tm == univConstant)

let isnilConstant tm = (tm == nilConstant)

let isconsConstant tm = (tm == consConstant)

let isintcConstant tm = (tm == intcConstant)

let isrealcConstant tm = (tm == realcConstant)

let isstrcConstant tm = (tm == strcConstant)

let istrueConstant tm = (tm == trueConstant)

let iscutConstant tm = (tm == cutConstant)

let isfailConstant tm = (tm == failConstant)

let ishaltConstant tm = (tm == haltConstant)

let isstopConstant tm = (tm == stopConstant)

let isintuminusConstant tm = (tm == intuminusConstant)

let ismodConstant tm = (tm == modConstant)

let isiabsConstant tm = (tm == iabsConstant)

let isintplusConstant tm = (tm == intplusConstant)

let isintminusConstant tm = (tm == intminusConstant)

let isintmultConstant tm = (tm == intmultConstant)

let isintdivConstant tm = (tm == intdivConstant)

let isintlssConstant tm = (tm == intlssConstant)

let isintgrtConstant tm = (tm == intgrtConstant)

let isintleqConstant tm = (tm == intleqConstant)

let isintgeqConstant tm = (tm == intgeqConstant)

let istimeConstant tm = (tm == timeConstant)

let isitorConstant tm = (tm == itorConstant)

let isfloorConstant tm = (tm == floorConstant)

let isceilConstant tm = (tm == ceilConstant)

let istruncConstant tm = (tm == truncConstant)

let isrealuminusConstant tm = (tm == realuminusConstant)

let issqrtConstant tm = (tm == sqrtConstant)

let issinConstant tm = (tm == sinConstant)

let iscosConstant tm = (tm == cosConstant)

let isarctanConstant tm = (tm == arctanConstant)

let islogConstant tm = (tm == logConstant)

let israbsConstant tm = (tm == rabsConstant)

let isrtosConstant tm = (tm == rtosConstant)

let isrealplusConstant tm = (tm == realplusConstant)

let isrealminusConstant tm = (tm == realminusConstant)

let isrealmultConstant tm = (tm == realmultConstant)

let isrealdivConstant tm = (tm == realdivConstant)

let isreallssConstant tm = (tm == reallssConstant)

let isrealgrtConstant tm = (tm == realgrtConstant)

let isrealleqConstant tm = (tm == realleqConstant)

let isrealgeqConstant tm = (tm == realgeqConstant)

let isslenConstant tm = (tm == slenConstant)

let isstoiConstant tm = (tm == stoiConstant)

let isitochrConstant tm = (tm == itochrConstant)

let isitostrConstant tm = (tm == itostrConstant)

let isscatConstant tm = (tm == scatConstant)

let isstrlssConstant tm = (tm == strlssConstant)

let isstrgrtConstant tm = (tm == strgrtConstant)

let isstrleqConstant tm = (tm == strleqConstant)

let isstrgeqConstant tm = (tm == strgeqConstant)

let isgetenvConstant tm = (tm == getenvConstant)

let issubstrConstant tm = (tm == substrConstant)

let isandConstant tm = (tm == andConstant)

let isorConstant tm = (tm == orConstant)

let isampandConstant tm = (tm == ampandConstant)

let iscolondashConstant tm = (tm == colondashConstant)

let isimplConstant tm = (tm == implConstant)

let issomeConstant tm = (tm == someConstant)

let isallConstant tm = (tm == allConstant)

let isisConstant tm = (tm == isConstant)

let iseqConstant tm = (tm == eqConstant)

let isstdinConstant tm = (tm == stdinConstant)

let isstdoutConstant tm = (tm == stdoutConstant)

let isstderrConstant tm = (tm == stderrConstant)

let isopeninConstant tm = (tm == openinConstant)

let isopenstrConstant tm = (tm == openstrConstant)

let isopenoutConstant tm = (tm == openoutConstant)

let isopenappConstant tm = (tm == openappConstant)

let iscloseinConstant tm = (tm == closeinConstant)

let iseofConstant tm = (tm == eofConstant)

let iscloseoutConstant tm = (tm == closeoutConstant)

let isflushConstant tm = (tm == flushConstant)

let istermtostrConstant tm = (tm == termtostrConstant)

let isstrtotermConstant tm = (tm == strtotermConstant)

let isoutputConstant tm = (tm == outputConstant)

let isinputConstant tm = (tm == inputConstant)

let isinputlineConstant tm = (tm == inputlineConstant)

let islookaheadConstant tm = (tm == lookaheadConstant)

let isprintConstant tm = (tm == printConstant)

let isreadConstant tm = (tm == readConstant)

let isprinttermConstant tm = (tm == printtermConstant)

let isreadtermConstant tm = (tm == readtermConstant)

let issolveConstant tm = (tm == solveConstant)

let isnotConstant tm = (tm == notConstant)

let isopensocketConstant tm = (tm == opensocketConstant)

let isgenericApplyConstant tm = (tm == genericApplyConstant)

let isoverloadUMinusConstant tm = (tm == overloadUMinusConstant)

let isoverloadAbsConstant tm = (tm == overloadAbsConstant)

let isoverloadPlusConstant tm = (tm == overloadPlusConstant)

let isoverloadMinusConstant tm = (tm == overloadMinusConstant)

let isoverloadTimeConstant tm = (tm == overloadTimeConstant)

let isoverloadLTConstant tm = (tm == overloadLTConstant)

let isoverloadGTConstant tm = (tm == overloadGTConstant)

let isoverloadLEConstant tm = (tm == overloadLEConstant)

let isoverloadGEConstant tm = (tm == overloadGEConstant)



let pervasiveKinds = buildPervasiveKinds ()

let pervasiveConstants = buildPervasiveConstants ()

let pervasiveTypeAbbrevs = Table.SymbolTable.empty

let andTerm = Absyn.ConstantTerm(andConstant, [], false, Errormsg.none)       
let implicationTerm = Absyn.ConstantTerm(implConstant, [], false, Errormsg.none)

let isPerv const =                                                            
  let constCat = Absyn.getConstantType(const) in                                
  match constCat with                                                          
   	Absyn.PervasiveConstant(_) -> true                                        
  | _ -> false                                                                 



let regClobberingPerv const =  
  if ((const == solveConstant) || (const == getenvConstant) || (const == strtotermConstant) || (const == readtermConstant) || (const == readConstant) || (const == isConstant) || (const == lookaheadConstant) || (const == inputConstant) || (const == inputlineConstant)) then true else false 



let backtrackablePerv const =  
  if ((const == eqConstant)) then true else false 



