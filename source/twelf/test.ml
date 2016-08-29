let z = 
  Lfabsyn.Object(Lfabsyn.Const("z", Errormsg.none), 
                 Lfabsyn.IdType(Lfabsyn.Const("nat", Errormsg.none), Errormsg.none),  
                 Lfabsyn.Prefix, 
                 Lfabsyn.None, 
                 100, 
                 Errormsg.none)

let s = 
  Lfabsyn.Object(Lfabsyn.Const("s", Errormsg.none), 
                 (Lfabsyn.PiType(Lfabsyn.Const("N", Errormsg.none),
                                 Lfabsyn.IdType(Lfabsyn.Const("nat", Errormsg.none), Errormsg.none), 
                                (Lfabsyn.IdType(Lfabsyn.Const("nat", Errormsg.none), Errormsg.none)), 
                                Errormsg.none)), 
                 Lfabsyn.Prefix, 
                 Lfabsyn.None, 
                 100, 
                 Errormsg.none)

let nat = 
  Lfabsyn.TypeFam(Lfabsyn.Const("nat", Errormsg.none), 
                  Lfabsyn.Type(Errormsg.none), 
                  Lfabsyn.Prefix, 
                  Lfabsyn.None, 
                  100, 
                  ref [ref z; ref s], 
                  Errormsg.none)

let nil =
  Lfabsyn.Object(Lfabsyn.Const("nil", Errormsg.none),
                 Lfabsyn.AppType(Lfabsyn.Const("list", Errormsg.none), 
                                 [Lfabsyn.IdTerm(Lfabsyn.Const("z", Errormsg.none), Errormsg.none)],  
                                 Errormsg.none),
                 Lfabsyn.Prefix,
                 Lfabsyn.None,
                 100,
                 Errormsg.none)

let cons =
  Lfabsyn.Object(Lfabsyn.Const("cons", Errormsg.none),
                 Lfabsyn.PiType(Lfabsyn.Var("N", Errormsg.none),
                                Lfabsyn.IdType(Lfabsyn.Const("nat", Errormsg.none), Errormsg.none),
                                Lfabsyn.ImpType(Lfabsyn.IdType(Lfabsyn.Const("nat", Errormsg.none), Errormsg.none),
                                                Lfabsyn.ImpType(Lfabsyn.AppType(Lfabsyn.Const("list", Errormsg.none),
                                                                                [Lfabsyn.IdTerm(Lfabsyn.Var("N", Errormsg.none), Errormsg.none)],
                                                                                 Errormsg.none),
                                                                Lfabsyn.AppType(Lfabsyn.Const("list", Errormsg.none),
                                                                                [Lfabsyn.AppTerm(Lfabsyn.Const("s", Errormsg.none),
                                                                                                 [Lfabsyn.IdTerm(Lfabsyn.Var("N", Errormsg.none), Errormsg.none)], 
                                                                                                 Errormsg.none)],
                                                                                Errormsg.none),
                                                                Errormsg.none),
                                                Errormsg.none),
                                Errormsg.none),
                 Lfabsyn.Prefix,
                 Lfabsyn.None,
                 100,
                 Errormsg.none)

let list =
  Lfabsyn.TypeFam(Lfabsyn.Const("list", Errormsg.none),
                  Lfabsyn.ImpKind(Lfabsyn.IdType(Lfabsyn.Const("nat", Errormsg.none), Errormsg.none),
                                  Lfabsyn.Type(Errormsg.none),
                                  Errormsg.none),
                  Lfabsyn.Prefix,
                  Lfabsyn.None,
                  100,
                  ref [ref nil; ref cons],
                  Errormsg.none)

let sign = 
  Lfsig.Signature(ref ["append"], 
                  Symboltable.insert (Symboltable.insert Symboltable.empty 
                                                         (Symb.symbol "nat") 
                                                         nat)
                                     (Symb.symbol "list")
                                     list)

let q1 = 
  Lfabsyn.Query([], 
                Lfabsyn.LogicVar("M",Errormsg.none), 
                Lfabsyn.IdType(Lfabsyn.Const("nat",Errormsg.none),Errormsg.none))

let q2 =
  Lfabsyn.Query([Lfabsyn.LogicVar("X",Errormsg.none), 
                 Lfabsyn.IdType(Lfabsyn.Const("nat",Errormsg.none),Errormsg.none)],
                Lfabsyn.LogicVar("M",Errormsg.none), 
                Lfabsyn.IdType(Lfabsyn.Const("list",Errormsg.none),Errormsg.none))


(*
let lpsign = Translator.NaiveTranslation.translate sign


let _ = Optimization.Specialize.set true;
        Optimization.Swap.set true


let lpoptsign = Translator.OptimizedTranslation.translate sign
*)
