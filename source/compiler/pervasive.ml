type symbol = Symbol.symbol

(**********************************************************************
* Pervasive (initial) symbol tables
**********************************************************************)

let buildPervasiveConstants = function ktable -> 
  let currentId = ref 1 in
  let nextId () =
    (currentId := !currentId + 1;
    !currentId)
  in
    
  let kint = Option.get (Table.find (Symbol.symbol "int") ktable) in
  let kreal = Option.get (Table.find (Symbol.symbol "real") ktable) in
  let kstring = Option.get (Table.find (Symbol.symbol "string") ktable) in
  let kinstream = Option.get (Table.find (Symbol.symbol "in_stream") ktable) in
  let koutstream = Option.get (Table.find (Symbol.symbol "out_stream") ktable) in
  let ko = Option.get (Table.find (Symbol.symbol "o") ktable) in
  let klist = Option.get (Table.find (Symbol.symbol "list") ktable) in
  
  let t = Table.SymbolTable.empty in

  (*  Logical Operators *)
  let ty = Absyn.AppType(ko, []) in
  let t = Table.add (Symbol.symbol "true") (Absyn.Constant(Symbol.symbol "true", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref(Some(Absyn.Skeleton(ty, ref None, ref false))), ref 0, ref None, ref (Some Absyn.Builtin), ref(Absyn.PervasiveConstant(true)), ref (nextId ()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "false") (Absyn.Constant(Symbol.symbol "false", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref(Some(Absyn.Skeleton(ty, ref None, ref false))), ref 0, ref None, ref (Some Absyn.Builtin), ref(Absyn.PervasiveConstant(true)), ref (nextId ()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "!") (Absyn.Constant(Symbol.symbol "!", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref(Some(Absyn.Skeleton(ty, ref None, ref false))), ref 0, ref None, ref (Some Absyn.Builtin), ref(Absyn.PervasiveConstant(false)), ref (nextId ()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "fail") (Absyn.Constant(Symbol.symbol "fail", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref(Some(Absyn.Skeleton(ty, ref None, ref false))), ref 0, ref None, ref (Some Absyn.Builtin), ref(Absyn.PervasiveConstant(true)), ref (nextId ()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "halt") (Absyn.Constant(Symbol.symbol "halt", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref(Some(Absyn.Skeleton(ty, ref None, ref false))), ref 0, ref None, ref (Some Absyn.Builtin), ref(Absyn.PervasiveConstant(true)), ref (nextId ()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "stop") (Absyn.Constant(Symbol.symbol "stop", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref(Some(Absyn.Skeleton(ty, ref None, ref false))), ref 0, ref None, ref (Some Absyn.Builtin), ref(Absyn.PervasiveConstant(true)), ref (nextId ()), Errormsg.none)) t in
  
  (*
  let ty = Absyn.ArrowType(Absyn.AppType(ko, []), Absyn.ArrowType(Absyn.AppType(ko, []), Absyn.AppType(ko, []))) in
  let t = Table.add (Symbol.symbol ",") (Absyn.Constant(Symbol.symbol ",", ref Absyn.Infixl, ref 110, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(false)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "&") (Absyn.Constant(Symbol.symbol "&", ref Absyn.Infixl, ref 120, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(false)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol ";") (Absyn.Constant(Symbol.symbol ";", ref Absyn.Infixl, ref 100, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(false)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol ":-") (Absyn.Constant(Symbol.symbol ":-", ref Absyn.Infix, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(false)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "=>") (Absyn.Constant(Symbol.symbol "=>", ref Absyn.Infixr, ref 130, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(false)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.ArrowType(Absyn.SkeletonVarType(0), Absyn.AppType(ko, [])), Absyn.AppType(ko, [])) in
  let ty' = Absyn.ArrowType(Absyn.ArrowType(Absyn.TypeVarType(ref None, false), Absyn.AppType(ko, [])), Absyn.AppType(ko, [])) in
  let t = Table.add (Symbol.symbol "sigma") (Absyn.Constant(Symbol.symbol "sigma", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty'], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "pi") (Absyn.Constant(Symbol.symbol "pi", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty'], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.AppType(ko, []), Absyn.AppType(ko, [])) in
  let t = Table.add (Symbol.symbol "not") (Absyn.Constant(Symbol.symbol "not", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  (*  Integer Arithmetic  *)
  let ty' = Absyn.TypeSetType(Absyn.AppType(kint, []), ref [Absyn.AppType(kint, []); Absyn.AppType(kreal, [])]) in
  let ty = Absyn.ArrowType(ty', Absyn.TypeRefType(ty')) in
  let t = Table.add (Symbol.symbol "~") (Absyn.Constant(Symbol.symbol "~", ref Absyn.Prefix, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "abs") (Absyn.Constant(Symbol.symbol "abs", ref Absyn.Prefix, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(ty', Absyn.ArrowType(ty', ty')) in
  let t = Table.add (Symbol.symbol "+") (Absyn.Constant(Symbol.symbol "+", ref Absyn.Infixl, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "-") (Absyn.Constant(Symbol.symbol "-", ref Absyn.Infixl, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "*") (Absyn.Constant(Symbol.symbol "*", ref Absyn.Infixl, ref 160, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.AppType(kint, []), Absyn.ArrowType(Absyn.AppType(kint, []), Absyn.AppType(kint, []))) in
  let t = Table.add (Symbol.symbol "div") (Absyn.Constant(Symbol.symbol "div", ref Absyn.Infixl, ref 160, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "mod") (Absyn.Constant(Symbol.symbol "mod", ref Absyn.Infixl, ref 160, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  (*  Floating point arithmetic *)
  let ty = Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.AppType(kreal, [])) in
  let t = Table.add (Symbol.symbol "ln") (Absyn.Constant(Symbol.symbol "ln", ref Absyn.Infixl, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "sqrt") (Absyn.Constant(Symbol.symbol "sqrt", ref Absyn.Infixl, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "sin") (Absyn.Constant(Symbol.symbol "sin", ref Absyn.Infixl, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "cos") (Absyn.Constant(Symbol.symbol "cos", ref Absyn.Infixl, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "arctan") (Absyn.Constant(Symbol.symbol "arctan", ref Absyn.Infixl, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.AppType(kreal, []))) in
  let t = Table.add (Symbol.symbol "/") (Absyn.Constant(Symbol.symbol "/", ref Absyn.Infixl, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in

  let ty = Absyn.ArrowType(Absyn.AppType(kint, []), Absyn.AppType(kreal, [])) in
  let t = Table.add (Symbol.symbol "int_to_real") (Absyn.Constant(Symbol.symbol "int_to_real", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.AppType(kint, []), Absyn.AppType(kstring, [])) in
  let t = Table.add (Symbol.symbol "int_to_string") (Absyn.Constant(Symbol.symbol "int_to_string", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.AppType(kint, [])) in
  let t = Table.add (Symbol.symbol "truncate") (Absyn.Constant(Symbol.symbol "truncate", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "ceil") (Absyn.Constant(Symbol.symbol "ceil", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "floor") (Absyn.Constant(Symbol.symbol "floor", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.AppType(kstring, [])) in
  let t = Table.add (Symbol.symbol "real_to_string") (Absyn.Constant(Symbol.symbol "real_to_string", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in

  let ty = Absyn.ArrowType(Absyn.AppType(kstring, []), Absyn.AppType(kint, [])) in
  let t = Table.add (Symbol.symbol "string_to_int") (Absyn.Constant(Symbol.symbol "string_to_int", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.AppType(kstring, []), Absyn.AppType(kreal, [])) in
  let t = Table.add (Symbol.symbol "string_to_real") (Absyn.Constant(Symbol.symbol "string_to_real", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  (*  String Operations *)
  let t = Table.add (Symbol.symbol "^") (Absyn.Constant(Symbol.symbol "^", ref Absyn.Infixl, ref 150, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.AppType(kstring, []), Absyn.AppType(kint, [])) in
  let t = Table.add (Symbol.symbol "size") (Absyn.Constant(Symbol.symbol "size", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref Some(Absyn.Skeleton(ty, ref None, ref false)), [ty], Absyn.Builtin, ref (nextId ()), ref(Absyn.PervasiveConstant(true)), Errormsg.none)) t in
  *)
  
  (*  List operations *)
  let ty = Absyn.AppType(klist, [Absyn.SkeletonVarType(ref 0)]) in
  let ty' = Absyn.AppType(klist, [Absyn.makeTypeVariable ()]) in
  let t = Table.add (Symbol.symbol "nil") (Absyn.Constant(Symbol.symbol "nil", ref Absyn.NoFixity, ref 0, ref false, ref false, ref false, ref false, ref true, ref false, ref(Some(Absyn.Skeleton(ty, ref (Some 1), ref false))), ref 1, ref None, ref (Some Absyn.Builtin), ref(Absyn.PervasiveConstant(true)), ref (nextId ()), Errormsg.none)) t in
  
  let ty = Absyn.ArrowType(Absyn.SkeletonVarType(ref 0), Absyn.ArrowType(Absyn.AppType(klist, [Absyn.SkeletonVarType(ref 0)]), Absyn.AppType(klist, [Absyn.SkeletonVarType(ref 0)]))) in
  let v = Absyn.TypeVarType(ref None, ref false) in
  let ty' = Absyn.ArrowType(v, Absyn.ArrowType(Absyn.AppType(klist, [v]), Absyn.AppType(klist, [v]))) in
  let t = Table.add (Symbol.symbol "::") (Absyn.Constant(Symbol.symbol "::", ref Absyn.Infixr, ref (-2), ref false, ref false, ref false, ref false, ref true, ref false, ref(Some(Absyn.Skeleton(ty, ref (Some 1), ref false))), ref 1, ref None, ref (Some Absyn.Builtin), ref(Absyn.PervasiveConstant(true)), ref (nextId ()), Errormsg.none)) t in  
  t

let buildPervasiveKinds = function () ->
  let currentId = ref 1 in
  let nextId () =
    (currentId := !currentId + 1;
    !currentId)
  in

  let t = Table.SymbolTable.empty in
  let t = Table.add (Symbol.symbol "int") (Absyn.PervasiveKind(Symbol.symbol "int", Some 0, ref (nextId()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "string") (Absyn.PervasiveKind(Symbol.symbol "string", Some 0, ref (nextId()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "real") (Absyn.PervasiveKind(Symbol.symbol "real", Some 0, ref (nextId()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "in_stream") (Absyn.PervasiveKind(Symbol.symbol "in_stream", Some 0, ref (nextId()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "out_stream") (Absyn.PervasiveKind(Symbol.symbol "out_stream", Some 0, ref (nextId()), Errormsg.none)) t in
  let t = Table.add (Symbol.symbol "o") (Absyn.PervasiveKind(Symbol.symbol "o", Some 0, ref (nextId()), Errormsg.none)) t in
  
  let t = Table.add (Symbol.symbol "list") (Absyn.PervasiveKind(Symbol.symbol "list", Some 1, ref (nextId ()), Errormsg.none)) t in
  
  t


(*  Build Tables  *)
let pervasiveKinds = buildPervasiveKinds ()
let pervasiveConstants = buildPervasiveConstants pervasiveKinds
let pervasiveTypeAbbrevs = Table.SymbolTable.empty

(*  Various constants needed elsewhere. *)
let genericApplyConstant = (Absyn.Constant(Symbol.symbol " apply ", ref Absyn.Infixl, ref (Absyn.maxPrec + 2), ref false, ref false, ref false, ref false, ref true, ref false, ref(Some(Absyn.Skeleton(Absyn.ErrorType, ref None, ref false))), ref 0, ref None, ref (Some Absyn.Builtin), ref(Absyn.PervasiveConstant(true)), ref 0, Errormsg.none))

let makeConstant = fun s ->
  match (Table.find (Symbol.symbol s) pervasiveConstants) with
    Some(c) -> c
  | None -> Errormsg.impossible Errormsg.none ("Pervasive.makeConstant: " ^ s ^ " not defined.")

let makeKind = fun s ->
  match (Table.find (Symbol.symbol s) pervasiveKinds) with
    Some(k) -> k
  | None -> Errormsg.impossible Errormsg.none ("Pervasive.makeKind: " ^ s ^ " not defined.")

let commaConstant = makeConstant ","
let semiColonConstant = makeConstant ";"
let consConstant = makeConstant "::"
let nilConstant = makeConstant "nil"
let allConstant = makeConstant "all"
let andConstant = makeConstant "&"
let orConstant = semiColonConstant
let someConstant = makeConstant "some"
let colonDashConstant = makeConstant ":-"
let implicationConstant = makeConstant "<-"
let cutConstant = makeConstant "!"

let boolKind = makeKind "bool"

let andTerm = Absyn.ConstantTerm(andConstant, [], false, Errormsg.none)
let implicationTerm = Absyn.ConstantTerm(implicationConstant, [], false, Errormsg.none)