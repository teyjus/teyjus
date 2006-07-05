(**********************************************************************
*	Pervasive (initial) symbol tables
**********************************************************************)
let buildPervasiveConstants = function ktable ->
	let i = ref 0 in
	let currentBuiltin = fun () ->
		(Absyn.BuiltinIndex(!i))
	in
	
	let nextBuiltin = fun () ->
		(i := !i + 1;
		Absyn.BuiltinIndex(!i))
	in
	
	let Some(kint) = Table.find (Symbol.symbol "int") ktable in
	let Some(kreal) = Table.find (Symbol.symbol "real") ktable in
	let Some(kstring) = Table.find (Symbol.symbol "string") ktable in
	let Some(kinstream) = Table.find (Symbol.symbol "in_stream") ktable in
	let Some(koutstream) = Table.find (Symbol.symbol "out_stream") ktable in
	let Some(ko) = Table.find (Symbol.symbol "o") ktable in
	let Some(klist) = Table.find (Symbol.symbol "list") ktable in
	
	let t = Table.SymbolTable.empty in

	(*	Logical Operators	*)
	let ty = Absyn.AppType(ko, []) in
	let t = Table.add (Symbol.symbol "true") (Absyn.Constant(Symbol.symbol "true", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], currentBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "!") (Absyn.Constant(Symbol.symbol "!", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "fail") (Absyn.Constant(Symbol.symbol "fail", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "halt") (Absyn.Constant(Symbol.symbol "halt", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "stop") (Absyn.Constant(Symbol.symbol "stop", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.AppType(ko, []), Absyn.ArrowType(Absyn.AppType(ko, []), Absyn.AppType(ko, []))) in
	let t = Table.add (Symbol.symbol ",") (Absyn.Constant(Symbol.symbol ",", Absyn.Infixl, 110, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "&") (Absyn.Constant(Symbol.symbol "&", Absyn.Infixl, 120, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol ";") (Absyn.Constant(Symbol.symbol ";", Absyn.Infixl, 100, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol ":-") (Absyn.Constant(Symbol.symbol ":-", Absyn.Infix, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "=>") (Absyn.Constant(Symbol.symbol "=>", Absyn.Infixr, 130, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.ArrowType(Absyn.SkeletonVarType(0), Absyn.AppType(ko, [])), Absyn.AppType(ko, [])) in
	let ty' = Absyn.ArrowType(Absyn.ArrowType(Absyn.TypeVarType(None, false), Absyn.AppType(ko, [])), Absyn.AppType(ko, [])) in
	let t = Table.add (Symbol.symbol "sigma") (Absyn.Constant(Symbol.symbol "sigma", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty'], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "pi") (Absyn.Constant(Symbol.symbol "pi", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty'], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.AppType(ko, []), Absyn.AppType(ko, [])) in
	let t = Table.add (Symbol.symbol "not") (Absyn.Constant(Symbol.symbol "not", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	(*	Integer Arithmetic	*)
	let ty = Absyn.ArrowType(Absyn.AppType(kint, []), Absyn.AppType(kint, [])) in
	let t = Table.add (Symbol.symbol "~") (Absyn.Constant(Symbol.symbol "~", Absyn.Prefix, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "abs") (Absyn.Constant(Symbol.symbol "abs", Absyn.Prefix, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.AppType(kint, []), Absyn.ArrowType(Absyn.AppType(kint, []), Absyn.AppType(kint, []))) in
	let t = Table.add (Symbol.symbol "+") (Absyn.Constant(Symbol.symbol "+", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "-") (Absyn.Constant(Symbol.symbol "-", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "*") (Absyn.Constant(Symbol.symbol "*", Absyn.Infixl, 160, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "div") (Absyn.Constant(Symbol.symbol "div", Absyn.Infixl, 160, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "mod") (Absyn.Constant(Symbol.symbol "mod", Absyn.Infixl, 160, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	(*	Floating point arithmetic	*)
	let ty = Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.AppType(kreal, [])) in
	let t = Table.add (Symbol.symbol "r~") (Absyn.Constant(Symbol.symbol "r~", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "rabs") (Absyn.Constant(Symbol.symbol "rabs", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "ln") (Absyn.Constant(Symbol.symbol "ln", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "sqrt") (Absyn.Constant(Symbol.symbol "sqrt", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "sin") (Absyn.Constant(Symbol.symbol "sin", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "cos") (Absyn.Constant(Symbol.symbol "cos", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "arctan") (Absyn.Constant(Symbol.symbol "arctan", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.AppType(kreal, []))) in
	let t = Table.add (Symbol.symbol "r+") (Absyn.Constant(Symbol.symbol "r+", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "r-") (Absyn.Constant(Symbol.symbol "r-", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "r*") (Absyn.Constant(Symbol.symbol "r*", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "/") (Absyn.Constant(Symbol.symbol "/", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in

	let ty = Absyn.ArrowType(Absyn.AppType(kint, []), Absyn.AppType(kreal, [])) in
	let t = Table.add (Symbol.symbol "int_to_real") (Absyn.Constant(Symbol.symbol "int_to_real", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.AppType(kint, []), Absyn.AppType(kstring, [])) in
	let t = Table.add (Symbol.symbol "int_to_string") (Absyn.Constant(Symbol.symbol "int_to_string", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.AppType(kint, [])) in
	let t = Table.add (Symbol.symbol "truncate") (Absyn.Constant(Symbol.symbol "truncate", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "ceil") (Absyn.Constant(Symbol.symbol "ceil", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "floor") (Absyn.Constant(Symbol.symbol "floor", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.AppType(kreal, []), Absyn.AppType(kstring, [])) in
	let t = Table.add (Symbol.symbol "real_to_string") (Absyn.Constant(Symbol.symbol "real_to_string", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in

	let ty = Absyn.ArrowType(Absyn.AppType(kstring, []), Absyn.AppType(kint, [])) in
	let t = Table.add (Symbol.symbol "string_to_int") (Absyn.Constant(Symbol.symbol "string_to_int", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.AppType(kstring, []), Absyn.AppType(kreal, [])) in
	let t = Table.add (Symbol.symbol "string_to_real") (Absyn.Constant(Symbol.symbol "string_to_real", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	(*	String Operations	*)
	let t = Table.add (Symbol.symbol "^") (Absyn.Constant(Symbol.symbol "^", Absyn.Infixl, 150, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.AppType(kstring, []), Absyn.AppType(kint, [])) in
	let t = Table.add (Symbol.symbol "size") (Absyn.Constant(Symbol.symbol "size", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 0, true)], [ty], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in

	(*	List operations	*)
	let ty = Absyn.AppType(klist, [Absyn.SkeletonVarType(0)]) in
	let ty' = Absyn.AppType(klist, [Absyn.TypeVarType(None, false)]) in
	let t = Table.add (Symbol.symbol "nil") (Absyn.Constant(Symbol.symbol "nil", Absyn.NoFixity, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 1, true)], [ty'], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in
	
	let ty = Absyn.ArrowType(Absyn.SkeletonVarType(0), Absyn.ArrowType(Absyn.AppType(klist, [Absyn.SkeletonVarType(0)]), Absyn.AppType(klist, [Absyn.SkeletonVarType(0)]))) in
	let v = Absyn.TypeVarType(None, false) in
	let ty' = Absyn.ArrowType(v, Absyn.ArrowType(Absyn.AppType(klist, [v]), Absyn.AppType(klist, [v]))) in
	let t = Table.add (Symbol.symbol "::") (Absyn.Constant(Symbol.symbol "::", Absyn.Infixr, 0, false, false, false, false, true, [Absyn.Skeleton(ty, 1, true)], [ty'], nextBuiltin (), Absyn.GlobalConstant, Errormsg.none)) t in	
	t

let buildPervasiveKinds = function () ->
	let t = Table.SymbolTable.empty in
	let t = Table.add (Symbol.symbol "int") (Absyn.PervasiveKind(Symbol.symbol "int", Some 0, Absyn.KindIndex(0), Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "string") (Absyn.PervasiveKind(Symbol.symbol "string", Some 0, Absyn.KindIndex(0), Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "real") (Absyn.PervasiveKind(Symbol.symbol "real", Some 0, Absyn.KindIndex(0), Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "in_stream") (Absyn.PervasiveKind(Symbol.symbol "in_stream", Some 0, Absyn.KindIndex(0), Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "out_stream") (Absyn.PervasiveKind(Symbol.symbol "out_stream", Some 0, Absyn.KindIndex(0), Errormsg.none)) t in
	let t = Table.add (Symbol.symbol "o") (Absyn.PervasiveKind(Symbol.symbol "o", Some 0, Absyn.KindIndex(0), Errormsg.none)) t in
	
	let t = Table.add (Symbol.symbol "list") (Absyn.PervasiveKind(Symbol.symbol "list", Some 1, Absyn.KindIndex(0), Errormsg.none)) t in
	
	t

let pervasiveKinds = buildPervasiveKinds ()
let pervasiveConstants = buildPervasiveConstants pervasiveKinds
let pervasiveTypeAbbrevs = Table.SymbolTable.empty
