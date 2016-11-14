(* This is taken from the Twelf implementation *)

  type term =				(* term *)
     Internal of IntSyn.exp * IntSyn.exp * Paths.region (* (U, V, r) *)
        (* G |- U : V nf where V : L or V == kind *)
        (* not used currently *)

   | Constant of IntSyn.head * Paths.region
   | Bvar of int * Paths.region
   | Typ of Paths.region
   | Arrow of term * term
   | Pi of dec * term
   | Lam of dec * term
   | App of term * term
   | Hastype of term * term
   | Mismatch of term * term * string * string
   | Omitted of Paths.region
   | Lcid of string list * string * Paths.region
   | Ucid of string list * string * Paths.region
   | Quid of string list * string * Paths.region
   | Scon of string * Paths.region
   | Evar of string * Paths.region
   | Fvar of string * Paths.region
   | Omitapx of Approx.exp * Approx.exp * Approx.uni * Paths.region
   | Omitexact of IntSyn.exp * IntSyn.exp * Paths.region
  and dec =				(* variable declaration *)
     Dec of string option * term * Paths.region
   | Dec0 of string option * Paths.region


  let lcid (a,b,c) = Lcid(a,b,c) (* lower case id *)
  let ucid (a,b,c) = Ucid(a,b,c) (* upper case id *)
  let quid (a,b,c) = Quid(a,b,c) (* quoted id, currently not parsed *)
  let scon (a,b) = Scon(a,b) (* string constant *)

  let backarrow (tm1, tm2) = Arrow(tm2, tm1)

  (* unconditionally interpreted as such *)
  let evar (a,b) = Evar(a,b)
  let fvar (a,b) = Fvar(a,b)



  let typ a = Typ(a)	(* type, region for "type" *)
  let arrow (a,b) = Arrow(a,b)	(* tm -> tm *)
  let pi (a,b) = Pi(a,b)           (* {d} tm *)
  let lam (a,b) = Lam(a,b)          (* [d] tm *)
  let app (a,b) = App(a,b)	(* tm tm *)
  let hastype (a,b) = Hastype(a,b)	(* tm : tm *)
  let omitted a = Omitted(a)	(* _ as object, region for "_" *)

  (* region for "{dec}" "[dec]" etc. *)
  let dec (a,b,c) = Dec(a,b,c) (* id : tm | _ : tm *)
  let dec0 (a,b) = Dec0(a,b) (* id | _  (type omitted) *)



  type condec =				(* constant declaration *)
     Condec of string * term

  let condec (a,b) = Condec(a,b)	(* id : tm *) 


  type query =			(* query *)
     Query of string option * term

  let query (a,b) = Query(a,b) (* ucid : tm | tm *)

  let rec string_of_term t =
    match t with
     Internal _ -> "internal" 
   | Constant (IntSyn.Const(cid),_) -> "constant("^(IntSyn.conDecName (IntSyn.sgnLookup cid))^")"
   | Bvar _ -> "bvar"
   | Typ _ -> "type"
   | Arrow (t1,t2) -> "arrow("^(string_of_term t1)^", "^(string_of_term t2)^")" 
   | Pi (Dec (Some(n),t,_), b) -> "pi("^n^" : "^(string_of_term t)^". "^(string_of_term b)^")"
   | Pi (Dec (None,t,_), b) -> "pi("^(string_of_term t)^" -> "^(string_of_term b)^")"
   | Pi _ -> "pi" 
   | Lam _ -> "lam"
   | App (t1, t2) -> "app("^(string_of_term t1)^", "^(string_of_term t2)^")"
   | Hastype _ -> "hastype"
   | Mismatch _ -> "mismatch"
   | Omitted _ -> "omitted"
   | Lcid _ -> "lcid"
   | Ucid _ -> "ucid"
   | Quid _ -> "quid"
   | Scon _ -> "scon"
   | Evar _ -> "evar"
   | Fvar (n,_) -> "fvar("^n^")"
   | Omitapx _ -> "omitapx"
   | Omitexact _ -> "omitexact"
