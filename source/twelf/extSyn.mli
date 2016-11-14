(* This is taken from the Twelf implementation *)

  type term =				(* term *)
     Internal of IntSyn.exp * IntSyn.exp * Paths.region (* (U, V, r) *)
        (* G |- U : V nf where V : L or V == kind *)
        (* not used currently *)
   | Constant of IntSyn.head * Paths.region
   | Bvar of int * Paths.region
   | Typ of Paths.region
   | Arrow of term * term
   | Backarrow of term * term
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


  val lcid : string list * string * Paths.region -> term (* lower case id *)
  val ucid : string list * string * Paths.region -> term (* upper case id *)
  val quid : string list * string * Paths.region -> term (* quoted id, currently not parsed *)
  val scon : string * Paths.region -> term (* string constant *)



  (* unconditionally interpreted as such *)
  val evar : string * Paths.region -> term
  val fvar : string * Paths.region -> term



  val typ : Paths.region -> term	(* type, region for "type" *)
  val arrow : term * term -> term	(* tm -> tm *)
  val backarrow : term * term -> term	(* tm <- tm *)
  val pi : dec * term -> term           (* {d} tm *)
  val lam : dec * term -> term          (* [d] tm *)
  val app : term * term -> term		(* tm tm *)
  val hastype : term * term -> term	(* tm : tm *)
  val omitted : Paths.region -> term	(* _ as object, region for "_" *)

  (* region for "{dec}" "[dec]" etc. *)
  val dec : string option * term * Paths.region -> dec (* id : tm | _ : tm *)
  val dec0 : string option * Paths.region -> dec (* id | _  (type omitted) *)



  type condec =				(* constant declaration *)
     Condec of string * term

  val condec : string * term -> condec	(* id : tm *) 


  type query =			(* query *)
     Query of string option * term

  val query : string option * term -> query (* ucid : tm | tm *)

  val string_of_term : term -> string
