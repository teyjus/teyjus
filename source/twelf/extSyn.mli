  type term =				(* term *)
     Typ of Paths.Paths.region
   | Arrow of term * term
   | Backarrow of term * term
   | Pi of dec * term
   | Lam of dec * term
   | App of term * term
   | Hastype of term * term
   | Omitted of Paths.Paths.region
   | Lcid of string list * string * Paths.Paths.region
   | Ucid of string list * string * Paths.Paths.region
   | Quid of string list * string * Paths.Paths.region
   | Scon of string * Paths.Paths.region
   | Evar of string * Paths.Paths.region
   | Fvar of string * Paths.Paths.region
  and dec =				(* variable declaration *)
     Dec of string option * term * Paths.Paths.region
   | Dec0 of string option * Paths.Paths.region


  val lcid : string list * string * Paths.Paths.region -> term (* lower case id *)
  val ucid : string list * string * Paths.Paths.region -> term (* upper case id *)
  val quid : string list * string * Paths.Paths.region -> term (* quoted id, currently not parsed *)
  val scon : string * Paths.Paths.region -> term (* string constant *)



  (* unconditionally interpreted as such *)
  val evar : string * Paths.Paths.region -> term
  val fvar : string * Paths.Paths.region -> term



  val typ : Paths.Paths.region -> term	(* type, region for "type" *)
  val arrow : term * term -> term	(* tm -> tm *)
  val backarrow : term * term -> term	(* tm <- tm *)
  val pi : dec * term -> term           (* {d} tm *)
  val lam : dec * term -> term          (* [d] tm *)
  val app : term * term -> term		(* tm tm *)
  val hastype : term * term -> term	(* tm : tm *)
  val omitted : Paths.Paths.region -> term	(* _ as object, region for "_" *)


  (* region for "{dec}" "[dec]" etc. *)
  val dec : string option * term * Paths.Paths.region -> dec (* id : tm | _ : tm *)
  val dec0 : string option * Paths.Paths.region -> dec (* id | _  (type omitted) *)



  type condec =				(* constant declaration *)
     Condec of string * term

  val condec : string * term -> condec	(* id : tm *) 


  type query =			(* query *)
     Query of string option * term

  val query : string option * term -> query (* ucid : tm | tm *)

