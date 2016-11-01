(* This is taken from the Twelf implementation *)

  type region = Reg of int * int	(* r ::= (i,j) is interval [i,j) *)
  type location = Loc of string * region (* loc ::= (filename, region) *)

  (* line numbering, used when printing regions *)
  type linesInfo			(* mapping from character positions to lines in a file *)
  val resetLines : unit -> unit         (* reset line numbering *)
  val newLine : int -> unit		(* new line starts at character i *)
  val getLinesInfo : unit -> linesInfo  (* get lines info for current file *)

  val join : region * region -> region	(* join(r1,r2) = smallest region enclosing r1 and r2 *)
  val toString : region -> string	(* line1.col1-line2.col2, parsable by Emacs *)
  val wrap : region * string -> string  (* add region to error message, parsable by Emacs *)
  val wrapLoc : location * string -> string  (* add location to error message, also parsable *)
  val wrapLoc' : location * linesInfo option * string -> string
					(* add location to error message in line.col format *)

  (* Paths, occurrences and occurrence trees only work well for normal forms *)
  (* In the general case, regions only approximate true source location *)

  (* Follow path through a term to obtain subterm *)
  type path =
     Label of path			(* [x:#] U or {x:#} V *)
   | Body of path			(* [x:V] # or {x:V} # *)
   | Head				(* # @ S, term in normal form *)
   | Arg of int * path			(* H @ S1; ...; #; ...; Sn; Nil *)
   | Here				(* #, covers Uni, EVar, Redex(?) *)

  (*
     Construct an occurrence when traversing a term.
     The resulting occurrence can be translated to a region
     via an occurrence tree stored with the term.

     An occurrence is a path in reverse order.
  *)
  type occ =
     OccTop 
   | OccLabel of occ
   | OccBody of occ
   | OccHead of occ
   | OccArg of int * occ 

  (*
     An occurrence tree is a data structure mapping occurrences in a term
     to regions in an input stream.  Occurrence trees are constructed during parsing.
  *)
  type occExp =				(* occurrence tree for u expressions *)
     Leaf of region 		(* could be _ or identifier *)
   | Bind of region * occExp option * occExp 
   | Root of region * occExp * int * int * occSpine 
  and occSpine =				(* occurrence tree for s spines *)
     App of occExp * occSpine 
   | Nils 

  type occConDec =			(* occurrence tree for constant declarations *)
     Dec of int * occExp              (* (#implicit, v) in c : V *)
   | Def of int * occExp * occExp option
					(* (#implicit, u, v) in c : V = U *)

  val toRegion : occExp -> region
  val toRegionSpine : occSpine * region -> region

  val posToPath : occExp -> int -> path

  val occToRegionExp : occExp -> occ -> region
  val occToRegionDec : occConDec -> occ -> region (* into v for c : V *)
  val occToRegionDef1 : occConDec -> occ -> region (* into u for c : V = U *)
  val occToRegionDef2 : occConDec -> occ -> region (* into v for c : V = U *)
  val occToRegionClause : occConDec -> occ -> region (* into v for c : V ... *)
