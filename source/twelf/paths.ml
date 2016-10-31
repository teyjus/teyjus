(* This is taken from the Twelf implementation *)

module type PATHS = 
sig
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

end;;

module Paths : PATHS = 
struct
  type pos = int			(* characters, starting at 0 *)
  type region = Reg of pos * pos	(* r ::= (i,j) is interval [i,j) *)
  type location = Loc of string * region (* loc ::= (filename, region) *)

  type linesInfo = pos list
  let posToLineCol' (linesInfo, i) =
      let rec ptlc l =
        match l with
            (j::js) -> if i >= j then (List.length js, i-j)
			     else ptlc js
	     (* first line should start at 0 *)
	     (* nil means first "line" was not terminated by <newline> *)
	   | [] -> (0, i)
      in
	ptlc linesInfo

    (* pos list ref *)
  let linePosList = ref []

  let resetLines () = linePosList := []
  let newLine (i) = linePosList := i::(!linePosList)
  let getLinesInfo () = !linePosList
  let posToLineCol (i) = posToLineCol' (!linePosList, i)

  (* join (r1, r2) = r
     where r is the  smallest region containing both r1 and r2
  *)
  let join (Reg (i1, j1), Reg (i2, j2)) = Reg (min i1 i2, max j1 j2)

  (* The right endpoint of the interval counts IN RANGE *)
  let posInRegion (k, Reg (i,j)) = i <= k && k <= j

  let lineColToString (line,col) =
      (string_of_int (line+1)) ^ "." ^ (string_of_int (col+1))

  (* toString r = "line1.col1-line2.col2", a format parsable by Emacs *)
  let toString (Reg (i,j)) =
        lineColToString (posToLineCol i) ^ "-"
	^ lineColToString (posToLineCol j)

  (* wrap (r, msg) = msg' which contains region *)
  let wrap (r, msg) = (toString r ^ " Error: \n" ^ msg)

  (* wrapLoc ((loc, r), msg) = msg' which contains region and filename
     This should be used for locations retrieved from origins, where
     the region is given in character positions, rather than lines and columns
  *)
  let wrapLoc0 (Loc (filename, Reg (i,j)), msg) =
         filename ^ ":" ^ (string_of_int (i+1)) ^ "-" ^ (string_of_int (j+1))
	 ^ " " ^ "Error: \n" ^ msg

  (* wrapLoc' ((loc, r), linesInfo, msg) = msg'
     like wrapLoc, but converts character positions to line.col format based
     on linesInfo, if possible
  *)
  let wrapLoc' tuple =
    match tuple with
        (Loc (filename, Reg (i,j)), Some(linesInfo), msg) ->
          let lcfrom = posToLineCol' (linesInfo, i) in
          let lcto = posToLineCol' (linesInfo, j) in
          let regString = lineColToString (lcfrom) ^ "-" ^ lineColToString (lcto) in
          filename ^ ":" ^ regString ^ " " ^ "Error: \n" ^ msg
      | (loc, None, msg) -> wrapLoc0 (loc, msg)

  let wrapLoc (loc, msg) =
        wrapLoc' (loc, Some (getLinesInfo()), msg)

  (* Paths, occurrences and occurrence trees only work well for normal forms *)
  (* In the general case, regions only approximate true source location *)

  (* Follow path through a term to obtain subterm *)

  type path =
     Label of path			(* [x:#] U or {x:#} V *)
   | Body of path			(* [x:V] # or {x:V} # *)
   | Head				(* # @ S, term in normal form *)
   | Arg of int * path			(* C @ S1; ...; #; ...; Sn; Nil *)
   | Here				(* #, covers Uni, EVar, Redex(?) *)

  (* Occurrences: paths in reverse order *)
  (* could also be: type occ = path -> path *)
  type occ =
      OccTop
    | OccLabel of occ
    | OccBody of occ
    | OccHead of occ
    | OccArg of int * occ

  (* Occurrence trees for expressions and spines *)
  (* Maps occurrences to regions *)
  (* Simple-minded implementation *)
  (* A region in an intermediate node encloses the whole expression *)
  type occExp =			(* occurrences in expressions *)
      Leaf of region			(* _ or identifier *)
    | Bind of region * occExp option * occExp (* [x:vOpt] u or {x:vOpt} v' *)
    | Root of region * occExp * int * int * occSpine (* h @ s, # of implicit arguments, # of arguments actually input (as opposed to generated by eta-expansion) *)
  and occSpine =			(* occurrences in spines *)
      App of occExp * occSpine		(* u;s *)
    | Nils				(* nil *)

  (* occToPath (occ, p) = p'(p) and occ corresponds to p' *)
  let rec occToPath tuple =
    match tuple with
        (OccTop, path) -> path
      | (OccLabel(occ), path) -> occToPath (occ, Label(path))
      | (OccBody(occ), path) -> occToPath (occ, Body(path))
      | (OccHead(occ), path) ->
      (* path = Here by invariant *)
        occToPath (occ, Head)
      | (OccArg(n,occ), path) -> occToPath (occ, Arg(n,path))

  type occConDec =			(* occurrence tree for constant declarations *)
      Dec of int * occExp               (* (#implicit, v) in c : V *)
    | Def of int * occExp * occExp option
					(* (#implicit, u, v) in c : V = U *)

  (* val posToPath : occExp -> pos -> path *)
  (* posToPath (u, k) = p
     where p is the path to the innermost expression in u enclosing position i.

     This includes the position immediately at the end of a region [i,j).
     For example, in "f (g x) y",
     0,1 => "f"
     2   => "(g x)"
     3,4 => "g"
     5,6 => "x"
     8,9 => "y"
  *)
  let posToPath u k =
    (* local functions refer to k but not u *)
    let inside args =
      match args with
          (Leaf r) -> posInRegion (k, r)
        | (Bind (r, _, _)) -> posInRegion (k, r)
        | (Root (r, _, _, _, _)) -> posInRegion (k, r)
    in
    let rec toPath args =
      match args with
          (Leaf (Reg (i,j))) -> Here (* check? mark? *)
        | (Bind (Reg (i,j), None, u)) ->
          if inside u then Body (toPath u)
          else Here
        | (Bind (Reg (i,j), Some(u1), u2)) ->
          if inside u1 then Label (toPath u1)
          else if inside u2 then Body (toPath u2)
               else Here
	| (Root (Reg (i,j), h, imp, actual, s)) ->
	  if inside h then Head
	  else (match (toPathSpine (s, 1)) with
	            None -> Here
    	          | Some(n, path) -> Arg (n+imp, path))
      (* in some situations, whitespace after subexpressions *)
      (* might give a larger term than anticipated *)
    and toPathSpine args =
      match args with
          (Nils, n) -> None
        | (App(u,s), n) ->
          if inside u then Some(n, toPath u)
          else toPathSpine (s, n+1)
    in
    toPath u

  (* toRegion (u) = r, the region associated with the whole occurrence tree u *)
  let toRegion args =
    match args with
        (Leaf r) -> r
      | (Bind (r, _, _)) -> r
      | (Root (r, _, _, _, _)) -> r

  (* toRegionSpine (s, r) = r', the join of all regions in s and r *)
  let rec toRegionSpine args =
    match args with
        (Nils, r) -> r
      | (App (u, s), r) ->
        join (toRegion u, toRegionSpine (s, r))	(* order? *)

  (* pathToRegion (u, p) = r,
     where r is the region identified by path p in occurrence tree u
  *)
  let rec pathToRegion args =
    match args with
        (u, Here) -> toRegion u
      | (Bind (r, None, u), Label(path)) ->
        (* addressing implicit type label returns region of binder and its scope *)
        r
      | (Bind (r, Some(u1), u2), Label(path)) ->
        pathToRegion (u1, path)
      | (Bind (r, _, u), Body(path)) ->
	pathToRegion (u, path)
      | (Root (r, _, _, _, _), Label(path)) ->
        (* addressing binder introduced as the result of eta expansion
           approximate as the eta-expanded root *)
        r
      | ((Root _) as u, Body(path)) ->
        (* bypassing binder introduced as the result of eta expansion *)
        pathToRegion (u, path)
      | (Root (r, h, imp, actual, s), Head) -> toRegion h
      | (Root (r, h, imp, actual, s), Arg (n, path)) ->
        if n <= imp
	then (* addressing implicit argument returns region of head *)
	     toRegion h
        else if n-imp > actual
             then (* addressing argument created by eta expansion
                     approximate by the whole root *)
                  r
             else pathToRegionSpine (s, n-imp, path)
      | (Leaf (r), _) -> r	(* possible if leaf was _ (underscore) *)
      (* other combinations should be impossible *)
  and pathToRegionSpine args =
    match args with
        (App (u, s), 1, path) ->
        pathToRegion (u, path)
      | (App (u, s), n, path) ->
	pathToRegionSpine (s, n-1, path)
    (* anything else should be impossible *)

  (* occToRegionExp u occ = r,
     where r is the closest region including occ in occurrence tree u
  *)
  let occToRegionExp u occ = pathToRegion (u, occToPath (occ, Here))

  let rec skipImplicit args =
    match args with
        (0, path) -> path
      | (n, Body(path)) ->
        skipImplicit (n-1, path)
      | (n, Label(path)) ->
	(* implicit argument: approximate as best possible *)
	Here
      | (n, Here) ->
	(* addressing body including implicit arguments: approximate by body *)
	Here
    (* anything else should be impossible *)

  (* occToRegionDec d occ = r
     where r is the closest region in v including occ for declaration c : V
  *)
  let occToRegionDec (Dec (n, v)) occ =
      pathToRegion (v, skipImplicit (n, occToPath (occ, Here)))

  (* occToRegionDef1 d occ = r
     where r is the closest region in u including occ for declaration c : V = U
  *)
  let occToRegionDef1 (Def (n, u, vOpt)) occ =
      pathToRegion (u, skipImplicit (n, occToPath (occ, Here)))

  (* occToRegionDef2 d occ = r
     where r is the closest region in V including occ for declaration c : V = U
  *)
  let occToRegionDef2 arg occ =
    match arg with
        (Def (n, u, Some(v))) ->
        pathToRegion (v, skipImplicit (n, occToPath (occ, Here)))
      | (Def (n, u, None)) ->
        pathToRegion (u, Here)

  (* occToRegionClause d occ = r
     where r is the closest region in V including occ for declaration
     c : V or c : V = U.
  *)
  let occToRegionClause d occ = 
    match d with
        (Dec _) -> occToRegionDec d occ
    |   (Def _) -> occToRegionDef2 d occ

end;;
