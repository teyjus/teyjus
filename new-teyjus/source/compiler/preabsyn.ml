(**********************************************************************
*Preabsyn Module:
* The prebstract syntax for Teyjus.  This is more or less a direct
* translation from the original C source.  I am sure there are numerous
* places where it could be made better.
**********************************************************************)
type symbol = Symbol.symbol
type pos = Errormsg.pos

(*  Kinds of Identifiers  *)
type pidkind =
    CVID
  | ConstID
  | AVID
  | VarID

type pfixitykind =
    Infix of pos
  | Infixl of pos
  | Infixr of pos
  | Prefix of pos
  | Prefixr of pos
  | Postfix of pos
  | Postfixl of pos

    
(*  Symbols *)
type psymbol = Symbol of (symbol * pidkind * pos)

(*  Type Symbols  *)
and ptypesymbol = TypeSymbol of (symbol * ptype option * pidkind * pos)

(*  Types *)
and ptype =
    Atom of (symbol * pidkind * pos)
  | App of (ptype * ptype * pos)
  | Arrow of (ptype * ptype * pos)
  | ErrorType

and ptypeabbrev = TypeAbbrev of (psymbol * psymbol list * ptype * pos)

and pboundterm = BoundTerm of (ptypesymbol list * pterm list)

(*  Terms *)
and pterm =
    SeqTerm of (pterm list * pos)
  | ListTerm of (pterm list * pos)
  | ConsTerm of (pterm list * pterm * pos)
  | LambdaTerm of (ptypesymbol list * pterm list * pos)
  | IdTerm of (symbol * ptype option * pidkind * pos)
  | RealTerm of (float * pos)
  | IntTerm of (int * pos)
  | StringTerm of (string * pos)
  | ErrorTerm
  
and pclause = Clause of (pterm)

(*  Constants *)
and pconstant = Constant of (psymbol list * ptype option * pos)

(*  Kinds *)
and pkind = Kind of (psymbol list * int option * pos)

(*  Fixity  *)
and pfixity = Fixity of (psymbol list * pfixitykind * int * pos)

(********************************************************************
*Module:
* This type stores information about a preabsyn module.
* See interface for details.
********************************************************************)
type pmodule =
    Module of (string * pconstant list * pconstant list * 
      pconstant list * pconstant list * pfixity list * pkind list *
      pkind list * ptypeabbrev list * pclause list * psymbol list *
      psymbol list * psymbol list)
  | Signature of (string * pconstant list * pkind list *
      ptypeabbrev list * pfixity list * psymbol list)

(********************************************************************
*string_of_termlist:
********************************************************************)
let rec string_of_termlist = function
    [] -> ""
  | t::[] -> string_of_term t
  | t::ts -> (string_of_term t) ^ ", " ^ (string_of_termlist ts)

and string_of_typesymbollist = function
    [] -> ""
  | t::[] -> string_of_typesymbol t
  | t::ts -> (string_of_typesymbol t) ^ ", " ^ (string_of_typesymbollist ts)

and string_of_typesymbol = function
    TypeSymbol(tsym, Some t, idk, pos) ->
      ("TypeSymbol(" ^ (Symbol.name tsym) ^ ", " ^
      (string_of_type t) ^ ", " ^ (string_of_idkind idk) ^ ", " ^ (Errormsg.string_of_pos pos))
  | TypeSymbol(tsym, None, idk, pos) ->
      ("TypeSymbol(" ^ (Symbol.name tsym) ^ ", " ^
      (string_of_idkind idk) ^ ", " ^ (Errormsg.string_of_pos pos))

(********************************************************************
*string_of_term:
********************************************************************)
and string_of_term = fun term ->
  match term with
    SeqTerm(tlist, pos) ->
      ("SeqTerm([" ^ (string_of_termlist tlist) ^ "], " ^ (Errormsg.string_of_pos pos) ^ ")")
  | ListTerm(tlist, pos) ->
      ("ListTerm([" ^ (string_of_termlist tlist) ^ "], " ^ (Errormsg.string_of_pos pos) ^ ")")
  | ConsTerm(tlist, t, pos) ->
      ("ConsTerm([" ^ (string_of_termlist tlist) ^ "], " ^ (string_of_term t) ^ ", " ^ (Errormsg.string_of_pos pos) ^ ")")
  | IdTerm(sym, None, k, pos) ->
      ("IdTerm(" ^ (Symbol.name sym) ^ ", " ^ (string_of_idkind k) ^ ", " ^
        (Errormsg.string_of_pos pos) ^ ")")
  | IdTerm(sym, Some t, k, pos) ->
      ("IdTerm(" ^ (Symbol.name sym) ^ ", " ^ (string_of_type t) ^ ", " ^
        (string_of_idkind k) ^ ", " ^ (Errormsg.string_of_pos pos) ^ ")")
  | RealTerm(r, pos) ->
      ("RealTerm(" ^ (string_of_float r) ^ ", " ^ (Errormsg.string_of_pos pos) ^ ")")
  | IntTerm(i, pos) ->
      ("IntTerm(" ^ (string_of_int i) ^ ", " ^ (Errormsg.string_of_pos pos) ^ ")")
  | StringTerm(s, pos) ->
      ("StringTerm(" ^ s ^ ", " ^ (Errormsg.string_of_pos pos) ^ ")")
  | LambdaTerm(lt, t, pos) ->
      ("LambdaTerm(" ^ (string_of_typesymbollist lt) ^ ", " ^ (string_of_termlist t) ^ ", " ^ (Errormsg.string_of_pos pos) ^ ")")
  | ErrorTerm ->
      ("Error")     

(********************************************************************
*string_of_boundterm:
********************************************************************)
and string_of_boundterm = function BoundTerm(tysy, tl) ->
  ("BoundTerm([" ^ (string_of_typesymbollist tysy) ^
  "], [" ^ (string_of_termlist tl) ^ "])")

(********************************************************************
*string_of_idkind:
********************************************************************)
and string_of_idkind = function
    CVID -> "CVID"
  | ConstID -> "ConstID"
  | AVID -> "AVID"
  | VarID -> "VarID"

and string_of_type = function
    Atom(sym, k, pos) ->
      ("Atom(" ^ (Symbol.name sym) ^ ", " ^ (string_of_idkind k) ^ ", " ^ (Errormsg.string_of_pos pos) ^ ")")
  | App(t1, t2, pos) ->
      ("App(" ^ (string_of_type t1) ^ ", " ^ (string_of_type t2) ^ ", " ^ (Errormsg.string_of_pos pos) ^ ")")
  | Arrow(t1, t2, pos) ->
      ("Arrow(" ^ (string_of_type t1) ^ ", " ^ (string_of_type t2) ^ ", " ^ (Errormsg.string_of_pos pos) ^ ")")
  | ErrorType ->
      ("Error")

(********************************************************************
*printPreAbsyn:
* Prints all information about a preabsyn module.
********************************************************************)
let printPreAbsyn = fun m out ->
  (*  Text output functions *)
  let output = function s -> (output_string out s) in
  let output_line = function s -> (output_string out (s ^ "\n")) in
  
  let rec printWithCommas : ('a -> unit) -> 'a list -> unit = 
    fun f ls ->
      match ls with
        (l::[]) -> (f l)
      | (l::ls) ->
          ((f l);
          output ", ";
          printWithCommas f ls)
      | ([]) -> ()

  (*  Print constant to output channel  *)
  and printConstant = function
      Constant(symlist, Some t, pos) ->
        (output "Constant(";
        printWithCommas printSymbol symlist;
        output ", ";
        output (string_of_type t);
        output ", ";
        printPos pos;
        output_line ")";)
    | Constant(symlist, None, pos) ->
        (ignore (List.map printSymbol symlist);
        printPos pos)

  and printSymbol = function
      Symbol(sym, k, pos) ->
        (output "Symbol(";
        output (Symbol.name sym);
        output ", ";
        output (string_of_idkind k);
        output ", ";
        printPos pos;
        output ")")

    and printPos = function
      p ->
        (output "Pos(";
        output (Errormsg.string_of_pos p);
        output ")")
    
    and printTypeAbbrev = function
      TypeAbbrev(name, arglist, ty, pos) ->
        (output "TypeAbbrev(";
        printSymbol name;
        printWithCommas printSymbol arglist;
        output ", ";
        output (string_of_type ty);
        output ", ";
        printPos pos;
        output_line ")";)

    and printFixity = function
        Fixity(names, k, prec, pos) ->
          (output "Fixity(";
          printWithCommas printSymbol names;
          output ", ";
          printFixityKind k;
          output ", ";
          output (string_of_int prec);
          output ", ";
          printPos pos;
          output_line ")")
          
    and printFixityKind = function
        Infix(pos) ->
          (output "Infix(";
          printPos pos;
          output ")")
      | Infixl(pos) ->
          (output "Infixl(";
          printPos pos;
          output ")")
      | Infixr(pos) ->
          (output "Infixr(";
          printPos pos;
          output ")")
      | Prefix(pos) ->
            (output "Prefix(";
            printPos pos;
            output ")")
      | Prefixr(pos) ->
          (output "Prefixr(";
          printPos pos;
          output ")")
      | Postfix(pos) ->
          (output "Postfix(";
          printPos pos;
          output ")")
      | Postfixl(pos) ->
          (output "Postfixl(";
          printPos pos;
          output ")")

    and printKind = function
        Kind(symlist, Some i, pos) ->
          (output "Kind(";
          printWithCommas printSymbol symlist;
          output ",";
          output (string_of_int i);
          output_line ")")
      | Kind(symlist, None, pos) ->
          (output "Kind(";
          printWithCommas printSymbol symlist;
          output_line ")")

    (*  Print a Preabstract Syntax Term *)
    and printTerm = fun t -> output_line (string_of_term t)
    
    and printClause = function
        Clause(ts) ->
          (output "Clause(";
          printTerm ts;
          output_line ")")

    and printPreAbsyn' = function
        Module(name, gconstants, lconstants, cconstants, uconstants, fixities, gkinds, lkinds, tabbrevs, clauses, accummod, accumsig, usesig) ->
          (output_line ("Module:" ^ name);
          output_line "Constants:";
          List.iter printConstant gconstants;
          List.iter printConstant lconstants;
          List.iter printConstant cconstants;
          output_line "";
          output_line "Kinds:";
          List.iter printKind gkinds;
          output_line "";
          List.iter printKind lkinds;
          output_line "Type Abbrevs:";
          List.iter printTypeAbbrev tabbrevs;
          output_line "Clauses:";
          List.iter printClause clauses;
          output_line "Fixities:";
          List.iter printFixity fixities;
          ())
      | Signature(name, gconstants, gkinds, tabbrevs, fixities, accumsig) ->
          (output_line ("Signature: " ^ name);
          output_line "Constants:";
          List.iter printConstant gconstants;
          output_line "Kinds:";
          List.iter printKind gkinds;
          output_line "Type Abbrevs:";
          List.iter printTypeAbbrev tabbrevs;
          ())
  in
    printPreAbsyn' m

let getFixityPos = function
    Infix(i) -> i
  | Infixl(i) -> i
  | Infixr(i) -> i
  | Prefix(i) -> i
  | Prefixr(i) -> i
  | Postfix(i) -> i
  | Postfixl(i) -> i

let getTermPos = function
    SeqTerm(_, pos) -> pos
  | ListTerm(_, pos) -> pos
  | ConsTerm(_, _, pos) -> pos
  | IdTerm(_, _, _, pos) -> pos
  | RealTerm(_, pos) -> pos
  | IntTerm(_, pos) -> pos
  | StringTerm(_, pos) -> pos
  | LambdaTerm(_,_,pos) -> pos
  | ErrorTerm -> (Errormsg.impossible Errormsg.none "Preabsyn.getTermPos: invalid term")

let getModuleClauses = function
    Module(name, gconsts, lconsts, cconsts, uconsts, fixities,
      gkinds, lkinds, tabbrevs, clauses, accummods,
      accumsigs, usesigs) -> clauses
  | _ -> Errormsg.impossible Errormsg.none "Preabsyn.getModuleClauses: invalid module"
