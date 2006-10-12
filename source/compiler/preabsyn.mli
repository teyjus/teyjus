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
  | LambdaTerm of (pboundterm * pterm * pos)
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
* Module:
*   Name: string
*   Global Constants: pconstant list
*   Local Constants: pconstant list
*   Closed Constants: pconstant list
*   Fixities: pfixity list
*
*   Global Kinds: pkind list
*   Local Kinds: pkind list
*
*   Type Abbreviations: ptypeabbrev list
*
*   Clauses: pterm list
*
*   Accumulated Modules: psymbol list
*   Accumulated Signatures: psymbol list
*   Used Signatures: psymbol list
* Signature:
*   Name: string
*   Global Constants: pconstant list
*   Global Kinds: pkind list
*
*   Type Abbreviations: ptypeabbrevlist
*
*   Accumulated Signatures: psymbol list
********************************************************************)
type pmodule =
    Module of (string * pconstant list * pconstant list * 
      pconstant list * pfixity list * pkind list * pkind list * ptypeabbrev list *
      pclause list * psymbol list * psymbol list * psymbol list)
  | Signature of (string * pconstant list * pkind list *
      ptypeabbrev list * pfixity list * psymbol list)

val printPreAbsyn : pmodule -> out_channel -> unit

(*  Accessors *)
val getFixityPos : pfixitykind -> pos
val getTermPos : pterm -> pos
val getModuleClauses : pmodule -> pclause list
val string_of_term : pterm -> string