  exception Error of string
  exception Unprintable

  (* Constant names and fixities *)

  type qid = Qid of string list * string

  val qidToString : qid -> string
  val stringToQid : string -> qid option
  val unqualified : qid -> string option

  type namespace

  val newNamespace : unit -> namespace
  val insertConst : namespace * IntSyn.cid -> unit (* shadowing disallowed *)
  val insertStruct : namespace * IntSyn.mid -> unit (* shadowing disallowed *)
  val appConsts : (string * IntSyn.cid -> unit) -> namespace -> unit
  val appStructs : (string * IntSyn.mid -> unit) -> namespace -> unit

  val reset : unit -> unit
  val resetFrom : IntSyn.cid * IntSyn.mid -> unit

  (* The following functions have to do with the mapping from names
     to cids/mids only. *)
  val installConstName : IntSyn.cid -> unit
  val installStructName : IntSyn.mid -> unit

  val constLookup : qid -> IntSyn.cid option
  val structLookup : qid -> IntSyn.mid option
  val constUndef : qid -> qid option (* shortest undefined prefix of Qid *)
  val structUndef : qid -> qid option

  val constLookupIn : namespace * qid -> IntSyn.cid option
  val structLookupIn : namespace * qid -> IntSyn.mid option
  val constUndefIn : namespace * qid -> qid option
  val structUndefIn : namespace * qid -> qid option

  (* This function maps cids/mids to names.  It uses the information in
     the IntSyn.ConDec or IntSyn.StrDec entries only, and only considers
     the name->cid/mid mapping defined above in order to tell whether a
     name is shadowed (any constant or structure whose canonical name
     would map to something else, or to nothing at all, in the case of
     an anonymous structure, is shadowed). *)
  val conDecQid : IntSyn.conDec -> qid
  val constQid : IntSyn.cid -> qid (* will mark if shadowed *)
  val structQid : IntSyn.mid -> qid (* will mark if shadowed *)

  val installFixity : IntSyn.cid * Fixity.fixity -> unit
  val getFixity : IntSyn.cid -> Fixity.fixity
  val fixityLookup : qid -> Fixity.fixity (* Nonfix if undefined *)

  (* Name preferences for anonymous variables: a, EPref, UPref *)
  val installNamePref : IntSyn.cid * (string list * string list) -> unit
  val getNamePref : IntSyn.cid -> (string list * string list) option

  val installComponents : IntSyn.mid * namespace -> unit
  val getComponents : IntSyn.mid -> namespace

  (* EVar and BVar name choices *)
  val varReset : IntSyn.dctx -> unit (* context in which EVars are created *)
  val addEVar : IntSyn.exp * string -> unit (* assumes name not already used *)
  val getEVarOpt : string -> IntSyn.exp option (* NONE, if undefined or not EVar *)
  val evarName : IntSyn.dctx * IntSyn.exp -> string (* create, if undefined *)
  val bvarName : IntSyn.dctx * int -> string (* raises Unprintable if undefined *)

  val decName  : IntSyn.dctx * IntSyn.dec -> IntSyn.dec (* status unknown, like decEName *)
  val decEName : IntSyn.dctx * IntSyn.dec -> IntSyn.dec (* assign existential name *)
  val decUName : IntSyn.dctx * IntSyn.dec -> IntSyn.dec (* assign universal name *)
  val decLUName: IntSyn.dctx * IntSyn.dec -> IntSyn.dec (* assign local universal name *)

  val ctxName  : IntSyn.dctx -> IntSyn.dctx (* assign global existential names *)
  val ctxLUName: IntSyn.dctx -> IntSyn.dctx (* assign local universal names *)

  val nameConDec : IntSyn.conDec -> IntSyn.conDec

  (* Skolem constants *)
  val skonstName : string -> string

  (* Named EVars, used for queries *)
  val namedEVars : unit -> (IntSyn.exp * string) list
  (* Uninstantiated named EVars with constraints *)
  val evarCnstr : unit -> (IntSyn.exp * string) list
