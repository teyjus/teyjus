(* This is taken from the Twelf implementation *)

  type associativity = Left | Right | None
  type precedence = Strength of int

  val maxPrec : precedence
  val minPrec : precedence

  val less : precedence * precedence -> bool
  val leq : precedence * precedence -> bool
  val compare : precedence * precedence -> int

  val inc : precedence -> precedence
  val dec : precedence -> precedence

  type fixity =
      Nonfix
    | Infix of precedence * associativity
    | Prefix of precedence
    | Postfix of precedence

  val prec : fixity -> precedence
  val toString : fixity -> string

  (* returns integer for precedence such that lower values correspond to higher precedence, useful for exports *)
  val precToIntAsc : fixity -> int
