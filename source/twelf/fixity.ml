   (* Associativity ascribed to infix operators
       assoc ::= left    e.g. `<-'
               | right   e.g. `->'
               | none    e.g. `==' from some object language
    *)
    type associativity = Left | Right | None

    (* Operator Precedence *)
    type precedence = Strength of int

    (* Maximal and minimal precedence which can be declared explicitly *)
    let maxPrecInt = 9999
    let maxPrec = Strength(maxPrecInt)
    let minPrecInt = 0
    let minPrec = Strength(minPrecInt)

    let less (Strength(p), Strength(q)) = (p < q)
    let leq (Strength(p), Strength(q)) = (p <= q)
    let compare (Strength(p), Strength(q)) = compare p q

    let inc (Strength(p)) = Strength(p+1)
    let dec (Strength(p)) = Strength(p-1)

    (* Fixities ascribed to constants *)
    datatype fixity =
        Nonfix
      | Infix of precedence * associativity
      | Prefix of precedence
      | Postfix of precedence

    (* returns integer for precedence such that lower values correspond to higher precedence, useful for exports *)
    let precToIntAsc args =
      match args with
          (Infix(Strength n,_)) -> maxPrecInt + 1 - n
      | (Prefix(Strength n)) -> maxPrecInt + 1 - n
      | (Postfix(Strength n)) -> maxPrecInt + 1 - n
      | (Nonfix) -> minPrecInt

    (* prec (fix) = precedence of fix *)
    let prec f = 
      match f with
          (Infix(p,_)) -> p
      | (Prefix(p)) -> p
      | (Postfix(p)) -> p
      | (Nonfix) -> inc (maxPrec)

    (* toString (fix) = declaration corresponding to fix *)
    let toString f =
      match f with
          (Infix(Strength(p),Left)) -> "%infix left " ^ string_of_int p
      | (Infix(Strength(p),Right)) -> "%infix right " ^ string_of_int p
      | (Infix(Strength(p),None)) -> "%infix none " ^ string_of_int p
      | (Prefix(Strength(p))) -> "%prefix " ^ string_of_int p
      | (Postfix(Strength(p))) -> "%postfix " ^ string_of_int p
      | (Nonfix) -> "%nonfix"	(* not legal input *)
