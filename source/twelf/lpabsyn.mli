type pos = Errormsg.pos

type kinddecl = Kind of (id * int)

and typedecl = TypeDec of (id * typ * (clause ref) list ref)

and typ =
      ImpType of (typ * typ)
    | ConstType of (id * term list)
    | VarType of (id)

and term =
        AppTerm of (id * term list)
      | AbsTerm of (id * typ * term)
      | IdTerm of (id)

and id =
        Var of (string)
      | Const of (string)

and clause = Clause of (head * goal list)

and head = Head of (id * term list)

and goal =
        Atom of (term)
      | ImpGoal of (clause * goal)
      | Conjunction of (goal * goal)
      | Disjunction of (goal * goal)
      | Universal of (id * typ * goal)
      | Existential of (id * typ * goal)

val print_kinddecl : kinddecl -> string
val print_typedecl : typedecl -> string
val print_clause : clause -> string
val print_type : typ -> string
val print_term : term -> string
val print_id : id -> string
