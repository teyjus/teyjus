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

let rec print_kinddecl (Kind(id,i)) =
  let rec kind x =
    match x with
        0 -> "type"
      | _ -> "type -> " ^ (kind (i-1))
  in
  "kind " ^ (print_id id) ^ " " ^ (kind i) ^ "."

and print_typedecl (TypeDec(id,t,_)) =
  "type " ^ (print_id id) ^ " " ^ (print_type t) ^ "."

and print_type t =
  match t with
      ImpType(t1,t2) ->
        "(" ^ (print_type t1) ^ ") -> " ^ (print_type t2) ^ ""
    | ConstType(p,tms) ->
        (match tms with
             [] -> print_id p
           | _ ->
             let tmlist = 
               List.fold_left (fun str tm -> str ^ " " ^ (print_term tm)) "" tms
             in
             "(" ^ (print_id p) ^ tmlist ^ ")")
    | VarType(id) -> 
        print_id id

and print_clause (Clause(head, goals)) =
  match goals with
      [] -> (print_head head) ^ "."
    | (g :: gs) -> 
      let goallist = 
        List.fold_left (fun x y -> x ^ ", " ^ (print_goal y)) (print_goal g) gs
      in
      (print_head head) ^ " :- " ^ goallist

and print_head (Head(p, tms)) =
  let tmlist =
    List.fold_left (fun s tm -> s ^ " " ^ (print_term tm)) "" tms
  in
  (print_id p) ^ " " ^ tmlist

and print_goal g =
  match g with
      Atom(tm) -> print_term tm
    | ImpGoal(c, g) -> 
        "(" ^ (print_inner_clause c) ^ ") => " ^ (print_goal g)
    | Conjunction(g1,g2) -> 
        "(" ^ (print_goal g1) ^ ", " ^ (print_goal g2) ^ ")"
    | Disjunction(g1,g2) -> 
        "(" ^ (print_goal g1) ^ "; " ^ (print_goal g2) ^ ")"
    | Universal(id,ty,body) -> 
        "(pi " ^ (print_id id) ^ " : " ^ (print_type ty) ^ "\ " ^ (print_goal body) ^ ")"
    | Existential(id,ty,body) ->
        "(sigma " ^ (print_id id) ^ " : " ^ (print_type ty) ^ "\ " ^ (print_goal body) ^ ")"

and print_inner_clause (Clause(head, goals)) =
  match goals with
      [] -> print_head head
    | (g :: gs) ->
      let goallist = 
        List.fold_left (fun x y -> x ^ " => " ^ (print_goal y)) (print_goal g) gs
      in
      "(" ^ goallist ^ " => " ^ (print_head head) ^ ")"

and print_term t =
  match t with
      AppTerm(h,tms) ->
        let tmlist =
          List.fold_left (fun s t -> s ^ " " ^ (print_term t)) "" tms
        in
        "(" ^  (print_id h) ^ " " ^ tmlist ^ ")"
    | AbsTerm(id,ty,body) ->
        "(" ^ (print_id id) ^ " : " ^ (print_type ty) ^ "\ " ^ (print_term body) ^ ")"
    | IdTerm(id) -> print_id id

and print_id id =
  match id with
      Var(n)
    | Const(n) -> n
