(** Optimizations available for use by the translation. *)

(** The interface for an translation optimization. *)
module type Optimization =
sig

  (** Return true if this optimization is to be run. *)
  val get : unit -> bool

  (** Set if this optimization is to be run. *)
  val set : bool -> unit

  (** Run the optimization over the given LP signature. *)
  val run_optimization : (Metadata.metadata * 
                            Absyn.akind Table.SymbolTable.t * 
                            Absyn.aconstant Table.SymbolTable.t * 
                            Absyn.aterm list) -> 
                                                 (Metadata.metadata * 
                                                    Absyn.akind Table.SymbolTable.t * 
                                                    Absyn.aconstant Table.SymbolTable.t * 
                                                    Absyn.aterm list)

  (** Run the optimization on the given query. *)
  val optimize : Absyn.aterm -> Absyn.aterm
end

(** Use specialized predicates for each LF type. Removes the hastype 
    predicate. *)
module Specialize : Optimization

(** Change the first argument of a predicate to be the final 
    argument. Used to swap the proof term from first to second 
    argument of the hastype predicate. *)
module Swap : Optimization
