(** Translators for translating LF specifications into LP programs. *)

(** The interface for a translator. *)
module type Translator =
sig
  (** Translate the given LF signature into an LP signature. *)
  val translate : Lfsig.signature -> 
                    (Metadata.metadata * 
                      Absyn.akind Table.SymbolTable.t * 
                      Absyn.aconstant Table.SymbolTable.t * 
                      Absyn.aterm list)

  val translate_query : Lfabsyn.query -> Metadata.metadata ->
                          Absyn.akind Table.SymbolTable.t ->  
                          Absyn.aconstant Table.SymbolTable.t -> (Absyn.aterm * Absyn.atypesymbol list)
end

val get_translation : unit -> string

val set_translation : string -> unit

(** An implementation of the basic, naive translation from LF to 
    LP signatures. *)
module NaiveTranslation : Translator

(** An implementation of the translation from LF to LP signatures 
    which uses strictness to reduce the size of generated 
    clauses. Will run optimizations that have been set. *)
module OptimizedTranslation : Translator
