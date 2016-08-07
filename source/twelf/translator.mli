(** Translators for translating LF specifications into LP programs. *)

(** The interface for a translator. *)
module type Translator =
sig
  (** Translate the given LF signature into an LP signature. *)
  val translate : Lfsig.signature -> Absyn.amodule
end

(** An implementation of the basic, naive translation from LF to 
    LP signatures. *)
module NaiveTranslation : Translator

(** An implementation of the translation from LF to LP signatures 
    which uses strictness to reduce the size of generated 
    clauses. Will run optimizations that have been set. *)
module OptimizedTranslation : Translator
