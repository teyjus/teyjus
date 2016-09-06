(** Sets up the query state for solving LF queries.
    To solve LF queries we will need:
      - LF signature
      - LP constant symbol table
      - mapping between LF and LP constants
      - Teyjus simulator
    We set up this state in two steps:
      1. Set up the simulator state and load the translated
         module using the Teyjus loader
      2. load the mapping between LF and LP constants
*)

val load : string -> (Absyn.amodule * Metadata.metadata)
