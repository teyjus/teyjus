(** Frontend for the tjtwelf tool *)

(** main: currently does nothing but print out a message and exit. *)
let _ = 
  let _ = print_string "tjtwelf started!\n" in
  let sign = Test.sign in
  let (metadata, kinds, constants, terms) = Translator.NaiveTranslation.translate sign in
  let query = Test.q2 in
  let t = Lfquery.submit_query query metadata kinds constants in
  let s = if t
          then Lfquery.solve_query ()
          else exit 1 in
  let _ = if s
          then Lfquery.show_answers ()
          else exit 1 in
  print_string "done!\n";
  exit 1
