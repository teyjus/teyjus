/*
 * Interface to an implementation of an interactive theorem prover.
 */

sig  inter.

accum_sig  logic, nonlogical, formulas, ndproofs, ndtac, tacticals.

type	inter_top	name -> proof_object -> goal -> o.
type	inter		goal -> goal -> o.
type	do		o -> goal -> goal -> o.
type	quitg		goal -> goal -> o.
type	backup		goal -> goal -> o.
type	print_form_list	list judgment -> int -> o.
type    nl              o.
type    write           A -> o.
