/*
 *  A testing harness for the code that infers polytypes for programs
 *  in the simple functional programming language.
 */

module poly_test.

accumulate polyinfer {type polyinfer => pinf,*}.
accumulate examples.

type poly_test  string -> poly -> o.

poly_test String Ty :- prog String T, pinf T Ty.
