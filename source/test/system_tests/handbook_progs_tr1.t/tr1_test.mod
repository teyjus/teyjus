/*
 * A testing harness for the code in tr_recognizer that contains the 
 * definition of a predicate for recognizing tail recursiveness of 
 * two argument functions
 */

module tr1_test. 

accumulate terms, tr_recognizer. 

type test  int -> tm -> o.

test 1 F :- trm trfact1 F, tl_rec F.

test 2 F :- trm gcd2 F, tl_rec F.

test 3 F :- trm appnd F, tl_rec F.

