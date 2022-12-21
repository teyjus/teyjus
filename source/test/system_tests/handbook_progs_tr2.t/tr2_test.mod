/*
 * A testing harness for the general tail recursion recognizing program
 */

module  tr2_test.

accumulate  terms, general_tr. 

type  test  int -> tm -> o.

test 1 F :- trm trfact1 F, tailrec F. 

test 2 F :- trm gcd2 F, tailrec F.

test 3 F :- trm appnd F, tailrec F.

test 4 F :- trm trfact2 F, tailrec F.

