/*
 * Interface to the implementation of some tacticals, i.e. methods for 
 * combining primitive tactics to produce derived rules 
 */

sig  tacticals. 

accum_sig  goaltypes.

type    maptac          (goal -> goal -> o) -> goal -> goal -> o.
type    then            (goal -> goal -> o)
                          -> (goal -> goal -> o) -> goal -> goal -> o.
type    orelse          (goal -> goal -> o)
                          -> (goal -> goal -> o) -> goal -> goal -> o.
type    idtac           goal -> goal -> o.
type    repeattac       (goal -> goal -> o) -> goal -> goal -> o.
type    try             (goal -> goal -> o) -> goal -> goal -> o.
type    complete        (goal -> goal -> o) -> goal -> goal -> o.
