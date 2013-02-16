This example is taken from the POPLmark challenge. It involves a very
large term with many abstractions. When I try to compile I get the
following error,

  none(0,0) : Error : unable to perform register assignment

One interesting point is that that predicate "pred" is given two
terms called "filler". If one of these is removed (and the type of
pred is changed accordingly) then it does compile.
