sig randomlams.

kind i type.

type app i -> i -> i.
type abs (i -> i) -> i.
type freevar i -> i.
type bvar i -> o.
type hnorm, fnorm i -> i -> o.
type hnormw, fnormw i -> i -> o.
%type hstack i -> i -> i -> o.

type randterm i -> o.
type genterm int -> (list i) -> i -> o.
type  length  (list A -> int -> o).
type  random  (int -> int -> o).
type  nthmemb  (int -> list A -> A -> o).
type  instream  (in_stream -> o).
type  outstream  (out_stream -> o).
type lambdaterm i -> o.
type go, go2 int -> o.

type combinator int -> string -> i -> o.
type randomcombo int -> string -> i -> o.
type combo string -> i -> o.
type makecombos int -> int -> string -> o.
type test o.

type single_test string -> i -> o.











