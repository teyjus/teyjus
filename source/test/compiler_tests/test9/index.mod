module index.

%predicate with one clause definition
foo1 X :- foo1 X.

%predicate with two clause definitions
foo2 X :- foo2 X.
foo2 a :- foo2 a.

%predicate with more than two clauses 
%predicate with clauses with constant as first argument
foo3 X :- foo3 X.
foo3 a :- foo3 a.
foo3 Y :- foo3 a.
foo3 b :- foo3 X.


%predicate with clauses with list as first argument
foo5 (a :: Y) :- foo5 Y.
foo5 (X :: Y) :- foo5 Y.
foo5 nil.

%predicate with multiple clauses with same constant as first argument
foo6 a :- foo6 a.
foo6 X :- foo6 a.
foo6 b :- foo6 a.
foo6 a :- foo6 a.
foo6 b :- foo6 b.
foo6 a :- foo6 X.
