module tconst.

% constants with types as head and goal argument
foo tc1 :- foo tc1.
foo tc2 :- foo tc2.

% constants with types as application argument
foo (tc1 tc2) :- foo (tc1 tc2).

% constants with types as application head
foo (tc1 X) :- foo (tc1 X).