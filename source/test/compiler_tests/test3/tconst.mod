module tconst.

% constants with types as head and goal argument
foo tc1 :- foo tc1.
% note tc2 should not have type association (type perserving)
foo tc2 :- foo tc2. 

% constants with types as application head
foo (tc1 tc2) :- foo (tc1 tc2).

% constants with types as application argument
foo (tc1 (tc1 X)) :- foo (tc1 (tc1 X)).