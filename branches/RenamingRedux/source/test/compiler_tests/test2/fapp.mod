module fapp.

% flexible application as head and goal argument

foo  (F a b) :- foo (G a), foo (G b).

% flexible application as application (structure) argument
foo  (appl (F a b)) :- foo (appl (G a)), foo (appl (G b)).
