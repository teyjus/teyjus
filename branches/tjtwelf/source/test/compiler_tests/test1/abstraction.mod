module abstraction.


% abstractions as head and goal argument
foo1 (x\ x) :- foo1 (x\ x).

% abstractions as application argument
foo2 (abst (x\ x)) :- foo2 (abst (x\ x)).


% nested abstractions as head and goal argument
foo3 (x\ y\ (appl x y)) :- foo3 (x\ y\ (appl x y)).

% nested abstractions as application argument
foo2 (nabst (x\ y\ (appl x y))) :- foo2 (nabst (x\ y\ (appl x y))).
