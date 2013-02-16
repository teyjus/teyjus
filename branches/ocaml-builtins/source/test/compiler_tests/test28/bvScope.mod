module bvScope.

% lam lam #1
foo (abs x\ (abs x\ x)).

% lam (#1 (lam #1)).
foo (abs x\ (app x (abs x\ x))).

