module clauses.

%atomic clause
foo X Y.

%chain rule
foo X Y :- foo Y X.

%clauses with environments
foo X Y :- foo X Z, foo Z W, foo W R.