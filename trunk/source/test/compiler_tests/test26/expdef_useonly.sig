sig expdef_useonly.

kind i type.

% Tests useonly sig, exportdef mod.
type foo1 i -> o.
useonly foo1.

% Tests useonly mod, exportdef sig.
type foo2 i -> o.
expdef foo2.

% Tests no clauses for useonly.
type foo3 i -> o.
useonly foo3.

% Tests no clauses for exportdef.
type foo4 i -> o.
useonly foo4.
