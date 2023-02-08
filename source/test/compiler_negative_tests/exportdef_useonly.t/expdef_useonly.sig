sig expdef_useonly.

kind i type.

% Tests useonly sig, exportdef mod.
type foo1 i -> o.
useonly foo1.

% Tests useonly mod, exportdef sig.
type foo2 i -> o.
exportdef foo2.

