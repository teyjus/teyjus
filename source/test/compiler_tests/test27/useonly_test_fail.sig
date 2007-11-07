sig useonly_test_fail.
% negative cases:
kind i type.
type      foo3  i -> o.
type      foo4  i -> o.

% case 9:
useonly foo9 i.


type      foo5  i -> o.
useonly   foo5  i -> o.


type      foo6  i -> o.
useonly   foo6.