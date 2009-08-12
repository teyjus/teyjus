sig useonly_test_fail.

kind i type.

% negative cases:
useonly foo1  i -> o.

useonly foo2  i -> o.

type      foo3  i -> o.
type      foo4  i -> o.

% case 9:
useonly foo9 i.


type      foo5  i -> o.
useonly   foo5  i -> o.


type      foo6  i -> o.
useonly   foo6.