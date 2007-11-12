module useonly_test_fail.
kind i type.
% negative cases:

%    useonly_test.sig: useonly foo1  i -> o.
foo1 X. 


%    useonly_test.sig:
%      useonly foo2  i -> o.
type foo2 i -> o.
foo2 X.


%    useonly_test.sig: 
%      type      foo3  i -> o.   
useonly foo3 i -> o.
foo3 X.

%    useonly_test.sig
%      type      foo4 i -> o.
useonly foo4.
foo4 X.

foo5 X.
foo6 X.