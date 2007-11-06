module useonly_test_fail.

% negative cases:
%    useonly_test.sig: 
%      type      foo3  i -> o.   
useonly foo3 i -> o.
foo3 X.

%    useonly_test.sig
%      type      foo4 i -> o.
useonly foo4.
foo4 X.

type foo7 i -> o.
useonly foo7 i -> o.
foo7 X.
 