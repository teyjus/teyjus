sig useonly_test.

kind i type.


useonly foo1  i -> o.

useonly foo2  i -> o.


type      foo5  i -> o.
useonly   foo5  i -> o.


type      foo6  i -> o.
useonly   foo6.

% negative cases:

%type      foo3  i -> o.

%type      foo4  i -> o.

% case 7: 
% useonly_test.mod:
%   type      foo7 i -> o.
%   useonly   foo7 i -> o.


% case 9:
%useonly foo9 i.

