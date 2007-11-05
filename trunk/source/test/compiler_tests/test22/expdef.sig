
sig expdef.

kind i type.


exportdef foo1  i -> o.


exportdef foo2  i -> o.


type      foo5  i -> o.
exportdef foo5  i -> o.


type      foo6  i -> o.
exportdef foo6.


% negative test cases: currently commented out

%type      foo3  i -> o.

%type      foo4  i -> o.

% expdef.mod:
%   type      foo7 i -> o.
%   exportdef foo7 i -> o.

%exportdef foo9 i.

