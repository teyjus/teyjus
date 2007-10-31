% Please see comments in expdef.mod for the buggy cases!
sig useonly_test.

kind i type.

% case 1:
useonly foo1  i -> o.

% case 2:
useonly foo2  i -> o.

% case 3:
% *** buggy ***
type      foo3  i -> o.

% case 4:
% *** buggy *** 
type      foo4  i -> o.

% case 5:
% *** buggy ***
type      foo5  i -> o.
useonly foo5  i -> o.


% case 6:
% *** buggy ***
type      foo6  i -> o.
useonly   foo6.


% case 7: 
% expdef.mod:
%   type      foo7 i -> o.
%   useonly   foo7 i -> o.
% *** buggy *** 

% case 8: 
% expdef.mod:
%   type      foo8 i -> o.
%   useonly   foo8.
% *** buggy *** 

% case 9:
% *** buggy ***
useonly foo9 i.

