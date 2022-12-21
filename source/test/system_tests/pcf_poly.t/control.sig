%
% Some special control predicates of general utility
%

sig control.

type announce    o -> o.                % for displaying goals
type spi         o -> o.                % display entry and exit from goal
type if          o -> o -> o -> o.      % if then else
type once        o -> o.                % once only predicate
