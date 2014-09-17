sig church.

kind  i  type.

%  Notice that there are no constructors for objects in type i.  This
%  allows you to conclude that the only closed terms of 
%                          (i -> i) -> (i -> i) 
%  are the Church numerals.

type  zero, one, church    (A -> B) -> o. %((i -> i) -> (i -> i)) -> o.

type  plus, mult (A -> B) -> o.
%       (((i -> i) -> i -> i) -> ((i -> i) -> i -> i) ->
%        ((i -> i) -> i -> i)) -> o.
type  succ  (A -> B) -> o. %(((i -> i) -> i -> i) -> ((i -> i) -> i -> i)) -> o.

type  problem1   A -> B -> C -> D -> o.
%((i -> i) -> i -> i) ->  ((i -> i) -> i -> i) -> 
%((i -> i) -> i -> i) ->  ((i -> i) -> i -> i) -> o.


% added by liang.
type cn int -> (A -> B) -> o. %(((i -> i) -> (i -> i)) -> o.
type test int -> o.
type go o.



