module test.
import lists.

%type  mapfun   (A -> B) -> list A -> list B -> o.

%mapfun F nil nil.
(mapfun F (X :: L) ((F X) :: K)) :- (mapfun F L K). 
