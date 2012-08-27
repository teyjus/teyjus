module augmentedgoal.

foo X Y :- (foo Z Y => foo X Z).
foo X Y :- ((foo Z Y, foo Z Y :- foo Z Y) => foo X Z).

foo X Y :- (foo X Z :- foo Z Y) => foo Z Y.

foo X Y :- pi Z\ (foo Z Y => foo X Z).
foo X Y :- pi Z\ ((foo Z Y, foo Z Y :- foo Z Y) => foo X Z).

foo X Y :- pi bar\ ((bar X Y, bar X Z :- foo X Z) => bar X Y).


foo X Y :- (foo U V, (foo U W => foo X Y)).