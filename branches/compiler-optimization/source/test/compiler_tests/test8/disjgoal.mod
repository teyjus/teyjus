module disjgoal.

foo X Y :- foo X Z; foo Y Z.

foo X Y :- foo Z W, (foo X Z; foo Y W).

foo X Y :- foo Z W, (foo X U; foo Y V), foo U V.


