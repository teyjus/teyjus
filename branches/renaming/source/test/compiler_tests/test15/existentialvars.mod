module existentialvars.

foo X :- sigma Y\ ((bar Y :- foo X) => bar X).
foo X :- sigma X\ ((bar X :- foo X) => bar X).
foo X :- sigma Z\ ((bar X :- bar Y) => bar X).
