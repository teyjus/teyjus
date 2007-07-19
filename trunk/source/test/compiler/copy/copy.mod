module copy.

copy a a.
copy (app M N) (app M' N') :- copy M M', copy N N'.
copy (lam P) (lam Q) :- pi (x\ (copy x x => copy (P x) (Q x))).
copy (lam P) (lam Q) :-  pi (x\ (copy (P x) (Q x))).
