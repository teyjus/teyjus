% This encoding will not do much interesting since the real work is 
% in the flex-flex part of unification, which is not implemented in 
% lambda Prolog.

module post.

%  This is the Post problem {(uv,u), (u,vu)}.
two_corresp F :-
  pi u : i -> i\ pi v : i -> i\
   ((F  (x\ u (v x))    u           ) =
   (F   u              (x\ v (u x))),
   (F u u) = w\ u (G u w)).
% printterm std_out (F u u), print "\n", printterm std_out (w\ u (G u w)).

%  This is the Post problem {(uv,vu), (v,vu), (u,e)}.
three_corresp F :- pi u\ pi v\ 
   (F   (x\ u (v x))    v              u  ) =
   (F   (x\ v (u x))    (x\ v (u x))   x\x),
   (F u u u) = w\ u (G u w).
