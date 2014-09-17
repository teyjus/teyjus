sig typecat.
accum_sig bobparser.

% type checking
% and limited type inference

type tarr (list ttype) -> ttype -> ttype.  % arrow type
type typeexp texp -> ttype -> o.

