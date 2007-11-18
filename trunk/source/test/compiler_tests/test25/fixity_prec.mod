module fixity_prec.

% Infixl
foo (3 plus 2 mul 5 divide 6 minus 7).

% Infix
foo (4 ** 3 ** 2).

% Postfix
foo (3 inc).
foo (3 inc inc).
foo (3 inc_non).

% Prefix
foo (preinc 4).
foo (preinc preinc 4).