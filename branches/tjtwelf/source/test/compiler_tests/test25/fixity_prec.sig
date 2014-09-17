sig fixity_prec.

type   foo    int -> o.

% Infixl:
type   plus   int -> int -> int.
infixl plus   6.

type   minus  int -> int -> int.
infixl minus  6.

type   mul    int -> int -> int.
infixl mul    7.

type   divide int -> int -> int.
infixl divide 7.

% Infix
type ** int -> int -> int.
infixl ** 9.

% Postfix:
type inc_non int -> int.
postfix inc_non 10.

type inc int -> int.
postfixl inc 10.

% Prefix
type preinc int -> int.
prefixr preinc 10.
