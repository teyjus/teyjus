sig fixity_prec.

type   foo    int -> o.

% Infix
type ** int -> int -> int.
infix ** 9.

% Postfix:
type inc_non int -> int.
postfix inc_non 10.

type inc int -> int.
postfixl inc 10.

% Prefix
type inc_non int -> int.
postfix inc_non 10.

type preinc int -> int.
prefixr preinc 10.
