module overloadOp.

% unspecialized type: use default
foo (X + Y).


% specialized type.
foo (X + 3).
foo (3.5 - X).
foo ("hello" < X).