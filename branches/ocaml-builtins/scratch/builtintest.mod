module builtintest.

% This one works because redefining resets the nodefs flag.
type open_out string -> A -> o.
open_out "foo.txt" A.

% Error.
open_in "foo.txt" A :- open_in "bar.txt" A.
