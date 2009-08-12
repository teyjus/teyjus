sig abbrevtest2.

% Correct type abbreviations and applications.
typeabbrev foo o -> o.
typeabbrev (bar) o -> o.
type app foo.

typeabbrev (appendtype A) list A -> list A -> list A -> o.
type append appendtype A.

% Incorrect applications
type append_error1 appendtype A A.  % Too many arguments.
type append_error2 appendtype.  % Too few arguments.
type foo_error foo A. % Too many arguments to foo.
type bar_error bar foo. % Too many arguments to bar.

type append_error3 appendtype A.  % See abbrevtest.mod

