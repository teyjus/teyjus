sig abbrevtest.

% Correct type abbreviations and applications.
typeabbrev foo o -> o.
typeabbrev (bar) o -> o.
type app foo.

typeabbrev (appendtype A) list A -> list A -> list A -> o.
type append appendtype A.

type append_error3 appendtype A.  % See abbrevtest.mod

