module abbrevtest2.

% Correct
append nil L L.
app F :- F.

% Error: append_error3 has wrong type for given clause.
append_error3 nil L 1.
