module close_in.

% Opens and closes a file which exists
test1 X :- open_in "open_in/bar" X , close_in X.
% Try to close a file which is already close 
test2 X :- open_in "open_in/bar" X , close_in X, close_in X.
% Try to close an unbound variable
test3 :- close_in Unbound. 
