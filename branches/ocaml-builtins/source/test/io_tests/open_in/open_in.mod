module open_in.

test1 X :- open_in "open_in/foo" X.
test2 X :- open_in "open_in/bar" X. 
test3 X :- open_in "open_in/bar" X ; open_in "open_in/bar" X. 
test4 X :- open_in Unbound X. 
