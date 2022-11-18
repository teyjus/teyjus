module foo.

%nth 1 X [X|T].
nth N X [H|T] :- not(X = H), N2 is (N - 1), nth N2 X T.