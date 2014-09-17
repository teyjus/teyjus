sig  assoc.

kind pair   type -> type -> type.

type pair   A -> B -> (pair A B).

type assoc  A -> B -> (list (pair A B)) -> o.

%% for forming testing query 
kind i      type.
type foo    i.
type bar    i.