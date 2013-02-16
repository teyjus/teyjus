module acc.

/* kind renaming */
kind i type.                  % should map to top level global kind i.
kind j type -> type.          % should map to top level local kind j.
kind k type -> type -> type.  % should map to top level local kind k (no dec 
                              % in top level module)
kind l type -> type -> type -> type. % should not appear in top level 


/* constant renaming */
type a A.                     % should map to top level global constant a.
type b A -> A.                % should map to top level local constant b.
type c A -> A -> A.           % should map to top level local constant c (no
                              % dec in top level module)
type d A -> A -> A -> A.      % should not appear in top level