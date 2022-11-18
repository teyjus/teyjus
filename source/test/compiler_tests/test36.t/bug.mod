module bug.

% Each of the following three lines (taken one at a time) give 
% the following compile-time error: 
%% 
%% $ tjcc bug
%% none(0,0) : Internal Error : Absyn.getTermConstant: invalid term: B
%% Fatal error: exception Errormsg.InternalError
%% $ 

nabla B :- pi B.
nabla (x\ B x) :- pi B.
nabla (x\ B x) :- pi x\ B x.

% The following one does not produce this error:

nabla (x\ B x) :- pi x\ true, B x.   

